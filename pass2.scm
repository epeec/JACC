(define-module pass2
  (use util)
  (use xm)

  (use srfi-1)
  (use srfi-11)
  (use srfi-13)
  (use file.util)
  (use util.match)
  (use sxml.tools)
  (use sxml.sxpath)
  (use util.combinations)
  (use gauche.process)
  (use gauche.sequence)
  (use sxml.serializer)

  (export
   pass2
   ))
(select-module pass2)

;;;
;;; PASS2
;;;
;;; Replace OpenACC statements by JACC routines
;;;

(define (jit-acc! xm state)
  (match ((ccc-sxpath "string") state)
    ["DATA"
     (jit-acc-data! xm state)]

    ["UPDATE"
     (jit-acc-update! xm state)]

    ["ENTER_DATA"
     (jit-acc-enter! xm state #t)]

    ["EXIT_DATA"
     (jit-acc-enter! xm state #f)]

    ["PARALLEL"
     (jit-acc-parallel! xm state)]

    [else #f]
    ))

(define NEW_TYPE_COUNT 0)

(define (new-pointer-type)
  #"jacc__P~(inc! NEW_TYPE_COUNT)")

(define (add-pointer-type! xm ref)
  (rlet1 ptype (new-pointer-type)
    (sxml:content-push!
     (xm-type-table xm)
     `(pointerType (@ (type ,ptype) (ref ,ref))))))

(define (add-jacc-init! xm)
  (let1 main-body
      ((if-car-sxpath
        '(// (functionDefinition (name (equal? "main"))) body))
       (xm-global-declarations xm))

    (when main-body
      (sxml:change-content!
       main-body
       (list
        (apply gen-compound
         `(,(gen-funcall "__jacc_init")
           ,@(cdr main-body)
           ,(gen-funcall "__jacc_close"))
         )))
      )))

(define (generate-data-state label arg enter)
  (and-let*
      ([(not (eq? (sxml:name arg) 'Var))]

       [fun-name
        (if enter
            (match label
              [(or "COPY" "COPYIN")
               "__jacc_copyin"]

              [(or "COPYOUT" "CREATE")
               "__jacc_create"]

              [else #f])

            (match label
              [(or "COPY" "COPYOUT")
               "__jacc_copyout"]

              [(or "COPYIN" "CREATE")
               "__jacc_delete"]

              [else #f]))]

       [var   (~ (sxml:content arg) 0)]
       [size  (cdr (sxml:content arg))]
       [start (~ size 0 1)]
       [len   (map (cut ~ <> 2) size)]
       [type-size
        `(sizeOfExpr (typeName (@ (type ,(sxml:attr var 'element_type)))))])

    (gen-funcall
     fun-name
     `(plusExpr
       ,var
       (mulExpr ,type-size ,(gen-*-expr (cons start (cdr len)))))
     `(mulExpr ,type-size ,(gen-*-expr len)))
    ))

(define (generate-update-state label arg)
  (and-let*
      ([(not (eq? (sxml:name arg) 'Var))]

       [fun-name
        (match label
          ["HOST"
           "__jacc_update_self"]

          ["DEVICE"
           "__jacc_update_device"]

          [else #f])]

       [var   (~ (sxml:content arg) 0)]
       [size  (cdr (sxml:content arg))]
       [start (~ size 0 1)]
       [len   (map (cut ~ <> 2) size)]
       [type-size
        `(sizeOfExpr (typeName (@ (type ,(sxml:attr var 'element_type)))))])

    (gen-funcall
     fun-name
     `(plusExpr
       ,var
       (mulExpr ,type-size ,(gen-*-expr (cons start (cdr len)))))
     `(mulExpr ,type-size ,(gen-*-expr len)))
    ))

(define (translate-transfer-clauses state gen)
  (apply gen-compound

   (append-map
    (lambda (c)
      (let ([label ((ccc-sxpath "string")       c)]
            [args  ((content-car-sxpath "list") c)])
        (filter-map (pa$ gen label) args)))

    ((content-car-sxpath "list") state))
   ))

(define (translate-data-clauses state enter)
  (translate-transfer-clauses
   state
   (lambda (label arg) (generate-data-state label arg enter))))

(define (translate-update-clauses state)
  (translate-transfer-clauses state generate-update-state))

(define (jit-acc-update! xm state)
  (let1 c (translate-update-clauses state)
    ;; sync after update
    (sxml:change! state (gen-compound c (gen-funcall "__jacc_wait")))))

(define (jit-acc-enter! xm state enter)
  (let1 c (translate-data-clauses state enter)
    (if enter
        (sxml:change! state c)
        ;; sync after exit
        (sxml:change! state (gen-compound c (gen-funcall "__jacc_wait"))))))

(define (jit-acc-data! xm state)
  (sxml:change!
   state

   (gen-compound
    (translate-data-clauses state #t)

    (~ (sxml:content state) 2)

    ;; sync after exit
    (let ([c (translate-data-clauses state #f)]
          [labels ((sxpath "list/*/string/text()") state)])
      (if (null? (lset-difference equal? labels '("PRESENT")))
          c (gen-compound c (gen-funcall "__jacc_wait"))))
    )))

(define (extract-array-type name state)
  ((if-car-sxpath
    `(// (((or@ arrayAddr Var)) ((equal? ,name))) @ type *text*)) state))

(define (extract-accessed-arrays state)
  (delete-duplicates
   (append
    ((sxpath
      `(// ((arrayRef (* 1)) (,(sxpath:name 'arrayAddr))) *text*)) state)
    ((sxpath
      `(// (not@ pointerRef) * pointerRef
           ((plusExpr (* 1)) (,(sxpath:name 'Var))) *text*)) state)
    )))

(define (extract-array-type-definition type xm)
  ((if-car-sxpath
    `((((or@ pointerType arrayType))
       (@ (type ((equal? ,type))))))) (xm-type-table xm)))

;; Return (n x m) from the type "double [n][m]"
(define (static-array-size type xm)
  (let1 type-definition (extract-array-type-definition type xm)

    (match type-definition
      [('arrayType _ ...)
       `(mulExpr
         (intConstant ,(sxml:attr type-definition 'array_size))
         ,(static-array-size (sxml:attr type-definition 'element_type) xm))]

      [#f `(sizeOfExpr (typeName (@ (type ,type))))]

      [else (gen-int-expr 0)]
      )))

(define (xcodeml-expr->string e :optional (rename values))
  (define r (lambda (x) (xcodeml-expr->string x rename)))
  (match (cons (sxml:name e) (sxml:content e))
    [('plusExpr a b)
     #"(~(r a) + (~(r b)))"]

    [('minusExpr a b)
     #"(~(r a) - (~(r b)))"]

    [('mulExpr a b)
     #"(~(r a) * (~(r b)))"]

    [('divExpr a b)
     #"(~(r a) / (~(r b)))"]

    [('intConstant _ ...)
     (sxml:car-content e)]

    [('Var _ ...)
     (rename (sxml:car-content e))]

    [else
     (error #"Unknown expr: ~e")]))

;; Return "double (*)[n][m]"
(define (generate-array-type-specifier
         type xm rename :optional (prefix ""))
  (let1 type-definition (extract-array-type-definition type xm)

    (match type-definition
      ;; pointerType must refer one of arrayType or scalar types.
      [('pointerType _ ...)
       (generate-array-type-specifier
        (sxml:attr type-definition 'ref) xm rename " ")]

      [('arrayType _ ...)
       (let* ([array-size (sxml:attr type-definition 'array_size)]
              [array-size
               (if (not (equal? array-size "*")) array-size
                   (xcodeml-expr->string
                    (car ((sxpath "arraySize/*") type-definition)) rename))])
         (generate-array-type-specifier
          (sxml:attr type-definition 'element_type) xm rename
          ;; Omit the first dimension
          (if (string-null? prefix) " "
              (string-append
               prefix "[" array-size "]"))))]

      [else #"~type (*)~prefix"]
      )))

(define (collect-array-type-definition type xm)
  (let1 type-definition (extract-array-type-definition type xm)

    (match type-definition
      [('pointerType _ ...)
       (collect-array-type-definition (sxml:attr type-definition 'ref) xm)]

      [('arrayType _ ...)
       (append
        type-definition
        (collect-array-type-definition
         (sxml:attr type-definition 'element_type) xm))]

      [else '()]
      )))

(define KERNEL_COUNT 0)

(define (jit-acc-parallel! xm state)
  (let* (;; PGI doesn't accept long variables
         ;;[timestamp (sys-strftime "%H%M%S" (sys-localtime (sys-time)))]
         ;;[timestamp
         ;; (string-take #"~|timestamp|~(~ (current-time) 'nanosecond)" 9)]
         [timestamp (inc! KERNEL_COUNT)]
         [add-timestamp (lambda (x) #"~|x|_~|timestamp|")]
         [state-orig      state] ; for sxml:change!
         [state           (list-copy-deep state)]

         [referenced-vars
          (append
           ((sxpath '(// (not@ list) Var *text*)) state)
           ((sxpath `(// (list (string *text*
                                       ,(make-sxpath-query
                                         #/^(?!PRESENT|DEVICEPTR)/)))
                         // Var *text*)) state)
           ((sxpath `(// (list (string *text*
                                       ,(make-sxpath-query
                                         #/^(PRESENT|DEVICEPTR)/)))
                         list list list // Var *text*)) state))]

         [presented-vars
          (append
           ((sxpath `(// (list (string *text* ,(make-sxpath-query
                                                #/^(PRESENT|DEVICEPTR)$/)))
                         list Var *text*)) state)
           ((sxpath `(// (list (string *text* ,(make-sxpath-query
                                                #/^(PRESENT|DEVICEPTR)$/)))
                         list list Var *text*)) state))]

         [declared-vars ((sxpath '(// compoundStatement
                                      declarations varDecl name *text*))
                         state)]

         [external-arrays (extract-accessed-arrays state)]

         [external-vars
          (delete-duplicates
           (lset-difference
            equal? referenced-vars declared-vars external-arrays))]

         [reducted-vars
          ((sxpath
            `(// ,(sxpath:name 'ACCPragma)
                 list
                 (list (string *text* ,(make-sxpath-query #/^REDUCTION_/)))
                 list Var *text*))
           state)]

         [unused-presented-vars
          (delete-duplicates
           (lset-difference
            equal? presented-vars referenced-vars external-arrays))]

         [lookup-id
          (lambda (s)
            (car ((sxpath `(// (id (name (equal? ,s))) @ type *text*))
                  (xm-global-symbols xm))))]

         [id-of-jacc-arg (lookup-id "__JaccArg")]

         [id-of-jacc-arg-attr (lookup-id "__JaccArgAttr")]

         [pointer-type-to-jacc-arg (add-pointer-type! xm id-of-jacc-arg)]

         [array-types
          (map
           (^[name]
             (or (extract-array-type name state)
                 (error #"Array type not found: ~name")))
           external-arrays)]

         [array-definitions
          (append-map (cut collect-array-type-definition <> xm) array-types)]

         [index-vars ((sxpath '(// Var *text*)) array-definitions)]

         [external-vars (lset-union equal? external-vars index-vars)]

         [var-types
          (map
           (^[v]
             (or ((if-car-sxpath `(// (Var ((equal? ,v))) @ type *text*))
                  (append state array-definitions))
                 (error #"Variable type not found: ~v")))
           external-vars)]

         [compound-for-jit-code
          (let1 def
              (map (^[symbol type is-array]
                     (list
                      (if is-array (add-pointer-type! xm type) type)
                      symbol))
                   (append external-arrays external-vars)
                   (append array-types var-types)
                   (append array-types (map not var-types)))

            (gen-compound-with-local-vars def state))]

         [compound-for-jit-code (list-copy-deep compound-for-jit-code)])

    ;; Remove unused present vars and also scalars, that would be re-allocated
    (for-each
     (^[lst]
       (sxml:change-content!
        lst
        (filter
         (^[v]
           (let1 varname ((car-sxpath '(// Var *text*)) v)
             (and (not (member varname unused-presented-vars))
                  (not (member varname external-vars)))))
         (sxml:content lst))))
     ((sxpath `(// (list (string *text* ,(make-sxpath-query
                                          #/^(PRESENT|DEVICEPTR)$/)))
                   list)) compound-for-jit-code))

    ;; Remove empty present clauses
    (for-each
     (^[lst]
       (sxml:change-content!
        lst
        (filter
         (^[l]
           (or (not (or ((if-car-sxpath '(string (equal? "PRESENT"))) l)
                        ((if-car-sxpath '(string (equal? "DEVICEPTR"))) l)))
               ((if-car-sxpath '(list *)) l)))
         (sxml:content lst))))
     ((sxpath `(// (list (list (string (*text* ,(make-sxpath-query
                                                 #/^(PRESENT|DEVICEPTR)$/)))))
                   )) compound-for-jit-code))

    ;; put async
    (let1 clauses ((car-sxpath "//list") compound-for-jit-code)
      (sxml:change-content!
       clauses (cons `(list (string "ASYNC")) (sxml:content clauses))))

    ;; Append timestamp to all variables
    (for-each
     (^[v]
       (sxml:change-content!
        v
        (list (add-timestamp (sxml:car-content v)))))
     (append
      ((sxpath '(// (or@ Var varAddr arrayAddr))) compound-for-jit-code)
      ((sxpath '(// (or@ symbols declarations) (or@ id varDecl) name))
       compound-for-jit-code)))

    ;; Replace a kernel launch
    (sxml:change!
     state-orig

     (apply gen-compound-with-local-vars
      `((,pointer-type-to-jacc-arg "__arg" (intConstant "0")))

      (append
       (map
        (^[symbol type is-array]
          (let* ([type-def (extract-array-type-definition type xm)]
                 [is-static-array
                  (and is-array
                       (eq? 'arrayType (sxml:name type-def))
                       (sxml:attr type-def 'array_size))])
            (gen-var= "__arg"
             `(compoundValueAddr (@ (type ,pointer-type-to-jacc-arg))
               (value
                (value
                 ;; type
                 (stringConstant
                  ,(if (not is-array) type
                       (generate-array-type-specifier
                        type xm add-timestamp)))

                 ;; symbol
                 (stringConstant ,(add-timestamp symbol))

                 ;; variable address
                 (varAddr ,symbol)

                 ;; array or variable address
                 ,(if is-array `(Var ,symbol) `(varAddr ,symbol))

                 ;; data size
                 (sizeOfExpr
                  (typeName
                   (@ (type ,(if is-array
                                 (add-pointer-type! xm type) type)))))

                 ;; attr
                 ,(gen-+-expr
                   (map
                    (^[label cond]
                      `(moeConstant (@ (type ,id-of-jacc-arg-attr))
                                    ,(if cond label "__JACC_NONE")))

                    '("__JACC_ARRAY" "__JACC_STATIC"
                      "__JACC_PRESENT" "__JACC_REDUCTED" "__JACC_WRITTEN")

                    (list is-array is-static-array
                          (member symbol presented-vars)
                          (member symbol reducted-vars)

                          (let1 assigns '(or@
                                          postIncrExpr postDecrExpr
                                          preIncrExpr preDecrExpr
                                          assignExpr asgPlusExpr asgMinusExpr
                                          asgMulExpr asgDivExpr asgModExpr
                                          asgLshiftExpr asgRshiftExpr
                                          asgBitAndExpr asgBitOrExpr
                                          asgBitXorExpr)
                            (or ((if-car-sxpath
                                  `(// ,assigns pointerRef plusExpr
                                       Var (equal? ,symbol))) state)
                                ((if-car-sxpath
                                  `(// varAddr (equal? ,symbol))) state)
                                ((if-car-sxpath
                                  `(// (( ,assigns (* 1)))
                                       ,@(if is-array '(//) '())
                                       ((,(sxpath:name
                                           (if is-array 'arrayAddr 'Var))
                                         (equal? ,symbol)))))
                                 state)))
                          )))

                 ;; arg->next
                 (Var "__arg")
                 ))))))
        (append external-arrays external-vars)
        (append array-types var-types)
        (append array-types (map not var-types)))

       (list
        (gen-funcall
         "__jacc_kernel_push"

         `(stringConstant
           ,(regexp-replace-all*
             (string-join (sxml->c xm compound-for-jit-code) "\\n")
             #/\"/ "\\\\\""))

         '(Var "__arg"))))
      ))))

(define XtoC_PATH
  #"~(sys-dirname (current-load-path))/tools/xcodeml-to-c")

(define (sxml->c xm compound-state)
  (let*
      ([compound-state
        `(compoundStatement
          ,(~ compound-state 1)
          ,(~ compound-state 2)
          (body
           (exprStatement (stringConstant "<JACC>"))
           ,@(cdr (~ compound-state 3))
           (exprStatement (stringConstant "</JACC>"))))]

       [sxml
        `(XcodeProgram

          ,(append
            (xm-type-table xm)
            '((functionType
               (|@| (type "__F1") (return_type "int")) (params))))

          ,(append
            (xm-global-symbols xm)
            '((id (|@| (type "__F1") (sclass "extern_def")) (name "__main"))))

          (globalDeclarations
           (functionDefinition
            (name "__main") (symbols) (params)
            (body ,compound-state))))])

    (call-with-temporary-file
      (lambda (oport path)
        (srl:sxml->xml sxml oport)
        (flush oport)
        (process-output->string-list #"~XtoC_PATH ~path -s")
        ))))

(define (pass2 xm)
  (rlet1 xm (xm-copy xm)
    (add-jacc-init! xm)

    (for-each
     (cut jit-acc! xm <>)
     ((sxpath '(// ACCPragma)) (xm-global-declarations xm)))
    ))
