(define-module pass2
  (use util)
  (use xm)
  (use analysis)

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

(define (new-type)
  #"jacc__t~(inc! NEW_TYPE_COUNT)")

(define (add-extended-type! xm ref len)
  (rlet1 ntype (new-type)
    (sxml:content-push!
     (xm-type-table xm)
     `(FbasicType
       (@ (type ,ntype) (ref ,ref))
       (kind ,(gen-int-expr len)))
     )))

(define (add-jacc-init! xm)
  (let*
      ([program-type
        ((if-car-sxpath
          '(// (FfunctionType (@ (is_program ((equal? "true")))))
               @ type *text*))
         (xm-type-table xm))]

       [main-body
        ((if-car-sxpath
          `(// (FfunctionDefinition
                (name (@ (type (equal? ,program-type))))) body))
         (xm-global-declarations xm))])

    (sxml:content-push!
     (xm-type-table xm)
     '(FfunctionType (@ (type "UNDEF") (return_type "UNDEF"))))

    (for-each
     (lambda (f)
       (sxml:content-push!
        (xm-global-symbols xm)
        `(id (@ (type "UNDEF") (sclass "ffunc")) (name ,f))))
     '("jacc_init"
       "jacc_close"
       "jacc_arg_build"
       "jacc_wait"
       "jacc_acc_init"
       "jacc_acc_shutdown"
       "jacc_kernel_push"
       "jacc_copyin"
       "jacc_create"
       "jacc_copyout"
       "jacc_delete"
       "jacc_update_self"
       "jacc_update_device"
       "c_loc"
       "merge"
       ))

    (for-each
     (lambda (decl)
       (sxml:content-push!
        decl
        '(FuseDecl (@ (name "mod_jacc")))))
     ((sxpath '((or@ FfunctionDefinition FmoduleDefinition) declarations))
      (xm-global-declarations xm)))

    (when main-body
      (sxml:change-content!
       main-body
       `(,(gen-funcall "jacc_init" "Fvoid")
         ,@(cdr main-body)
         ,(gen-funcall "jacc_close" "Fvoid"))
       ))))

(define (generate-data-state label arg enter)
  (and-let*
   ([fun-name
     (if enter
         (match label
           [(or "COPY" "COPYIN" "PRESENT_OR_COPY" "PRESENT_OR_COPYIN")
            "jacc_copyin"]

           [(or "COPYOUT" "CREATE" "PRESENT_OR_COPYOUT" "PRESENT_OR_CREATE")
            "jacc_create"]

           [else #f])

         (match label
           [(or "COPY" "COPYOUT" "PRESENT_OR_COPY" "PRESENT_OR_COPYOUT")
            "jacc_copyout"]

           [(or "COPYIN" "CREATE" "PRESENT_OR_COPYIN" "PRESENT_OR_CREATE")
            "jacc_delete"]

           [else #f]))]

    [var (if (eq? (sxml:name arg) 'Var) arg
             ;; list
             (~ (sxml:content arg) 0))]
    #;[size  (cdr (sxml:content arg))]
    #;[start (~ size 0 1)]
    #;[len   (map (cut ~ <> 2) size)]
    #;[type-size
    `(sizeOfExpr (typeName (@ (type ,(sxml:attr var 'element_type)))))])

   (gen-funcall
    fun-name
    "Fvoid"
    ;; `(plusExpr ,var ,(gen-*-expr (cons start (cdr len))))
    (gen-funcall-expr "c_loc" "Fint" var)
    ;; `(mulExpr ,type-size ,(gen-*-expr len))
    (gen-funcall-expr "sizeof" "Fint" var)
    )))

(define (generate-update-state label arg)
  (and-let*
      ([(not (eq? (sxml:name arg) 'Var))]

       [fun-name
        (match label
          ["HOST"
           "jacc_update_self"]

          ["DEVICE"
           "jacc_update_device"]

          [else #f])]

       [var   (~ (sxml:content arg) 0)]
       [size  (cdr (sxml:content arg))]
       [start (~ size 0 1)]
       [len   (map (cut ~ <> 2) size)]
       [type-size
        `(sizeOfExpr (typeName (@ (type ,(sxml:attr var 'element_type)))))])

    (gen-funcall
     fun-name
     "Fvoid"
     ;; `(plusExpr ,var ,(gen-*-expr (cons start (cdr len))))
     (gen-funcall-expr "c_loc" "Fint" var)
     ;; `(mulExpr ,type-size ,(gen-*-expr len))
     (gen-funcall-expr "sizeof" "Fint" var)
     )))

(define (translate-transfer-clauses state gen)
  (apply gen-block

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
    (sxml:change! state c)))

(define (jit-acc-enter! xm state enter)
  (let1 c (translate-data-clauses state enter)
    (sxml:change! state c)))

(define (jit-acc-data! xm state)
  (sxml:change!
   state

   (apply gen-block
    `(,(translate-data-clauses state #t)

      ,@(let1 x (~ (sxml:content state) 2)
          (if (eq? (sxml:name x) 'list)
              (cdr x) (list x)))

      ,(translate-data-clauses state #f)
      ))))

(define (extract-assign state)
  ((sxpath `(// (((or@ ,@XM_ASSIGNS))))) state))

(define (extract-array-type name state)
  ((if-car-sxpath
    `(// (((or@ arrayAddr Var)) ((equal? ,name))) @ type *text*)) state))

(define (extract-accessed-arrays state)
  (delete-duplicates
   ((sxpath `(// FarrayRef varRef Var *text*)) state)))

(define (extract-type-definition type xm)
  ((if-car-sxpath
    `((FbasicType
       (@ (type ((equal? ,type))))))) (xm-type-table xm)))

;; ;; Return (n x m) from the type "double [n][m]"
;; (define (static-array-size type xm)
;;   (let1 type-definition (extract-array-type-definition type xm)

;;     (match type-definition
;;       [('arrayType _ ...)
;;        `(mulExpr
;;          (intConstant ,(sxml:attr type-definition 'array_size))
;;          ,(static-array-size (sxml:attr type-definition 'element_type) xm))]

;;       [#f `(sizeOfExpr (typeName (@ (type ,type))))]

;;       [else (gen-int-expr 0)]
;;       )))

(define (xcodeml-expr->string e :optional (rename values))
  (define (r e) (xcodeml-expr->string e rename))
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

(define (generate-var-type-specifier type xm)
  (if-let1
   bt ((if-car-sxpath
        `((((or@ basicType))
           (@ (type ((equal? ,type))))))) (xm-type-table xm))

   (let* ([name ((car-sxpath '(@ name *text*)) bt)]
          [const (equal? ((if-car-sxpath '(@ is_const *text*)) bt) "1")])
     ;(if const #"const ~name" name)
     name)

   type))

(define (convert-xcodeml-type type)
  (match type
    ["Fint" "integer"]

    ["Freal" "real"]

    ["Fcomplex" "complex"]

    ["Flogical" "logical"]

    ["Fcharacter" "character"]

    ;["Fvoid" ]
    ))

(define (generate-type-specifier type xm :optional (rename values))
  (let loop ([defs '()] [type type])
    (if-let1 def (extract-type-definition type xm)
      (loop (cons def defs) (sxml:attr def 'ref))

      ;; TODO: parameter
      ;; "type(kind), intent(..), parameter"
      (let (#;[kind      (any (if-car-sxpath "kind/FintConstant/text()") defs)]
            #;[intent    (any (cut sxml:attr <> 'intent) defs)]
            #;[parameter (any (cut sxml:attr <> 'parameter) defs)])

        (string-append
         (convert-xcodeml-type type)
         #;(if kind #"(~kind)" "")
         #;(if intent #", intent(~intent)" "")
         #;(if parameter #", parameter" ""))
        ))))

(define (extract-array-element-type type xm)
  ((#/^([^ ]*) \(/ (generate-array-type-specifier type xm values)) 1))

;; (define (extract-array-size type xm)
;;   (let1 type-definition (extract-array-type-definition type xm)

;;     (match type-definition
;;       ;; pointerType must refer one of arrayType or scalar types.
;;       [('pointerType _ ...)
;;        (cons #f (extract-array-size (sxml:attr type-definition 'ref) xm))]

;;       [('arrayType _ ...)
;;        (let* ([array-size (sxml:attr type-definition 'array_size)]
;;               [array-size
;;                (cond [(not array-size) #f]
;;                      [(not (equal? array-size "*"))
;;                       `(intConstant (@ (type "int")) ,array-size)]
;;                      [else (car ((sxpath "arraySize/*") type-definition))])])
;;          (cons array-size
;;                (extract-array-size
;;                 (sxml:attr type-definition 'element_type) xm)))]

;;       [else '()]
;;       )))

;; (define (collect-array-type-definition type xm)
;;   (let1 type-definition (extract-array-type-definition type xm)

;;     (match type-definition
;;       [('pointerType _ ...)
;;        (collect-array-type-definition (sxml:attr type-definition 'ref) xm)]

;;       [('arrayType _ ...)
;;        (append
;;         type-definition
;;         (collect-array-type-definition
;;          (sxml:attr type-definition 'element_type) xm))]

;;       [else '()]
;;       )))

(define XM_ASSIGNS
  '(FassignStatement))

(define (extract-array-write state)
  ((sxpath `(// (((or@ ,@XM_ASSIGNS))
                 ((((* 1)) (,(sxpath:name 'FarrayRef))))))) state))

(define (extract-array-read state)
  (append
   ((sxpath `(// (not@ ,@XM_ASSIGNS) FarrayRef)) state)
   ((sxpath `(// (or@ ,@XM_ASSIGNS)
                 (((* 2)) (,(sxpath:name 'FarrayRef))))) state)))

(define (jit-acc-parallel! xm state)
  (let* ([state-orig      state] ; for sxml:change!
         [state           (list-copy-deep state)]
         [add-underscore (lambda (x) #"j__~|x|")]

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

         [declared-vars ((sxpath '(// blockStatement
                                      declarations varDecl name *text*))
                         state)]

         [external-arrays (extract-accessed-arrays state)]

         [external-vars
          (delete-duplicates
           (lset-difference
            equal? referenced-vars declared-vars external-arrays))]

         [delete-one
          (lambda (x lst)
            (let loop ([lst lst])
              (cond [(null? lst) '()]
                    [(equal? (car lst) x) (cdr lst)]
                    [else (cons (car lst) (loop (cdr lst)))])))]

         [innermost-parallel-state
          (extract-innermost-parallel-region state)]

         [reducted-vars
          (let* ([sx
                  (sxpath
                   `(// ,(sxpath:name 'ACCPragma)
                        list
                        (list (string *text* ,(make-sxpath-query #/^REDUCTION_/)))
                        list Var *text*))]
                 [parent (sx state)]
                 [child  (sx innermost-parallel-state)])
            (fold delete-one parent child))]

         [reduction-list
          ((sxpath
            `(// ,(sxpath:name 'ACCPragma)
                 list
                 (list (string *text* ,(make-sxpath-query #/^REDUCTION_/)))))
           state)]

         [reduction-label
          (append-map
           (^(r)
             (let1 label ((car-sxpath '(string *text*)) r)
               (map (cut cons <> label) ((sxpath '(list Var *text*)) r))))
           reduction-list)]

         [find-iterator (.$ (sxpath `(// FdoStatement Var *text*)) (^(x) `(a ,x)))]

         [sequential-iterators (find-iterator innermost-parallel-state)]

         [parallel-iterators
          (fold delete-one (find-iterator state) sequential-iterators)]

         [unused-presented-vars
          (delete-duplicates
           (lset-difference
            equal? presented-vars referenced-vars external-arrays))]

         [fint8 (add-extended-type! xm "Fint" 8)]

         [array-types
          (map
           (^[name]
             (or (extract-array-type name state)
                 (error #"Array type not found: ~name")))
           external-arrays)]

         [array-type-alist
          (map cons external-arrays array-types)]

         ;; Replaced by lbound, ubound
         ;; [array-size-alist
         ;;  (map cons external-arrays
         ;;       (map (cut extract-array-size <> xm) array-types))]

         ;; [array-definitions
         ;;  (append-map (cut collect-array-type-definition <> xm) array-types)]

         ;; [index-vars ((sxpath '(// Var *text*)) array-definitions)]

         ;; [external-vars (lset-union equal? external-vars index-vars)]

         [var-types
          (map
           (^[v]
             (or ((if-car-sxpath `(// (Var ((equal? ,v))) @ type *text*))
                  state)
                 (error #"Variable type not found: ~v")))
           external-vars)]

         [compound-for-jit-code
          (let1 def
              (append-map
               (^[symbol type is-array]
                 `((,type ,symbol)
                   ,@(if is-array `( ("Fint" ,#"~|symbol|__lb")
                                     ("Fint" ,#"~|symbol|__ub") ) '() )))
               (append external-arrays external-vars)
               (append array-types var-types)
               (append array-types (map not var-types)))

            (gen-block-with-local-vars (cons '("Fint" "gpuid") def) state))]

         [compound-for-jit-code (list-copy-deep compound-for-jit-code)]

         [pragma-in-jit-code ((car-sxpath '(body ACCPragma)) compound-for-jit-code)]

         [conflict (extract-conflict pragma-in-jit-code sequential-iterators)]

         [dependency (extract-dependency pragma-in-jit-code sequential-iterators)]

         [var-written
          (append
           ((sxpath `(// (or@ ,@XM_ASSIGNS)
                         (* 1) ,(sxpath:name 'FarrayRef) varRef Var *text*)) state)
           ((sxpath `(// (or@ ,@XM_ASSIGNS)
                         (* 1) ,(sxpath:name 'Var) *text*)) state))]

         [var-read
          ((sxpath `(// (not@ ,@(cons 'list XM_ASSIGNS)) Var *text*)) state)]

         [var-written (delete-duplicates var-written)]
         [var-read (delete-duplicates var-read)]

         [assigns (extract-assign pragma-in-jit-code)]
         [array-writes (extract-array-write pragma-in-jit-code)]
         [array-reads  (extract-array-read pragma-in-jit-code)]

         [dist-vars '()]

         [find-output
          (^(name set)
            (delete-duplicates
             (filter-map
              (lambda (x)
                (and (equal? name (caar x))
                     (or (> (length (cdr x)) 1)
                         (member (revert-tag (cadr x)) reducted-vars))
                     (remove-tag (cdr x))))
              set)))]

         ;; array -> split-index-position
         [split-dimention (locate-split-dimention
                           array-writes parallel-iterators sequential-iterators)]

         [gen-index1d
          (lambda (access)
            (let* ([name (car access)]
                   [index (cdr access)])

              (and (pair? index)
                   (sxml:snip (~ index (assoc-ref split-dimention name))))))]

         [pt (and (pair? parallel-iterators) (car parallel-iterators))]

         [ps (and pt ((if-car-sxpath `(// (FdoStatement (Var (((equal? ,pt))))))) state))]

         [loop-counter (and ps (extract-loop-counters ps))]

         [loop-init (and loop-counter (~ loop-counter 1))]
         [loop-ub   (and loop-counter (~ loop-counter 2))]
         [loop-ub   ;(if (and loop-counter (eq? (~ loop-counter 3) '<=))
                        `(plusExpr ,loop-ub ,(gen-int-expr 1))]
                        ;loop-ub)]
         [loop-sect (and loop-counter
                         `(divExpr
                           (plusExpr (minusExpr ,loop-ub ,loop-init)
                                     (minusExpr (Var "gpunum") ,(gen-int-expr 1)))
                           (Var "gpunum")))]

         [construct-predicate
          (match-lambda1 (name . i)
            (if i
                (gen-AND-expr
                 `(,(gen-var<=-expr #"~|name|__lb" i)
                   ,(gen-var>=-expr #"~|name|__ub" i)))
                ;; reduction
                ;(gen-==-expr '(Var "gpuid") (gen-int-expr 0))
                (let1 pt (car parallel-iterators)
                  (gen-AND-expr
                   `(,(gen-<=-expr `(plusExpr ,loop-init
                                              (mulExpr ,loop-sect
                                                       (Var "gpuid")))
                                   `(Var ,pt))
                     ,(gen->-expr `(plusExpr ,loop-init
                                             (mulExpr ,loop-sect
                                                      (plusExpr (Var "gpuid")
                                                                ,(gen-int-expr 1))))
                                  `(Var ,pt)))))
                ))])

    (mark-duplicated-statement!
     pragma-in-jit-code conflict dependency '() split-dimention)

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

    ;; put predicate ( a[x][y]=... -> (I <= (x*N+y) <= J) ? (a[]=..) : a[]; )
    (for-each
     (lambda (assign)
       (let* ([assign-orig assign]
              [assign (list-copy-deep assign)]
              [lv (sxml:car-content assign)]
              [lv-orig (sxml:car-content assign-orig)])

         (case (or (sxml:attr assign 'jacc_dup) (sxml:name lv))
           [(FarrayRef)
            (let* ([c (sxml:content lv)]
                   [name (extract-varref-name (car c))]
                   [index (map (select-array-index name) (cdr c))]
                   [out (cons `(,name ,@index)
                              (find-output (add-tag name lv-orig) conflict))]
                   [index-1ds (map gen-index1d out)]
                   [filter (delete-duplicates (map cons (map car out) index-1ds))])

              (unless #f #;(> (length filter) 5)
                (push! dist-vars name)

                (sxml:change!
                 assign-orig
                 (list-copy-deep
                  (gen-if
                   (gen-OR-expr (map construct-predicate filter))
                   assign
                   `(FassignStatement ,lv ,lv)
                   )))))]

           [(Var)
            (let* ([name (sxml:car-content (sxml:content lv))]
                   [out ;; find-output returns only arrays and reducted vars
                    (find-output (add-tag name lv-orig) conflict)]
                   [out (if (member name reducted-vars)
                            (cons `(,name) out) out)]
                   [index-1ds (map gen-index1d out)]
                   [fil (delete-duplicates (map cons (map car out) index-1ds))])
              (when (pair? fil)
                (sxml:change!
                 assign-orig
                 (list-copy-deep
                  (gen-if
                   (gen-OR-expr (map construct-predicate fil))
                   assign
                   `(FassignStatement (Var ,name) (Var ,name))
                   )))))]
           )))
     assigns)

    ;; put predicate for read
    ;; ( a[i] -> ( ( b_lb <= ... && b_ub >= ... ) || ... ) ? a[i] : 0 )
    #;(for-each
     (lambda (fetch)
       (let* ([fetch-orig fetch]
              [fetch (list-copy-deep fetch)])

         (case (or (sxml:attr fetch 'jacc_dup) (sxml:name fetch))
           [(FarrayRef)
            (let* ([c (sxml:content fetch)]
                   [name (extract-varref-name (car c))]
                   [out  (find-output (add-tag name fetch-orig) dependency)]
                   [index-1ds (map gen-index1d out)]
                   [filter (delete-duplicates (map cons (map car out) index-1ds))]
                   [type-spec (generate-type-specifier (assoc-ref array-type-alist name) xm)]
                   [zero `(FrealConstant ,#"~(add-underscore name)__zero")])

              (unless (or (null? filter) #f #;(> (length filter) 5))
                (sxml:change!
                 fetch-orig
                 (list-copy-deep
                  (if (#/complex/ type-spec)
                      (error "Not suported: Merge with complex")
                      #;(gen-funcall-expr "CMPLX" "Fcomplex"
                        (gen-cond-expr
                         (gen-OR-expr (map construct-predicate filter))
                         (gen-funcall-expr "REAL" "Freal" fetch '(FintConstant "8"))
                         '(FrealConstant "0._8"))
                        (gen-cond-expr
                         (gen-OR-expr (map construct-predicate filter))
                         (gen-funcall-expr "AIMAG" "Freal" fetch)
                         '(FrealConstant "0._8")))
                      (gen-cond-expr
                       (gen-OR-expr (map construct-predicate filter))
                       fetch
                       zero))))
                ))]
           )))
     array-reads)

    ;; Append underscore to all variables
    (for-each
     (^[v]
       (sxml:change-content!
        v
        (list (add-underscore (sxml:car-content v)))))
     (append
      ((sxpath '(// (or@ Var varAddr arrayAddr))) compound-for-jit-code)
      ((sxpath '(// (or@ symbols declarations) (or@ id varDecl) name))
       compound-for-jit-code)))

    ;; Replace a kernel launch
    (sxml:change!
     state-orig

     (apply gen-block-with-local-vars
      `((,fint8 "j__arg" (FintConstant "0")))

      (append
       (map
        (^[symbol type is-array]
          (let* ([type-def (extract-type-definition type xm)]
                 [is-static-array #f] ; deplicated
                 [splitdim (and is-array (assoc-ref split-dimention symbol))])

            (gen-funcall
             "jacc_arg_build" "Fvoid"

             ;; type
             `(FcharacterConstant
               ,(append-null-char (generate-type-specifier type xm)))

             ;; symbol
             `(FcharacterConstant ,(append-null-char (add-underscore symbol)))

             ;; addr
             (gen-funcall-expr "c_loc" "Fint" `(Var ,symbol))

             ;; data size
             (gen-funcall-expr "sizeof" "Fint" `(Var, symbol))

             ;; attr
             (gen-+-expr
              (map
               (^[data cond]
                 (gen-int-expr (if cond data 0)))

               '(1  ;"__JACC_ARRAY"
                 2  ;"__JACC_STATIC"
                 4  ;"__JACC_PRESENT"
                 8  ;"__JACC_REDUCTED"
                 16 ;"__JACC_DIST"
                 32 ;"__JACC_WRITTEN"
                 64 ;"__JACC_READ"
                 )

               (list is-array is-static-array
                     (member symbol presented-vars)
                     (member symbol reducted-vars)
                     (member symbol dist-vars)
                     (member symbol var-written)
                     (member symbol var-read))
               ))

             ;; dimnum
             (gen-int-expr (length ((sxpath '((or@ indexRange arrayIndex))) type-def)))

             ;; splitdim or reduction type
             (gen-int-expr (cond [splitdim splitdim]

                                 [(assoc-ref reduction-label symbol) =>
                                  (match-lambda
                                   ["REDUCTION_PLUS"   0]
                                   ["REDUCTION_MINUS"  1]
                                   ["REDUCTION_MUL"    2]
                                   ["REDUCTION_LOGAND" 3]
                                   ["REDUCTION_LOGOR"  4]
                                   ["REDUCTION_BITAND" 5]
                                   ["REDUCTION_BITOR"  6]
                                   ["REDUCTION_BITXOR" 7]
                                   ["REDUCTION_MIN"    8]
                                   ["REDUCTION_MAX"    9])]

                                 [else 0]))

             ;; lbound
             (gen-funcall-expr (if is-array "lbound" "shape") "Fint" `(Var ,symbol))

             ;; ubound
             (gen-funcall-expr (if is-array "ubound" "shape") "Fint" `(Var ,symbol))

             ;; arg->next
             '(Var "j__arg")
             )))
        (append external-arrays external-vars)
        (append array-types var-types)
        (append array-types (map not var-types)))

       (list
        (gen-funcall
         "jacc_kernel_push"
         "Fvoid"

         `(FcharacterConstant
           ,(append-null-char
             (regexp-replace-all*
              (string-join (sxml->c xm compound-for-jit-code) "\\n")
              ;; kernels leads to analysis errors
              ; #/PARALLEL/ (if (sxml:attr state 'jacc_kernel) "KERNELS" "PARALLEL")
              #/\"/ "\\\\\"")))

         '(Var "j__arg"))))
      ))
    ))

(define (append-null-char str)
  (string-append str "\\0"))

(define XtoF_PATH
  #"~(sys-dirname (current-load-path))/tools/xcodeml-to-f")

(define (sxml->c xm compound-state)
  (let*
      ([compound-state
        `(blockStatement
          ,(~ compound-state 1)
          ,(~ compound-state 2)
          (body
           (exprStatement (FcharacterConstant "<JACC>"))
           ,@(cdr (~ compound-state 3))
           (exprStatement (FcharacterConstant "</JACC>"))))]

       [sxml
        `(XcodeProgram

          ,(append
            (xm-type-table xm)
            '((FfunctionType
               (|@| (type "__F1") (return_type "Fint")) (params))))

          ,(append
            (xm-global-symbols xm)
            '((id (|@| (type "__F1") (sclass "extern_def")) (name "__main"))))

          (globalDeclarations
           (FfunctionDefinition
            (name "__main") (symbols) (params) (declarations)
            (body ,compound-state))))])

    (call-with-temporary-file
      (lambda (oport path)
        (srl:sxml->xml sxml oport)
        (flush oport)
        (process-output->string-list #"~XtoF_PATH ~path -s")
        ))))

(define (pass2 xm)
  (rlet1 xm (xm-copy xm)
    (add-jacc-init! xm)

    (for-each
     (cut jit-acc! xm <>)
     ((sxpath '(// ACCPragma)) (xm-global-declarations xm)))
    ))
