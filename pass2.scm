;;      This file is part of JACC and is licenced under terms contained in the COPYING file
;;
;;      Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

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
     `(plusExpr ,var ,(gen-*-expr (cons start (cdr len))))
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
     `(plusExpr ,var ,(gen-*-expr (cons start (cdr len))))
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

(define (extract-assign state)
  ((sxpath `(// (not@ init) (((or@ ,@XM_ASSIGNS))))) (list 'top state)))

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

;; Return "double (*)[n][m]"
(define (generate-array-type-specifier type xm rename :optional (prefix ""))
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

      [else #"~(generate-var-type-specifier type xm) (*)~prefix"]
      )))

(define (extract-array-element-type type xm)
  ((#/^([^ ]*) \(/ (generate-array-type-specifier type xm values)) 1))

(define (extract-array-size type xm)
  (let1 type-definition (extract-array-type-definition type xm)

    (match type-definition
      ;; pointerType must refer one of arrayType or scalar types.
      [('pointerType _ ...)
       (cons #f (extract-array-size (sxml:attr type-definition 'ref) xm))]

      [('arrayType _ ...)
       (let* ([array-size (sxml:attr type-definition 'array_size)]
              [array-size
               (cond [(not array-size) #f]
                     [(not (equal? array-size "*"))
                      `(intConstant (@ (type "int")) ,array-size)]
                     [else (car ((sxpath "arraySize/*") type-definition))])])
         (cons array-size
               (extract-array-size
                (sxml:attr type-definition 'element_type) xm)))]

      [else '()]
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

(define XM_ASSIGNS
  '(postIncrExpr postDecrExpr
    preIncrExpr preDecrExpr
    assignExpr asgPlusExpr asgMinusExpr
    asgMulExpr asgDivExpr asgModExpr
    asgLshiftExpr asgRshiftExpr
    asgBitAndExpr asgBitOrExpr
    asgBitXorExpr))

(define (extract-array-write state)
  ((sxpath `(// (((or@ ,@XM_ASSIGNS)
                  ((,(make-sxpath-query
                      (lambda (x)
                        (let1 n (car (sxml:car-content x))
                              (or (eq? n 'arrayRef) (eq? n 'pointerRef)) ))
                      )) ))))) state))

(define (extract-array-read state)
  (append
   ((sxpath `(// (not@ ,@XM_ASSIGNS) (or@ arrayRef pointerRef))) state)
   ((sxpath `(// (or@ ,@XM_ASSIGNS)
                 (((* 2)) (,(sxpath:name 'arrayRef))))) state)
   ((sxpath `(// (or@ ,@XM_ASSIGNS)
                 (((* 2)) (,(sxpath:name 'pointerRef))))) state)))

(define (jit-acc-parallel! xm state)
  (let* ([state-orig      state] ; for sxml:change!
         [state           (list-copy-deep state)]
         [add-underscore (lambda (x) #"__~|x|")]

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

         [find-iterator (sxpath `(// init assignExpr (* 1) ,(sxpath:name 'Var) *text*))]

         [sequential-iterators (find-iterator innermost-parallel-state)]

         [parallel-iterators
          (fold delete-one (find-iterator state) sequential-iterators)]

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

         [array-size-alist
          (map cons external-arrays
               (map (cut extract-array-size <> xm) array-types))]

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
              (append-map
               (^[symbol type is-array]
                 (let1 type (if is-array (add-pointer-type! xm type) type)
                   `((,type ,symbol)
                     ,@(if is-array `( ("int" ,#"~|symbol|__lb")
                                       ("int" ,#"~|symbol|__ub") ) '() ))))
               (append external-arrays external-vars)
               (append array-types var-types)
               (append array-types (map not var-types)))

            (gen-compound-with-local-vars
             (append '(("int" "gpunum") ("int" "gpuid")) def) state))]

         [compound-for-jit-code (list-copy-deep compound-for-jit-code)]

         [pragma-in-jit-code ((car-sxpath '(body ACCPragma)) compound-for-jit-code)]

         [conflict (extract-conflict pragma-in-jit-code sequential-iterators)]

         [dependency (extract-dependency pragma-in-jit-code sequential-iterators)]

         [var-written
          (append
           ((sxpath `(// varAddr *text*)) state)
           ((sxpath `(// (or@ ,@XM_ASSIGNS)
                         (* 1) ,(sxpath:name 'pointerRef) plusExpr
                         (* 1) ,(sxpath:name 'Var) *text*)) state)
           ((sxpath `(// (or@ ,@XM_ASSIGNS)
                         (* 1) ,(sxpath:name 'arrayRef) arrayAddr *text*)) state)
           ((sxpath `(// (or@ ,@XM_ASSIGNS)
                         (* 1) ,(sxpath:name 'Var) *text*)) state))]

         [var-read
          (append
           ((sxpath `(// (not@ exprStatement) (or@ ,@XM_ASSIGNS)
                         (* 1) ,(sxpath:name 'pointerRef) plusExpr
                         (* 1) ,(sxpath:name 'Var) *text*)) state)
           ((sxpath `(// (not@ exprStatement) (or@ ,@XM_ASSIGNS)
                         (* 1) ,(sxpath:name 'arrayRef) arrayAddr *text*)) state)
           ((sxpath `(// (not@ exprStatement) (or@ ,@XM_ASSIGNS)
                         (* 1) ,(sxpath:name 'Var) *text*)) state)
           ((sxpath `(// varAddr *text*)) state)
           ((sxpath `(// (or@ ,@XM_ASSIGNS)
                         (* 2) ,(sxpath:name 'pointerRef) plusExpr
                         (* 1) ,(sxpath:name 'Var) *text*)) state)
           ((sxpath `(// (or@ ,@XM_ASSIGNS)
                         (* 2) ,(sxpath:name 'arrayRef) arrayAddr *text*)) state)
           ((sxpath `(// (or@ ,@XM_ASSIGNS)
                         (* 2) ,(sxpath:name 'Var) *text*)) state)
           ((sxpath `(// (not@ ,@XM_ASSIGNS) pointerRef plusExpr
                         (* 1) ,(sxpath:name 'Var) *text*)) state)
           ((sxpath `(// (not@ ,@XM_ASSIGNS) arrayRef arrayAddr *text*)) state)
           ((sxpath `((not@ ,@(cons 'list XM_ASSIGNS)) Var *text*))
            (lset-difference
             equal?
             ((sxpath '(// (* (* Var)))) state)
             ((sxpath `(// (pointerRef
                            (plusExpr (((* 1) ,(sxpath:name 'Var))))))) state))))]

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

         [ps (and pt ((if-car-sxpath
                       `(// (forStatement
                             (init
                              (assignExpr
                               (Var (((equal? ,pt))))))))) state))]

         [loop-counter (and ps (extract-loop-counters ps))]
         [loop-counter (and (pair? loop-counter) (car loop-counter))]

         [loop-init (and loop-counter (~ loop-counter 1))]
         [loop-ub   (and loop-counter (~ loop-counter 2))]
         [loop-ub   (if (and loop-counter (eq? (~ loop-counter 4) '<=))
                        `(plusExpr ,loop-ub ,(gen-int-expr 1))
                        loop-ub)]
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

    ;; put async
    (let1 clauses ((car-sxpath "//list") compound-for-jit-code)
      (sxml:change-content!
       clauses (cons `(list (string "ASYNC")) (sxml:content clauses))))

    ;; put predicate ( a[x][y]=... -> (I <= (x*N+y) <= J) ? (a[]=..) : a[]; )
    (for-each
     (lambda (assign)
       (let* ([assign-orig assign]
              [assign (list-copy-deep assign)]
              [lv (sxml:car-content assign)]
              [lv-orig (sxml:car-content assign-orig)])

         (case (or (sxml:attr assign 'jacc_dup) (sxml:name lv))
           [(arrayRef)
            (let* ([c (sxml:content lv)]
                   [name (sxml:car-content (car c))]
                   [index (cdr c)]
                   [out (cons `(,name ,@index)
                              (find-output (add-tag name lv-orig) conflict))]
                   [index-1ds (map gen-index1d out)]
                   [filter (delete-duplicates (map cons (map car out) index-1ds))])

              (unless #f #;(> (length filter) 5)
                (push! dist-vars name)

                (sxml:change!
                 assign-orig
                 (list-copy-deep
                  (gen-cond-expr
                   (gen-OR-expr (map construct-predicate filter))
                   assign
                   lv)))))]

           [(pointerRef)
            (and-let*
                ([cc (sxml:car-content lv)]
                 [(and (eq? (sxml:name cc) 'plusExpr)
                       (eq? (sxml:name (sxml:car-content cc)) 'Var))]
                 [name (sxml:car-content (sxml:car-content cc))]
                 [index (cdr (sxml:content cc))]
                 [out (cons `(,name ,@index)
                            (find-output (add-tag name lv-orig) conflict))]
                 [index-1ds (map gen-index1d out)]
                 [filter (delete-duplicates (map cons (map car out) index-1ds))])

                (unless #f #;(> (length filter) 5)
                  (push! dist-vars name)

                  (sxml:change!
                   assign-orig
                   (list-copy-deep
                    (gen-cond-expr
                     (gen-OR-expr (map construct-predicate filter))
                     assign
                     lv)))))]

           [(Var)
            (let* ([name (sxml:car-content (sxml:content lv))]
                   [out ;; find-output returns only arrays and reducted vars
                    (find-output (add-tag name lv-orig) conflict)]
                   ;; only reduction; otherwise out := '()
                   [out (if (member name reducted-vars) (cons `(,name) out) '())]
                   [index-1ds (map gen-index1d out)]
                   [fil (delete-duplicates (map cons (map car out) index-1ds))])
              (when (pair? fil)
                (sxml:change!
                 assign-orig

                 (list-copy-deep
                  (gen-cond-expr
                   (gen-OR-expr (map construct-predicate fil))
                   assign
                   lv)))))]
           )))
     assigns)

    ;; put predicate for read
    ;; ( a[i] -> ( ( b_lb <= ... && b_ub >= ... ) || ... ) ? a[i] : 0 )
    (for-each
     (lambda (fetch)
       (let* ([fetch-orig fetch]
              [fetch (list-copy-deep fetch)])

         (case (or (sxml:attr fetch 'jacc_dup) (sxml:name fetch))
           [(arrayRef)
            (let* ([c (sxml:content fetch)]
                   [name (sxml:car-content (car c))]
                   [out  (find-output (add-tag name fetch-orig) dependency)]
                   [index-1ds (map gen-index1d out)]
                   [filter (delete-duplicates (map cons (map car out) index-1ds))])

              (unless (or (null? filter) #f #;(> (length filter) 5))
                (sxml:change!
                 fetch-orig
                 (list-copy-deep
                  (gen-cond-expr
                   (gen-OR-expr (map construct-predicate filter))
                   fetch
                   (gen-int-expr 0))))
                ))]

           [(pointerRef)
            (and-let*
                ([cc (sxml:car-content fetch)]
                 [(and (eq? (sxml:name cc) 'plusExpr)
                       (eq? (sxml:name (sxml:car-content cc)) 'Var))]
                 [name (sxml:car-content (sxml:car-content cc))]
                 [out  (find-output (add-tag name fetch-orig) dependency)]
                 [index-1ds (map gen-index1d out)]
                 [filter (delete-duplicates (map cons (map car out) index-1ds))])

              (unless (or (null? filter) #f #;(> (length filter) 5))
                (sxml:change!
                 fetch-orig
                 (list-copy-deep
                  (gen-cond-expr
                   (gen-OR-expr (map construct-predicate filter))
                   fetch
                   (gen-int-expr 0))))
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

     (apply gen-compound-with-local-vars
      `((,pointer-type-to-jacc-arg "__arg" (intConstant "0")))

      (append
       (map
        (^[symbol type is-array]
          (let* ([type-def (extract-array-type-definition type xm)]
                 [is-static-array
                  (and is-array
                       (eq? 'arrayType (sxml:name type-def))
                       (sxml:attr type-def 'array_size))]
                 [array-size (and is-array (assoc-ref array-size-alist symbol))]
                 [splitdim   (and is-array (assoc-ref split-dimention symbol))])

            (gen-var= "__arg"
             `(compoundValueAddr (@ (type ,pointer-type-to-jacc-arg))
               (value
                (value
                 ;; type
                 (stringConstant
                  ,(if (not is-array)
                       (generate-var-type-specifier type xm)
                       (generate-array-type-specifier type xm add-underscore)))

                 ;; symbol
                 (stringConstant ,(add-underscore symbol))

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
                      "__JACC_PRESENT" "__JACC_REDUCTED" "__JACC_DIST"
                      "__JACC_WRITTEN" "__JACC_READ")

                    (list is-array is-static-array
                          (member symbol presented-vars)
                          (member symbol reducted-vars)
                          (member symbol dist-vars)
                          (member symbol var-written)
                          (member symbol var-read))
                    ))

                 ;; split_dimsize or reduction type
                 ,(or (and splitdim (~ array-size splitdim))

                      (gen-int-expr
                       (match (assoc-ref reduction-label symbol)
                         ["REDUCTION_PLUS"   0]
                         ["REDUCTION_MINUS"  1]
                         ["REDUCTION_MUL"    2]
                         ["REDUCTION_LOGAND" 3]
                         ["REDUCTION_LOGOR"  4]
                         ["REDUCTION_BITAND" 5]
                         ["REDUCTION_BITOR"  6]
                         ["REDUCTION_BITXOR" 7]
                         ["REDUCTION_MIN"    8]
                         ["REDUCTION_MAX"    9]
                         [else 0])))

                 ;; memdepth
                 ,(or (and splitdim (gen-*-expr (drop array-size (+ splitdim 1))))
                      (gen-int-expr 0))

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
