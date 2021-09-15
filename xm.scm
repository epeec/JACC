(define-module xm
  (use sxml.tools)
  (use sxml.sxpath)
  (use gauche.record)
  (use util.match)
  (use srfi-11)

  (use util)

  (export
   make-xm
   xm-attributes
   xm-type-table
   xm-global-symbols
   xm-global-declarations

   sxml->xm
   xm->sxml
   xm-copy

   extract-loop-counters

   gen-compound
   gen-compound-with-local-vars
   gen-=-expr
   gen-var=-expr
   gen-var=int-expr
   gen-=
   gen-var=
   gen-var=int
   gen-funcall
   gen-funcall-expr
   gen-arrayref-expr
   gen-arrayref-int-expr
   gen-arrayref-var-expr
   gen-barrier
   gen-if
   gen-!-expr
   gen-!var-expr
   gen-*-expr
   gen-+-expr
   gen-==-expr
   gen-!=-expr
   gen-<-expr
   gen-var<-expr
   gen-var<var-expr
   gen-<=-expr
   gen-var<=-expr
   gen-var<=var-expr
   gen-var++-expr
   gen-for
   gen-int-expr
   gen-when
   ))
(select-module xm)

(define-record-type xm #t #t
  attributes
  (type-table)
  (global-symbols)
  (global-declarations))

(define (xm-copy xm)
  (make-xm
   (list-copy-deep (xm-attributes xm))
   (list-copy-deep (xm-type-table xm))
   (list-copy-deep (xm-global-symbols xm))
   (list-copy-deep (xm-global-declarations xm))))

(define (sxml->xm sxml)
  (make-xm
   (sxml:attr-list                    sxml)
   ((car-sxpath "typeTable")          sxml)
   ((car-sxpath "globalSymbols")      sxml)
   ((car-sxpath "globalDeclarations") sxml)))

(define (xm->sxml xm)
  (sxml:change-attrlist
   (list 'XcodeProgram
         (xm-type-table xm)
         (xm-global-symbols xm)
         (xm-global-declarations xm))
   (xm-attributes xm)))

(define (gen-compound . states)
  `(compoundStatement
    (symbols)
    (declarations)
    (body
     ,@states)))

;; lvar ::= '(type name) | '(type name value)
(define (gen-compound-with-local-vars local-vars . states)
  (define (gen-id type name sclass)
    `(id (@ (type ,type) (sclass ,sclass)) (name ,name)))

  (define (gen-decl name value)
    `(varDecl (name ,name) ,@(if value `((value ,value)) '())))

  (define local-var->id&decl
    (match-lambda
     [(type name)
      (values
       (gen-id type name "auto")
       (gen-decl name #f))]

     [(type name value)
      (values
       (gen-id type name "auto")
       (gen-decl name value))]

     [(type name value sclass)
      (values
       (gen-id type name sclass)
       (gen-decl name value))]

     [else
      (values () ())]
     ))

  (let-values ([(ids decls) (values-map local-var->id&decl local-vars)])
    `
    (compoundStatement
     (symbols ,@ids)
     (declarations ,@decls)
     (body ,@states))
    ))

(define-syntax define-binop-expr
  (syntax-rules ()
    [(_ name label)
     (define (name l r)
       (list label l r))]))

(define-binop-expr gen-=-expr 'assignExpr)

(define (gen-var=-expr varname rvalue)
  (gen-=-expr `(Var ,varname) rvalue))

(define (gen-var=int-expr varname n)
  (gen-var=-expr varname (gen-int-expr n)))

(define-syntax define-binop-state
  (syntax-rules ()
    [(_ name gen-expr)
     (define (name l r)
       `(exprStatement ,(gen-expr l r)))]))

(define-binop-state gen-=       gen-=-expr)
(define-binop-state gen-var=    gen-var=-expr)
(define-binop-state gen-var=int gen-var=int-expr)

(define (gen-funcall fun-name . args)
  `(exprStatement
    ,(apply gen-funcall-expr fun-name args)))

(define (gen-funcall-expr fun-name . args)
  `
  (functionCall
   (function
    (funcAddr ,fun-name))
   (arguments
    ,@args)))

(define (gen-arrayref-expr array-name index)
  `(arrayRef (@ (type "int")) (arrayAddr ,array-name) ,index))

(define (gen-arrayref-int-expr array-name i)
  (gen-arrayref-expr array-name (gen-int-expr i)))

(define (gen-arrayref-var-expr array-name varname)
  (gen-arrayref-expr array-name `(Var ,varname)))

(define (gen-barrier)
  '(OMPPragma (string "BARRIER") (list)))

(define gen-if
  (match-lambda*
   [(cond then)
    `(ifStatement
      (condition ,cond)
      (then ,then))]

   [(cond then else)
    `(ifStatement
      (condition ,cond)
      (then ,then)
      (else ,else))]))

(define (gen-!-expr e)
  `(logNotExpr ,e))

(define (gen-!var-expr varname)
  (gen-!-expr `(Var ,varname)))

(define-syntax define-expr-chain
  (syntax-rules ()
    [(_ name label nil)
     (define (name args)
       (match args
         [()  nil]
         [(x) x]
         [else
          (list
           label
           (car args)
           (name (cdr args)))]))]
    ))

(define-expr-chain gen-*-expr 'mulExpr '(gen-int-expr 1))
(define-expr-chain gen-+-expr 'plusExpr '(gen-int-expr 0))

(define-binop-expr gen-==-expr 'logEQExpr)
(define-binop-expr gen-!=-expr 'logNEQExpr)
(define-binop-expr gen-<=-expr 'logLEExpr)
(define-binop-expr gen-<-expr  'logLTExpr)

(define (gen-var<=-expr varname r)
  (gen-<=-expr `(Var ,varname) r))

(define-syntax define-gen-varOP-expr
  (syntax-rules ()
    [(_ name gen)
     (define (name varname r)
       (gen `(Var ,varname) r))]
    ))

(define-syntax define-gen-varOPvar-expr
  (syntax-rules ()
    [(_ name gen)
     (define (name lvarname rvarname)
       (gen `(Var ,lvarname) `(Var ,rvarname)))]
    ))

(define-gen-varOP-expr    gen-var<-expr     gen-<-expr)
(define-gen-varOPvar-expr gen-var<var-expr  gen-<-expr)
(define-gen-varOP-expr    gen-var<=-expr    gen-<=-expr)
(define-gen-varOPvar-expr gen-var<=var-expr gen-<=-expr)

(define (gen-var++-expr varname)
  `(postIncrExpr (Var ,varname)))

(define-syntax gen-for
  (syntax-rules ()
    [(_ (init-expr cond-expr iter-expr) body-state ...)
     `(forStatement
       (init      ,init-expr)
       (condition ,cond-expr)
       (iter      ,iter-expr)
       (body
        ,body-state ...))]
    ))

(define (gen-int-expr n)
  `(intConstant ,(number->string n)))

(define-syntax gen-when
  (syntax-rules ()
    [(_ cond state ...)
     (if cond
         (gen-compound state ...)
         (gen-compound))]
    ))

(define (extract-loop-counters state-for)
  (define (normalize-cond state-cond)
    ;; TODO
    ;; normalize cond like PGI compiler
    ;; for more flexibilty
    state-cond
    )

  ;; '((varname . op until) ...)
  (define (extract-cond-vars state-cond)
    ;; var (<=|<) expr
    (case (sxml:name state-cond)
      [(logLEExpr logLTExpr)
       (let1 c (sxml:content state-cond)
         (and (eq? (sxml:name (~ c 0)) 'Var)

              (list
               (list
                (sxml:car-content (~ c 0))
                (case (sxml:name state-cond)
                  [(logLEExpr) '<=]
                  [(logLTExpr) '<])
                (~ c 1)))
              ))]

      [else '()]
      ))

  ;; '((varname . step) ...)
  (define (extract-iter-vars state-iter)
    ;; i(++|--) | (++|--)i | i (+|-)= n
    ;; TODO
    ;; more flexibilty

    (define (inverse-sign constant)
      (list
       (sxml:name constant)
       (number->string (- (string->number (sxml:car-content constant))))))

    (let1 c (sxml:content state-iter)
      (case (sxml:name state-iter)
        [(postIncrExpr preIncrExpr postDecrExpr preDecrExpr)
         (match-let1 (var) c
           (or
            (and (eq? (sxml:name var) 'Var)

                 (list
                  (cons
                   (sxml:car-content var)
                   (case (sxml:name state-iter)
                     [(postIncrExpr preIncrExpr) (gen-int-expr 1)]
                     [(postDecrExpr preDecrExpr) (gen-int-expr -1)])
                   )))
            '()
            ))]

        [(asgPlusExpr asgMinusExpr)
         (match-let1 (var step) c
           (or
            (and (eq? (sxml:name var)  'Var)
                 (eq? (sxml:name step) 'intConstant)

                 (list
                  (list
                   (sxml:car-content var)
                   (case (sxml:name state-iter)
                     [(asgPlusExpr)  step]
                     [(asgMinusExpr) (inverse-sign step)])
                   )))
            '()
            ))]

        [else '()]
        )))

  ;; '((varname start) ...)
  (define (extract-init-vars state-init)
    ;; i = ...
    (let1 c (sxml:content state-init)
      (case (sxml:name state-init)
        [(assignExpr)
         (match-let1 (var start) c
           (or
            (and (eq? (sxml:name var) 'Var)

                 (list
                  (cons
                   (sxml:car-content var)
                   start)))
            '()))]
        )))

  (let* ([init ((if-ccc-sxpath "init")      state-for)]
         [cond ((if-ccc-sxpath "condition") state-for)]
         [iter ((if-ccc-sxpath "iter")      state-for)]
         [cond (and cond (map normalize-cond cond))]
         [init-vars (if init (extract-init-vars init) '())]
         [cond-vars (if cond (extract-cond-vars cond) '())]
         [iter-vars (if iter (extract-iter-vars iter) '())])

    (filter-map

     (^[ini]
       (and-let* ([varname (car ini)]
                  [start   (cdr ini)]

                  [c       (assoc-ref cond-vars varname)]
                  [op      (~ c 0)]
                  [until   (~ c 1)]

                  [step    (assoc-ref iter-vars varname)])

         (let* ([int? (lambda (e) (eq? (sxml:name e) 'intConstant))]
                [reducible (and (int? start) (int? until) (int? step))]

                [until (if reducible
                           (let* ([nc (.$ string->number sxml:car-content)]
                                  [start-n (nc start)]
                                  [until-n (nc until)]
                                  [step-n  (nc step)]
                                  [until-equal (eq? op '<=)]
                                  [width (- until-n start-n (if until-equal -1 0))]
                                  [new-until-n (+ (* (div width step-n) step-n) start-n)])

                             (gen-int-expr
                              (if (and (not until-equal) (= until-n new-until-n))
                                  (- new-until-n step-n) new-until-n)))

                           (if (eq? op '<=) until
                               `(minusExpr ,until ,(gen-int-expr 1))))]

                [op '<=])

           (list varname start until step op)
           )))

     init-vars)
    ))
