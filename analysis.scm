(define-module analysis
  (use util)
  (use xm)

  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (use sxml.tools)
  (use sxml.sxpath)
  (use gauche.collection)
  (use gauche.sequence)
  (use util.combinations)

  (export
   extract-innermost-parallel-region
   locate-split-dimention
   extract-dependency
   extract-conflict
   extract-index-flow
   flatten-index-flow
   similar-address?
   access-equal?
   expr-equal?
   set-sequential-iterator!
   construct-env-dependency
   mark-duplicated-statement!
   sort-expr
   add-tag
   revert-tag
   remove-tag
   select-array-index
   extract-varref-name
   ))
(select-module analysis)

(define (extract-parallel-content content)
  (let loop ([content content] [acc '()])
    (cond [(null? content)
           (and (= (length acc) 1)
                (car acc))]

          [(eq? (sxml:name (car content) ) 'ACCPragma)
           (loop (cdr content) (cons (car content) acc))]

          ;; No reference just under parallel/loop construct
          [(null? ((sxpath '(// FarrayRef)) (car content)))
           (loop (cdr content) acc)]

          [else #f])))

(define (extract-innermost-parallel-region state)
  (cond [(not (sxml:element? state)) state]

        [(eq? (sxml:name state) 'ACCPragma)
         (match-let1 (('string dirname) _ body) (sxml:content state)
           (let* ([body (if (and (eq? (sxml:name body) 'list)
                                 (= (length (sxml:content body)) 1))
                            ;; remove unncessary <list>
                            (sxml:car-content body) body)]
                  [name (sxml:name body)])
             (cond [(eq? name 'ACCPragma)
                    (extract-innermost-parallel-region body)]

                   [(eq? name 'FdoStatement)
                    (let1 content ((content-car-sxpath "body") body)
                      (if (not (= (length content) 1))
                          (extract-innermost-parallel-region
                           (or (extract-parallel-content content) body))
                          (extract-innermost-parallel-region (car content))))]

                   [(eq? name 'blockStatement)
                    (let1 content ((content-car-sxpath "body") body)
                      (if (not (= (length content) 1)) body
                          (extract-innermost-parallel-region (car content))))]

                   [else body]
                   )))]

        [(eq? (sxml:name state) 'FdoStatement)
         (let1 content ((content-car-sxpath "body") state)
            (cond [(not (sxml:attr state 'jacc_loop)) state]

                  [(not (= (length content) 1)) (cons 'body content)]

                  [else (extract-innermost-parallel-region (car content))]))]

        [else state]))

;; (array -> dimention)
(define (locate-split-dimention array-writes parallel-iterators sequential-iterators)
  (let*
      ([pad (+ (length array-writes) 1)]

       [access (map cddr
                 (filter cadr
                   (append-map
                    (.$ construct-index-flow sxml:car-content) array-writes)))]

       [parallelism
        (group-collection
         (map
          (lambda (a)
            (cons
             (revert-tag (car a))
             (map (^(i)
                    (let1 vars ((sxpath `(// ,(sxpath:name 'Var) *text*)) i)
                      (cond
                       [(null? vars) 0]

                       [(find (cut member <> sequential-iterators) vars) 0]

                       [(find (cut member <> parallel-iterators) vars) pad]

                       [else 1])))
                  (cdr a))))
          access)
         :key car :test string=?)]

       [parallelism-sum
        (map
         (^(p)
           (cons
            (caar p)
            (fold
             (^(e nil) (map + (cdr e) nil))
             (cdar p)
             (cdr p))))
         parallelism)])

    (map
     (^(ps)
       (cons
        (car ps)
        (let1 m (apply max (cdr ps))
          (find-index-tail (cut = m <>) (cdr ps)))))
     parallelism-sum)
    ))

(define deductibles '(plusExpr mulExpr minusExpr))

(define ITERATORS '())

(define (non-expr-equal-var-deduction-var? x y)
  (and
   (memq (car y) deductibles)
   (match-let1 (y1 y2) (sxml:content y)
     (and (expr-equal? x y1)
          (or (not (eq? (sxml:name x) 'Var))
              ;; sequential iterators
              (not (member (sxml:car-content x) ITERATORS)))
          (eq? (car y2) 'intConstant)
          (and
           (not (equal? (sxml:car-content y2) "0"))
           (or (not (eq? (car y) 'mulExpr))
               (not (equal? (sxml:car-content y2) "1"))))))))

(define (expr-equal? x y)
  (cond [(equal? x y) #t]

        [(and (eq? (car x) 'intConstant)
              (eq? (car y) 'intConstant)
              (not
               (equal? (sxml:car-content x)
                       (sxml:car-content y)))) #f]

        ;; FIXME: to be precise condition
        [(and (memq (car x) deductibles)
              (memq (car y) deductibles)
              (match-let ([(x1 x2) (sxml:content x)]
                          [(y1 y2) (sxml:content y)])
                  (and (expr-equal? x1 y1)
                       (not (expr-equal? x2 y2))))) #f]

        [(or (non-expr-equal-var-deduction-var? x y)
             (non-expr-equal-var-deduction-var? y x)) #f]

        [else #t]))

;; a__hash1 == a__hash2
(define (symbol-equal? a b)
  (equal? (revert-tag a) (revert-tag b)))

(define (access-equal? a b)
  (and (symbol-equal? (car a) (car b))
       (equal? (cdr a) (cdr b))))

(define (similar-address? a1 a2)
  (and
   (= (length a1) (length a2))
   (symbol-equal? (car a1) (car a2))
   (every expr-equal? (cdr a1) (cdr a2) )))

(define (state-equal? a b)
  (equal? (car a) (car b)))

(define (look-up-dominator access env)
  (or
   (any
    (lambda (e)
      (match-let1 (access2 . set) e
        (if (similar-address? access access2) set #f)))
    env)

   '()
   ))

(define (dig-all-writes flow env)
  (append-map
   (lambda (f)
     (case (car f)
       [(read) (look-up-dominator (cddr f) env)]

       [(write) (list (drop-right (cddr f) 1))]
       ))
   flow))

(define (dig-all-reads flow env)
  (append-map
   (lambda (f)
     (case (car f)
       [(read) (cons (cddr f) (look-up-dominator (cddr f) env))]

       [(write) '()]))
   flow))

;; Conflict:
;;   w[i] -> r[j] -> w[j]
;;   w[i] -> r[i] -> w[j]
;;   w[i] -> r[j] -> w[i]

(define (construct-env-conflict flow)
  (let loop ([flow flow] [env '()])
    (if (null? flow) env
        (let1 head (car flow)
          (case (car head)
            [(read)
             ;; w[*]->r[*] (only for the same array)
             (let* ([access (cddr head)]
                    [name (car access)]
                    [writes (filter (^(w) (symbol-equal? (car w) name))
                                    (look-up-dominator access env))]
                    [writes (append writes (assoc-ref env access '() access-equal?))]
                    [writes (delete-duplicates writes state-equal?)])
               (loop (cdr flow)
                     (acons access writes (alist-delete! access env))))]

            [(write)
             ;; w[*]->w[*]
             (let* ([args (cddr head)]
                    [access (drop-right args 1)]
                    [rv (last args)]
                    [env (loop rv env)]
                    [writes (dig-all-writes rv env)]
                    [writes (cons access writes)]
                    [writes (append writes (assoc-ref env access '() access-equal?))]
                    [writes (delete-duplicates writes state-equal?)])
               (loop (cdr flow) (acons access writes (alist-delete! access env))))]

            )))))


;; Dependency:
;;   r[*]->w[*]

(define (construct-env-dependency flow)
  (let loop ([flow flow] [env '()])
    (if (null? flow) env
        (let1 head (car flow)
          (case (car head)
            [(write)
             ;; r[*]->w[*]
             (let* ([args (cddr head)]
                    [access (drop-right args 1)]
                    [rv (last args)]
                    [reads (dig-all-reads rv env)]
                    [reads (delete-duplicates reads state-equal?)])
               (loop (cdr flow)
                     (acons access reads (alist-delete! access env))))]

            [(read) (loop (cdr flow) env)]

            )))))

(define (set-sequential-iterator! iter)
  (set! ITERATORS iter))

;; '((read -> write) ...)          (write could be variable. read be a array)
;; i.e. => { x = a[i]; b[i] = x; } (write-b needs read-a)
;;
;; variable write would be used for duplicating reduction or index calculation
(define (extract-dependency state iter)
  (set! ITERATORS iter)
  (let* ([innermost (extract-innermost-parallel-region state)]
         [index-flow (extract-index-flow innermost)]
         [index-flow (flatten-index-flow index-flow)]
         [env (construct-env-dependency index-flow)])

    (append-map
     (match-lambda1 (access . reads)
      ;; (if (= (length access) 1)
      ;;     '() ;; Variable
      (filter-map
       (lambda (x) (and (> (length x) 1) (cons x access)))
       reads
       ))
      ;; )
     env)))

;; '((write-a -> write-b) ...
;; i.e. =>  { a = ..; b = a; }   (write-b needs write-a)
(define (extract-conflict state iter)
  (set! ITERATORS iter)
  (let* ([innermost (extract-innermost-parallel-region state)]
         [index-flow (extract-index-flow innermost)]
         [index-flow (flatten-index-flow index-flow)]
         ;; ((write->write) ..)
         [env (construct-env-conflict index-flow)])

    (append-map
     (match-lambda1 (access . writes)
      (filter-map
       (lambda (x) (and (not (access-equal? access x))
                        (cons x access)))
       writes))
     env)))

(define (remove-tag access)
  (cons
   (revert-tag (car access))
   (cdr access)))

(define (extract-index-flow state)
  (construct-index-flow state))

(define (flatten-index-flow index-flow :optional (existential-quantifier #f) (no-loop #f))
  (define (rec flow) (flatten-index-flow flow existential-quantifier no-loop))
  (case (and (pair? index-flow) (car index-flow))
    [(loop)
     (let* ([body (append-map
                   (cut flatten-index-flow <> existential-quantifier #t)
                   (cddr index-flow))]
            [body (if no-loop body (append body body))])
       (if existential-quantifier
           (let1 iterators (cadr index-flow)
             (append (map (cut list 'def <>) iterators)
                     body
                     (map (cut list 'undef <>) iterators)))
           body))]

    [(cond)
     (let1 cond-read (append-map rec (cadr index-flow))
       (map
        (^(x)
          ;; SSA is disabled. To save the state before `cond`,
          ;; write in `cond` has to refer itself
          (if (not (equal? (car x) 'write)) x
              (let* ([expr (cadr x)] [access (drop-right (cddr x) 1)])
                `(write ,expr ,@access
                   ,(append (last (cdr x)) cond-read `((read #f ,@access)) )))))
        (append-map rec (cddr index-flow))))]

    [(write read) (list index-flow)]

    [else (append-map rec index-flow)]))

(define (mark-duplicated-statement! state conflicts dependencies exception split-dimention)
  (let* ([innermost (extract-innermost-parallel-region state)]
         [index-flow (extract-index-flow innermost)]
         [index-flow (flatten-index-flow index-flow #t)]

         [extract-index-var
          (^(addr)
            (let1 s (assoc-ref split-dimention (revert-tag (car addr)) #f)
              (if (not s) '()
                  (let loop ([x (~ addr (+ s 1))])
                    (cond [(not (pair? x)) '()]
                          [(eq? (car x) 'Var) (list (sxml:car-content x))]
                          [else (append-map loop x)])))))]

         [rflow (reverse index-flow)]

         ;; Add variables in index of write address to the list for duplication
         ;; FIXME: Keep least statements duplicated
         ;;        by creating the read-in-index->write relation
         ;;        (conflicts and dependencies have relations from writes,
         ;;         hence we cannot dig the writes from reads which do not affect
         ;;         those relations)
            ;; Tagged excluded statements:
            ;; ;; last write to exception
            ;; (map
            ;;  (^(e)
            ;;    (or
            ;;     (any (^(x) (and (eq? (car x) 'write)
            ;;                     (equal? (revert-tag (caddr x)) e)
            ;;                     (cadr x)))
            ;;          rflow) '()))
            ;;  exception)
         [exception
          (delete-duplicates
           (append
            exception
            (append-map
             (^(x)
               (let loop ([x (cddr x)])
                 (cond [(not (pair? x)) '()]
                       [(eq? (car x) 'Var) (list (sxml:car-content x))]
                       [else (append-map loop x)])))
             dependencies)))]

         ;; dup-write := '(access ...)
         [dup-write
          (filter-map
           (^(x) (and (member (revert-tag (cadr x)) exception) (car x)))
           conflicts)]

         ;; dup-read := '(access ...)
         [dup-read
          (filter-map
           (^(x) (and (member (revert-tag (cadr x)) exception) (car x)))
           dependencies)])

    (define (mark! expr)
      (when expr
        (sxml:add-attr! expr '(jacc_dup "1"))))

    ;; `updated` contains updated variables which cannot be referred anymore
    ;;
    ;;     tmp = (x <= ub && x >= lb) ? a[k] : 0; // this x is invalid (x is updated in the reversed order)
    ;;     x = 1
    ;;     (x <= ub && x >= lb) ? b[x] = 0 : 0;
    ;;
    (let loop ([rflow rflow] [iterators '()] [updated '()])

      (unless (null? rflow)
        (let1 head (car rflow)
          (case (car head)
            [(def)
             ;; OLD TODO: x[i]=x[i-1] in loop => this causes no problem if i is parallel
             (loop (cdr rflow)
                   (delete (cadr head) iterators equal?)
                   (cons (cadr head) updated))]

            [(undef)
             (loop (cdr rflow) (cons (cadr head) iterators) updated)]

            [(read)
             (let* ([expr (cadr head)]
                    [access (cddr head)])

               (when (member access dup-read)
                 ;; mark
                 (mark! expr))

               ;; when split-index has variables in updated
               (when (any (^(c) (and (pair? updated)
                                     (state-equal? access (car c))
                                     (any (^(v) (member v updated)) (extract-index-var (cdr c))) ))
                          dependencies)
                 (mark! expr))

               (loop (cdr rflow) iterators updated))]

            [(write)
             (let* ([expr (cadr head)]
                    [access (drop-right (cddr head) 1)]
                    [dup (member access dup-write)]
                    [name (car access)])

               (when dup
                 ;; mark
                 (mark! expr))

               ;; when split-index has variables in updated
               (when (any (^(c) (and (pair? updated)
                                     (state-equal? access (car c))
                                     (any (^(v) (member v updated)) (extract-index-var (cdr c))) ))
                          conflicts)
                 (mark! expr))

               ;; mark reads inside write
               (let loop ([deps (if (> (length access) 2) (last head) '())])
                 (unless (null? deps)
                   (let1 rh (car deps)
                     (when (eq? (car rh) 'read)
                       (mark! (cadr rh)))
                     (loop (cdr deps))
                     )))

               (loop
                (cdr rflow)

                iterators

                (let1 x (revert-tag name)
                  (if (and (= (length access) 1) (not (member x iterators)))
                      (cons x updated)
                      updated))
                ))]
            ))))))

(define (add-tag str state)
  #"~|str|__tag__~(eq-hash state)")

(define (revert-tag varname)
  #;((#/^(.*)__tag__\d+$/ varname) 1)
  ;; faster version
  (substring varname 0 (string-scan varname "__tag__")))

(define (select-array-index name)
  (lambda (x)
    (if (pair? (sxml:content x))
        (sxml:car-content x)
        `(Var ,name))))

(define (extract-varref-name varref)
  (sxml:car-content (sxml:car-content varref)))

;;;
;;; Construct the flow index '(loop (loop (write i j k) ...) (loop ...))
;;;
(define (construct-index-flow state)
  (define (%add-tag str)
    (add-tag str state))

  (case (sxml:name state)
    [(ACCPragma)
     (construct-index-flow (~ (sxml:content state) 2))]

    [(blockStatement)
     (append
      (append-map
       (match-lambda
        [(('name name) ('value value))
         `((write #f ,(%add-tag name) ,(append-map construct-index-flow value)))]
        [elses '()])
       ((sxpath "declarations/varDecl") state))
      (append-map construct-index-flow ((content-car-sxpath "body") state)))]

    [(FdoStatement)
     (let1 pred (construct-index-flow ((car-sxpath "indexRange") state))
       `((loop
          ,((sxpath `(Var *text*)) state)
          ,@pred
          (cond
           (,@pred)
           ,@(append-map construct-index-flow ((content-car-sxpath "body") state))
           ))))]

    [(indexRange)
     (append-map
      construct-index-flow
      (append
       ((content-car-sxpath "lowerBound") state)
       ((content-car-sxpath "upperBound") state)
       ((content-car-sxpath "step") state)))]

    [(value)
     (construct-index-flow ((sxml:content state)))]

    [(FdoWhileStatement)
     `((loop
        ()
       ,@(append-map
          construct-index-flow
          (append ((content-car-sxpath "condition") state)
                  ((content-car-sxpath "body")      state)))))]

    [(FselectCaseStatement)
     (append
      (construct-index-flow ((content-car-sxpath "value") state))
      (append-map construct-index-flow ((sxpath "FcaseLabel") state)))]

    [(FcaseLabel)
     (append-map
      construct-index-flow
      (append
       ((sxpath "value") state)
       ((sxpath "indexRange") state)
       ((content-car-sxpath "body"))))]

    [(FifStatement)
     (let1 c (map cadr (sxml:content state))
       `((cond
          ( ,@(construct-index-flow (car c)) )
          ,@(append-map construct-index-flow (cdr c)))))]

    [(then else body list)
     (append-map construct-index-flow (sxml:content state))]

    [(exprStatement)
     (construct-index-flow (sxml:car-content state))]

    [(functionCall)
     (append-map construct-index-flow
                 ((content-car-sxpath "arguments") state))]

    [(FassignStatement)
     (match-let* ([(lv rv) (sxml:content state)]
                  [rv-flow (construct-index-flow rv)])
       (append

        (case (sxml:name lv)
          [(Var)
           (append rv-flow
                   `((write #f ,(add-tag (sxml:car-content lv) lv) ,rv-flow)))]

          [(Var memberRef) '()]

          [(FarrayRef)
           (let* ([c (sxml:content lv)] [name (extract-varref-name (car c))])
             (append
              #;(append-map construct-index-flow (cdr c))

              `((write ,state
                 ,(add-tag name lv)
                 ,@(map (.$ sort-expr (select-array-index name)) (cdr c))
                 ,rv-flow ; no need to add index-flow of lv's indices, which are duplicated
                 ))))]
          )))]

    [(FarrayRef)
     (let* ([c (sxml:content state)] [name (extract-varref-name (car c))])
       (append
        (append-map (.$ construct-index-flow (select-array-index name)) (cdr c))

        `((read ,state
           ,(%add-tag name)
           ,@(map (.$ sort-expr (select-array-index name)) (cdr c))))))]

    [(plusExpr minusExpr mulExpr divExpr FpowerExpr FconcatExpr
      modExpr LshiftExpr RshiftExpr bitAndExpr bitOrExpr bitXorExpr
      logEQExpr logNEQExpr logGEExpr logGTExpr logLEExpr logLTExpr
      logAndExpr logOrExpr logEQVExpr logNEQVExpr unaryMinusExpr logNotExpr
      lowerBound upperBound step)
     (append-map construct-index-flow (sxml:content state))]

    [(Var varRef)
     `((read #f ,(%add-tag (sxml:car-content state) )))]

    [(FintConstant FrealConstant FcharacterConstant
      FlogicalConstant FcomplexConstant namedValue FcycleStatement FexitStatement
      varRef FarrayRef FcharacterRef statementLabel continueStatement)
     '()]

    [else (error #"Unknown statement: ~(sxml:name state)")]
    ))

(define (sort-expr expr)
  (if (not (sxml:element? expr)) expr
      (let1 c (sxml:content expr)
        (case (sxml:name expr)
          [(minusExpr divExpr FpowerExpr FconcatExpr logEQExpr
            logNEQExpr logGEExpr logGTExpr logLEExpr logLTExpr logAndExpr
            logOrExpr logEQVExpr logNEQVExpr unaryMinusExpr logNotExpr)
           (sxml:change-content expr (map sort-expr c))]

          [(plusExpr mulExpr)
           (sxml:change-content expr (sort (map sort-expr c)))]

          [else (sxml:snip expr)]
          ))))
