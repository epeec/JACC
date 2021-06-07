;; 	This file is part of JACC and is licenced under terms contained in the COPYING file
;;	
;;	Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

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
   mark-duplicated-statement!
   sort-expr
   add-tag
   revert-tag
   remove-tag
   ))
(select-module analysis)

(define (extract-parallel-content content)
  (let loop ([content content] [acc '()])
    (cond [(null? content)
           (and (= (length acc) 1)
                (car acc))]

          [(eq? (sxml:name (car content) ) 'ACCPragma)
           (loop (cdr content) (cons (car content) acc))]

          [(null? ((sxpath '(// (or@ arrayRef pointerRef))) (car content)))
           (loop (cdr content) acc)]

          [else #f])))

(define (extract-innermost-parallel-region state)
  (cond [(not (sxml:element? state)) state]

        [(eq? (sxml:name state) 'ACCPragma)
         (match-let1 (('string dirname) _ body) (sxml:content state)
           (let1 name (sxml:name body)
             (cond [(eq? name 'ACCPragma)
                    (extract-innermost-parallel-region body)]

                   [(eq? name 'forStatement)
                    (let* ([compound ((car-sxpath "body/compoundStatement") body)]
                           [content ((content-car-sxpath "body") compound)])
                      (if (not (= (length content) 1))
                          (extract-innermost-parallel-region
                           (or (extract-parallel-content content) compound))
                          (extract-innermost-parallel-region (car content))))]

                   [(eq? name 'compoundStatement)
                    (let1 content ((content-car-sxpath "body") body)
                      (if (not (= (length content) 1)) body
                          (extract-innermost-parallel-region (car content))))]

                   [else body]
                   )))]

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
          (find-index (cut = m <>) (cdr ps)))))
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
                     (if (null? writes) env
                         (acons access writes (alist-delete! access env)))))]

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
                     (if (null? reads) env
                         (acons access reads (alist-delete! access env)))))]

            [(read) (loop (cdr flow) env)]

            )))))

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

(define (flatten-index-flow index-flow :optional (existential-quantifier #f))
  (define (rec flow) (flatten-index-flow flow existential-quantifier))
  (case (car index-flow)
    [(loop)
     (let* ([body (append-map rec (cddr index-flow))]
            [body (append body body)])
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

         ;; Add variables in index of write address
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

    ;; updated contains updated variables which cannot not be referred anymore
    (let loop ([rflow rflow] [iterators '()] [updated '()])

      (unless (null? rflow)
        (let1 head (car rflow)
          (case (car head)
            [(def)
             ;; TODO: x[i]=x[i-1] in loop
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

;;;
;;; Construct the flow index '(loop (loop (write i j k) ...) (loop ...))
;;;
(define (construct-index-flow state)
  (define (%add-tag str)
    (add-tag str state))

  (case (sxml:name state)
    [(ACCPragma)
     (construct-index-flow (~ (sxml:content state) 2))]

    [(compoundStatement)
     (append
      (append-map
       (match-lambda
        [(('name name) ('value value))
         `((write #f ,(%add-tag name) ,(append-map construct-index-flow value)))]
        [elses '()])
       ((sxpath "declarations/varDecl") state))
      (append-map construct-index-flow ((content-car-sxpath "body") state)))]

    [(forStatement)
     (let1 pred (append-map
                 construct-index-flow
                 (append ((content-car-sxpath "init") state)
                         ((content-car-sxpath "condition") state)
                         ((content-car-sxpath "iter") state)))
       `((loop
          ,((sxpath `(init assignExpr (* 1) ,(sxpath:name 'Var) *text*)) state)
          ,@pred
          (cond
           (,@pred)
           ,@(construct-index-flow ((car-sxpath "body/compoundStatement") state))))))]

    [(doStatement whileStatement)
     `((loop
        ()
       ,@(append-map
          construct-index-flow
          (append ((content-car-sxpath "condition") state)
                  ((content-car-sxpath "body")      state)))))]

    [(switchStatement)
     (append
      (construct-index-flow ((content-car-sxpath "value") state))
      (construct-index-flow ((content-car-sxpath "body")  state)))]

    [(ifStatement)
     (let1 c (map cadr (sxml:content state))
       `((cond
          ( ,@(construct-index-flow (car c)) )
          ,@(append-map construct-index-flow (cdr c)))))]

    [(exprStatement castExpr)
     (construct-index-flow (sxml:car-content state))]

    [(functionCall)
     (append-map construct-index-flow
                 ((content-car-sxpath "arguments") state))]

    ;; FIXME: Convert asg* before pass2 to be rewritten as assignExpr
    [(asgPlusExpr asgMinusExpr asgMulExpr asgDivExpr
      asgModExpr asgLshiftExpr asgRshiftExpr asgBitAndExpr
      asgBitOrExpr asgBitXorExpr)
     (match-let1 (var val) (sxml:content state)
       (construct-index-flow (gen-=-expr var (list 'plusExpr var val))))]

    [(assignExpr)
     (match-let* ([(lv rv) (sxml:content state)]
                  [rv-flow (construct-index-flow rv)])
       (append

        (case (sxml:name lv)
          [(Var) (append rv-flow `((write #f ,(add-tag (sxml:car-content lv) lv) ,rv-flow)))]

          [(Var memberRef) '()]

          [(arrayRef)
           (let1 c (sxml:content lv)
             (append
              ;(append-map construct-index-flow (cdr c))

              `((write ,state
                 ,(add-tag (sxml:car-content (car c)) lv)
                 ,@(map sort-expr (cdr c))
                 ,rv-flow
                 ))))]

          [(pointerRef)
           (let1 c (sxml:car-content lv)
             (cond [(and (eq? (sxml:name c) 'plusExpr)
                         (eq? (sxml:name (sxml:car-content c)) 'Var))
                    (let1 index (cadr (sxml:content c))
                      (append
                       ;(construct-index-flow index)
                       `((write ,state
                          ,(add-tag (sxml:car-content (sxml:car-content c)) lv)
                          ,(sort-expr index)
                          ,rv-flow ))))]

                   [else rv-flow]))]
          )))]

    [(arrayRef)
     (let1 c (sxml:content state)
       (append
        (append-map construct-index-flow (cdr c))

        `((read ,state
           ,(%add-tag (sxml:car-content (car c)))
           ,@(map sort-expr (cdr c))))))]

    [(pointerRef)
     (let1 c (sxml:car-content state)
       (cond [(and (eq? (sxml:name c) 'plusExpr)
                   (eq? (sxml:name (sxml:car-content c)) 'Var))
              (let1 index (cadr (sxml:content c))
                (append
                 (construct-index-flow index)
                 `((read ,state
                    ,(%add-tag (sxml:car-content (sxml:car-content c)))
                    ,(sort-expr index)))))]

             [else '()]))]

    [(plusExpr minusExpr mulExpr divExpr condExpr
      modExpr LshiftExpr RshiftExpr bitAndExpr bitOrExpr bitXorExpr
      logEQExpr logNEQExpr logGEExpr logGTExpr logLEExpr logLTExpr
      logAndExpr logOrExpr
      postIncrExpr postDecrExpr preIncrExpr preDecrExpr
      unaryMinusExpr bitNotExpr logNotExpr sizeOfExpr
      commaExpr commaExpr0 memberRef)
     (append-map construct-index-flow (sxml:content state))]

    [(Var varAddr)
     `((read #f ,(%add-tag (sxml:car-content state) )))]

    [(caseLabel defaultLabel breakStatement continueStatement
      intConstant longlongConstant floatConstant
      stringConstant moeConstant funcAddr arrayAddr)
     '()]

    [else (error #"Unknown statement: ~(sxml:name state)")]
    ))

(define (sort-expr expr)
  (if (not (sxml:element? expr)) expr
      (let1 c (sxml:content expr)
        (case (sxml:name expr)
          [(assignExpr)
           (sort-expr (~ c 1))]

          [(postIncrExpr postDecrExpr commaExpr0)
           (sort-expr (~ c 0))]

          [(commaExpr)
           (sort-expr (last c))]

          [(preIncrExpr preDecrExpr)
           `(,(if (eq? (sxml:name expr) preIncrExpr) plusExpr minusExpr)
             ,@(sort (list (sort-expr (~ c 0)) (gen-int-expr 1))))]

          [(castExpr functionCall condExpr minusExpr divExpr
            modExpr LshiftExpr RshiftExpr bitAndExpr bitOrExpr bitXorExpr
            logEQExpr logNEQExpr logGEExpr logGTExpr logLEExpr logLTExpr
            logAndExpr logOrExpr unaryMinusExpr bitNotExpr logNotExpr
            sizeOfExpr commaExpr commaExpr0 arrayRef pointerRef)
           (sxml:change-content expr (map sort-expr c))]

          [(plusExpr mulExpr)
           (sxml:change-content expr (sort (map sort-expr c)))]

          [(moeConstant) expr]

          [else (sxml:snip expr)]
          ))))
