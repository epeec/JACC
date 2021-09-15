(define-module pass0
  (use util)
  (use xm)
  (use analysis)

  (use srfi-1)
  (use srfi-11)
  (use srfi-13)
  (use util.match)
  (use sxml.tools)
  (use sxml.sxpath)
  (use gauche.collection)

  (export
   pass0
   ))
(select-module pass0)

;;;
;;; PASS0
;;;
;;; Transform ACCPragma into the form supported by macc.
;;;
;;; - KERNELS_LOOP              => KERNELS  & LOOP
;;; - PARALLEL_LOOP             => PARALLEL & LOOP
;;;
;;; - (KERNELS|PARALLEL) COPYIN => DATA & (KERNELS|PARALLEL)
;;; - KERNELS                   => PARALLEL
;;;

(define-syntax define-clauses-separater
  (syntax-rules ()
    [(_ name pat)
     (define name
       (lambda (clauses)
         (let loop ([cl1 '(list)] [cl2 '(list)] [clauses (cdr clauses)])
           (if (null? clauses)
               (values (reverse cl1) (reverse cl2))

               (match-let1 (head . rest) clauses
                 (match1 ((ccc-sxpath "string") head) pat
                   (loop (cons head cl1) cl2 rest)
                   (loop cl1 (cons head cl2) rest)
                   ))))))]
    ))

(define-clauses-separater separate-acc-*-loop-clauses
  (or "ASYNC"
      "WAIT"
      "DEVICE_TYPE"
      "IF"
      "REDUCTION_PLUS"
      "REDUCTION_MINUS"
      "REDUCTION_MUL"
      "REDUCTION_BITAND"
      "REDUCTION_BITOR"
      "REDUCTION_BITXOR"
      "REDUCTION_LOGAND"
      "REDUCTION_LOGOR"
      "REDUCTION_MIN"
      "REDUCTION_MAX"
      "COPY"
      "COPYIN"
      "COPYOUT"
      "CREATE"
      "PRESENT"
      "PRESENT_OR_COPY"
      "PRESENT_OR_COPYIN"
      "PRESENT_OR_COPYOUT"
      "PRESENT_OR_CREATE"
      "DEVICEPTR"
      "PRIVATE"
      "FIRSTPRIVATE"
      "DEFAULT"
      "NUM_GANGS"
      "NUM_WORKERS"
      "VECT_LEN"
      "VECTOR_LENGTH"
      ))

(define (split-acc-*-loop! state)
  (match-let1 (('string dirname) clauses body) (sxml:content state)
    (let-values ([(cl1 cl2) (separate-acc-*-loop-clauses clauses)])
      (sxml:change-content! state
       `((string ,(string-drop-right dirname 5))
         ,cl1
         (ACCPragma
          (string "LOOP")
          ,cl2
          ,body)))
      )))

(define-clauses-separater separate-acc-data-clauses
  (or "COPY"
      "COPYIN"
      "COPYOUT"
      "CREATE"
      "DEVICEPTR"
      "PRESENT"
      "PRESENT_OR_COPY"
      "PRESENT_OR_COPYIN"
      "PRESENT_OR_COPYOUT"
      "PRESENT_OR_CREATE"))

(define (split-acc-data-clauses! state)
  (match-let1 (('string dirname) clauses body) (sxml:content state)
    (let-values ([(cl1 cl2) (separate-acc-data-clauses clauses)])
      (when (> (length cl1) 1)
        (sxml:change-content!
         state
         `((string "DATA")
           ,cl1
           (ACCPragma (string ,dirname) ,cl2 ,body)))
        ))))

(define (translate-acc-parallel! state)
  (attach-mark state))

(define (translate-acc-kernels! state)
  (sxml:change! state (translate-acc-kernels state)))

(define (translate-acc-kernels state :optional (clauses '(list)))
  (cond [(not (sxml:element? state)) state]

        [(eq? (sxml:name state) 'ACCPragma)
         (match-let1 (('string dirname) cl body) (sxml:content state)
           (if (equal? dirname "KERNELS")
               ;; KERNELS
               (translate-acc-kernels body (append clauses (cdr cl)))
               ;; LOOP
               (attach-parallel state clauses)))]

        [else
         (if (parallel-attachable? state)
             (attach-parallel state clauses)
             (sxml:change-content
              state
              (map (cut translate-acc-kernels <> clauses)
                   (sxml:content state))))]))

(define (parallel-attachable? state)
  (and (parallel-attachable-form? state)
       (parallel-attachable-dependency? state)))

(define (parallel-attachable-form? state)
  (let1 state-name (sxml:name state)
    (and (eq? state-name 'FdoStatement))
    #;(and (eq? state-name 'FdoStatement)

         (let1 body-content ((content-car-sxpath "body") state)
           (or (every have-no-loop? body-content)

               (and (= (length body-content) 1)
                    (let1 body-head (~ body-content 0)
                      (if (eq? (sxml:name body-head) 'ACCPragma) #t

                          (parallel-attachable-form? body-head))))
               )))
    ))

(define (have-no-loop? state)
  (null? 
   ((node-all
     (ntype-names?? '(FdoStatement FdoWhileStatement)))
    state)))

;; omit-loop is used for checking the existance of initialization only
;; (thus, no need to include cond)
(define (omit-loop index-flow)
  (define (rec flow) (omit-loop flow))

  (case (and (pair? index-flow) (car index-flow))
    [(loop)
     ;; also skip cond under loop
     (append-map rec (cddr (last index-flow)))]

    [(cond write read) (list index-flow)]

    [else (append-map rec index-flow)]))

(define (parallel-attachable-dependency? state)
  (and-let*
      ([(eq? (car state) 'FdoStatement)]
       [var ((if-ccc-sxpath "Var") state)]
       [index-flow (extract-index-flow state)]
       [index-flow (flatten-index-flow (omit-loop index-flow))]
       [(set-sequential-iterator! (list var))]
       [env (construct-env-dependency index-flow)]
       [reads (filter-map (^(x) (and (eq? (car x) 'read) (cddr x))) index-flow)]
       [writes (filter-map (^(x) (and (eq? (car x) 'read) (cddr x))) index-flow)]

       [i-ref
        (filter-map
         (match-lambda1 (access . reads)
           (and
            (= (length access) 1)
            (any
             (lambda (x) (equal? (revert-tag (car x)) var))
             reads)
            (revert-tag (car access))))
         env)]

       [i-ref (cons var i-ref)]

       [no-i-ref?
        (^(x) (null? (lset-intersection equal? ((sxpath '(// *text*)) (cons 'top x)) i-ref)))])

    ;; Detect loop-carried dependencies
    ;;   (Note: normal index-flow contains `cond` , therefore each alist in env has self-reference)
    ;;
    ;; Check whether an updated a[x] found in dependencies
    ;;  - i.g. a[i] = a[i-1]
    ;;    => variable or similar-access (but not the exactly same address)
    ;;       where iterator-based values are used except by itself
    ;;
    ;; OK:
    ;;  x = ...
    ;;  a[x] = ...
    ;;  a[i]++;
    ;;
    ;; TODO: when i-ref (variables derived from iterator) could cause random access
    ;;
    ;; Support this case by checking the position of initialization
    ;; (TODO: precise analysis of conditions, such as initialization in non-executed loops)
    ;;  for (i)
    ;;   for (j)
    ;;     a[x] +=..
    ;;     sum += ..
    ;;
    ;; TODO: (check in a reversive order like in mark-duplicated-statement )
    ;;  for (i)
    ;;     x = last;
    ;;     ..;
    ;;     last = i;
    ;;
    ;; TODO: also detect this case  (value updated in the last iteration)
    ;;
    ;;   for() {
    ;;    a[i]++;         // this case is ignored when i is in i-ref (iterator)
    ;;    a[100]=a[i]+1;  // self-reference from a[100] to a[100];
    ;;   }
    ;;

    (and
     ;; Check array writes with index of i-ref and check similar access in dependencies but not the same one
     (every
      (lambda (access)
       (or
        (no-i-ref? (cdr access))

        (every
         (lambda (r)
           (or
            (not (similar-address? r access))
            (every
             (^(a b) (or (no-i-ref? a)
                         (no-i-ref? b)
                         (expr-equal? a b)))
             r access)))
         reads)
        ))
      writes)

    ;; omit loop -> dependencies -> check self references (is there any initialization?)
    ;;  (except when the access has i-ref)
    (let* ([self-ref
            (map
             (match-lambda1 (access . reads)
              (cons
               (revert-tag (car access))
               (or
                ;; access containing i-ref
                (not (no-i-ref? (cdr access)))
                ;; every dependency is not self-referencing
                (every
                 (lambda (x)
                   (not (access-equal? access x)))
                 reads)
                )))
             env)]

           [col (group-collection self-ref :key car :test string=?)])
      
      (every
       (lambda (c)
         (find values (map cdr c)))
       col))
    )))

(define (attach-parallel state clauses)
  (let1 clauses (append clauses (collect-parallel-size! state))
    `(ACCPragma
      (@ (jacc_kernel "1"))
      (string "PARALLEL")
      ,clauses
      ,(attach-mark state))))

(define (attach-mark state)
  (for-each
   (lambda (d)
     (when (parallel-attachable-dependency? d)
       (sxml:add-attr! d '(jacc_loop "1"))
       ))
   ((node-all (ntype?? 'FdoStatement)) state))
  state)

(define (collect-parallel-size! state)
  (filter-map
   (lambda (c)
     (let ([content (sxml:content c)]
           [parname ((car-sxpath '(// string *text*)) c)])
       (and (= (length content) 2)

            (sxml:change-content! c `(,(~ content 0)))

            `(list
              (string
               ,(if (equal? parname "VECTOR") "VECT_LEN" #"NUM_~|parname|S"))
              ,(~ content 1))
            )))
   ((sxpath
     `(// (list (string *text* ,(make-sxpath-query #/^(GANG|WORKER|VECTOR)$/)))))
    state)))

(define-clauses-separater separate-acc-parallelism-clauses
  (or "NUM_GANGS"
      "NUM_WORKERS"
      "VECT_LEN"))

(define (pass0 xm)
  (rlet1 xm (xm-copy xm)
    (let* ([decls (xm-global-declarations xm)]
           [acc-trans!
            (lambda (proc pred?)
              (for-each proc ((sxpath `(// (ACCPragma (string *text* ,(make-sxpath-query pred?))))) decls)))])
      (acc-trans! split-acc-*-loop!           #/^(KERNELS|PARALLEL)_LOOP$/)
      (acc-trans! split-acc-data-clauses!     #/^(KERNELS|PARALLEL)$/)
      (acc-trans! translate-acc-parallel!     #/^PARALLEL$/)
      (acc-trans! translate-acc-kernels!      #/^KERNELS$/)
      )))
