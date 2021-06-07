;;      This file is part of JACC and is licenced under terms contained in the COPYING file
;;
;;      Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

(define-module pass0
  (use util)
  (use xm)

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
    (and (or (eq? state-name 'forStatement)
             (eq? state-name 'compoundStatement))

         (let1 body-content ((content-car-sxpath "body") state)
           (or (every have-no-loop? body-content)

               (and (= (length body-content) 1)
                    (let1 body-head (~ body-content 0)
                      (if (eq? (sxml:name body-head) 'ACCPragma) #t

                          (parallel-attachable-form? body-head))))
               )))))

(define (have-no-loop? state)
  (null? 
   ((node-all
     (ntype-names?? '(forStatement doStatement whileStatement functionCall)))
    state)))

(define (parallel-attachable-dependency? state)
  (and-let*
      ([for-states ((node-all (ntype?? 'forStatement)) state)]
       [(pair? for-states)]

       ;; Each forStatement must have valid iteration
       [(every (.$ pair? extract-loop-counters) for-states)]

       ;; state of innermost forStatement
       [innermost-body ((car-sxpath "body") (last for-states))]

       [assigns ((node-all (ntype?? 'assignExpr)) innermost-body)]
       [arrayrefs ((node-all (ntype?? 'arrayRef)) innermost-body)]

       [group-indexes
        (lambda (indexes)
          (map
           (lambda (k) (cons (caar k) (map cdr k)))
           (group-collection indexes :key car :test string=?)))]

       [indexes-set
        (group-indexes
         (map
          (lambda (ar)
            (match-let1 (addr index) (sxml:content ar)
              (cons (sxml:car-content addr) index)))
          arrayrefs))]

       [all-write-indexes
        (map
         (lambda (as)
           (match-let1 (lv _) (sxml:content as)
             (and
              (eq? (sxml:name lv) 'arrayRef)
              (match-let1 (addr index) (sxml:content lv)
                (cons (sxml:car-content addr) index)))))
         assigns)]

       ;; All assignments must be to array elements
       ;; TODO: iterative data-flow analysis for more accuracy
       [(every values all-write-indexes)]

       [write-indexes-set (group-indexes all-write-indexes)])

    (every
     (lambda (indexes)
       (match-let1 (var . vals) indexes
         (let1 write-indexes-vals (assoc-ref write-indexes-set var)
           (or
            ;; read only
            (not write-indexes-vals)

            ;; write only
            (= (length vals) (length write-indexes-vals))

            ;; all vals are same
            ;; TODO: normalize expression of vals
            (= (length (delete-duplicates vals)) 1)
            ))))
     indexes-set)
    ))

(define (attach-parallel state clauses)
  (let1 clauses (append clauses (collect-parallel-size! state))
    `(ACCPragma
      (string "PARALLEL")
      ,clauses
      ,state)))

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

(define (remove-parallelism-clauses! state)
  (match-let1 (_ clauses _) (sxml:content state)
    (let-values ([(_ new-clauses) (separate-acc-parallelism-clauses clauses)])
      (sxml:change-content! clauses (cdr new-clauses))
      )))

(define (divide-gangs-by-numgpus! state)
  (map
   (lambda (c)
     (let1 content (sxml:content c)
       (sxml:change-content!
        c
        `(,(~ content 0)
          (condExpr
           (Var "__macc_multi")
           (divExpr
            (minusExpr
             (plusExpr ,(~ content 1) (Var "__MACC_NUMGPUS"))
             (intConstant "1"))
            (Var "__MACC_NUMGPUS"))
           ,(~ content 1)))
        )))
   ((sxpath
     `(// (list (string *text* ,(make-sxpath-query #/^NUM_GANGS$/)))))
    state)))

(define (pass0 xm)
  (rlet1 xm (xm-copy xm)
    (let* ([decls (xm-global-declarations xm)]
           [acc-trans!
            (lambda (proc pred?)
              (for-each proc ((sxpath `(// (ACCPragma (string *text* ,(make-sxpath-query pred?))))) decls)))])
      (acc-trans! split-acc-*-loop!           #/^(KERNELS|PARALLEL)_LOOP$/)
      (acc-trans! split-acc-data-clauses!     #/^(KERNELS|PARALLEL)$/)
      (acc-trans! translate-acc-kernels!      #/^KERNELS$/)
      )))
