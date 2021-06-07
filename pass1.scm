;;      This file is part of JACC and is licenced under terms contained in the COPYING file
;;
;;      Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

(define-module pass1
  (use util)
  (use xm)

  (use util.match)
  (use sxml.tools)
  (use sxml.sxpath)

  (export
   pass1
   ))
(select-module pass1)

;;;
;;; PASS1
;;;
;;; Set necessary informations
;;;
;;;  - Attach element_type and size attr to the vars on acc data clauses
;;;  - Attach present clause to acc for-loops
;;;

(define (create-type-ht xm)
  (rlet1 ht (make-hash-table 'string=?)
    (for-each
     (^[x] (hash-table-put! ht (sxml:attr x 'type) x))
     (sxml:content (xm-type-table xm))
     )))

(define (fetch-element-type decl)
  (sxml:attr
   decl
   (case (sxml:name decl)
     [(pointerType) 'ref]
     [(arrayType)   'element_type]
     [(basicType)   'name])))

(define (search-element-type ht type)
  (or (and-let* ([type]
                 [decl (ref ht type #f)]
                 [type (fetch-element-type decl)])
        (search-element-type ht type))
      type))

(define (search-array-size ht type)
  (and-let* ([type] [decl (ref ht type #f)])
    (let1 inner (search-array-size ht (fetch-element-type decl))
      (if (eq? (sxml:name decl) 'arrayType)
          (and-let1 s (sxml:attr decl 'array_size)
            (cons (if-let1 n (string->number s)
                    (gen-int-expr n)
                    (sxml:car-content (sxml:car-content decl)))
                  (or inner '())))

          inner
          ))))

(define (extract-global-env xm)
  (update-env '() (xm-global-symbols xm)))

(define (update-env env symbols)
  (append
   (map
    (^[x] (cons ((ccc-sxpath "name") x) (sxml:attr x 'type)))
    ((sxpath "id") symbols))
   env))

;; Fetch the type of var-name
(define (fetch-type env var-name)
  (assoc-ref env var-name #f))

(define (insert-size! type-ht env state)
  (for-each
   (lambda (c)
     (match1 (sxml:content c)
             (('string (or "COPY"
                           "COPYIN"
                           "COPYOUT"
                           "CREATE"
                           "PRESENT"
                           "PRESENT_OR_COPY"
                           "PRESENT_OR_COPYIN"
                           "PRESENT_OR_COPYOUT"
                           "PRESENT_OR_CREATE"

                           "HOST"
                           "SELF"
                           "DEVICE")) args)
       (sxml:change-content!
        args
        (map
         (lambda (x)
           (or
            (and-let* ([(eq? (sxml:name x) 'Var)]
                       [varname    (sxml:car-content x)]
                       [type       (fetch-type env varname)]
                       [array-size (search-array-size type-ht type)])
              `(list ,x
                     ,@(map (lambda (s) `(list ,(gen-int-expr 0) ,s))
                            array-size)))
            x))
         (sxml:content args)))))

   ((content-car-sxpath "list") state)))

(define (insert-element-type! type-ht env state)
  (let1 vars ((sxpath '(list list list list Var)) state)
    (for-each
     (^[v]
       (let* ([varname (sxml:car-content v)]
              [type    (fetch-type env varname)])
         (sxml:add-attr!
          v (list 'element_type (search-element-type type-ht type)))
         ))
     vars)))

(define (insert-variable-type! type-ht env state)
  (let1 vars ((sxpath '(list list list Var)) state)
    (for-each
     (^[v]
       (let* ([varname (sxml:car-content v)]
              [type    (fetch-type env varname)])
         (sxml:add-attr!
          v (list 'variable_type (search-element-type type-ht type)))
         ))
     vars)))

(define (insert-data-info-acc-data! type-ht env state)
  (insert-size!  type-ht env state)
  (insert-element-type! type-ht env state)
  (insert-variable-type! type-ht env state))

(define (insert-data-info! type-ht env state)
  (define (rec-multi! env ss)
    (for-each
     (cut rec! env <>)
     ss))

  (define (rec! env s)
    (case (sxml:name s)
      [(ACCPragma)
       (let1 c (sxml:content s)
         (match1 (sxml:car-content (~ c 0))
                 (or "DATA" "UPDATE" "ENTER_DATA" "EXIT_DATA")
           (insert-data-info-acc-data! type-ht env s))
         (when (>= (length c) 3) (rec! env (~ c 2))))]

      [(functionDefinition compoundStatement)
       (let1 env (update-env env ((car-sxpath "symbols") s))
         (rec-multi! env ((content-car-sxpath "body") s)))]

      [(doStatement whileStatement forStatement switchStatement)
       (rec-multi! env ((content-car-sxpath "body") s))]

      [(ifStatement)
       (rec-multi! env (map cadr (sxml:content s)))]
      ))

  (rec! env state))

(define (extract-acc-data-vars name state)
  (let1 query (if (equal? name "DEVICEPTR")
                  "list/list[string='DEVICEPTR']/list"
                  "list/list[not(string='DEVICEPTR')]/list")
    (append (map (^x `(list ,x)) ((sxpath #"~|query|/Var") state))
            ((sxpath #"~|query|/list[Var]") state))))

(define (make-*-clause name vars)
  `
  (list
   (string ,name)
   (list ,@vars)))

(define (insert-*-acc-parallel! name vars state)
  (unless (null? vars)
    (let1 clauses ((car-sxpath "list") state)
      (sxml:change-content!
       clauses
       (cons (make-*-clause name vars) (sxml:content clauses)))
      )))

(define (insert-*! name vars state)
  (define (insert-*-multi! v ss)
    (for-each
     (cut insert-*! name v <>)
     ss))

  (case (sxml:name state)
    [(ACCPragma)
     (let* ([cnt     (sxml:content state)]
            [dirname ((ccc-sxpath "string") state)]
            [vars (match1 dirname "DATA"
                    (append (extract-acc-data-vars name state) vars)
                    vars)]
            [dup?
             (lambda (x y)
               (let1 f (car-sxpath "Var/text()") (equal? (f x) (f y))))]
            [vars (delete-duplicates vars dup?)])
       (match1 dirname "PARALLEL"
         (insert-*-acc-parallel! name vars state))
       (when (>= (length cnt) 3)
         (insert-*! name vars (~ cnt 2))))]

    [(functionDefinition compoundStatement
      doStatement whileStatement forStatement switchStatement)
     (insert-*-multi! vars ((content-car-sxpath "body") state))]

    [(ifStatement)
     (insert-*-multi! vars (map cadr (sxml:content state)))]
    ))

(define (insert-present! vars state) (insert-*! "PRESENT" vars state))
(define (insert-deviceptr! vars state) (insert-*! "DEVICEPTR" vars state))

(define (pass1 xm)
  (rlet1 xm (xm-copy xm)
    (let ([type-ht (create-type-ht xm)]
          [env     (extract-global-env xm)]
          [defs    ((sxpath "functionDefinition")
                    (xm-global-declarations xm))])
      (for-each (cut insert-data-info! type-ht env <>) defs)
      (for-each (cut insert-present!   '() <>)         defs)
      (for-each (cut insert-deviceptr! '() <>)         defs)
      )))
