;;      This file is part of JACC and is licenced under terms contained in the COPYING file
;;
;;      Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

#|

ACC ADJUSTOR

1) Transform acc for-loop in order to make it valid
---
#pragma acc parallel
{
    int i;
    for (i = 0; ...
---

 ||
 \/

---
#pragma acc parallel
for (int i = 0; ...
---

2) Transform pointer-ref for compile time checking
---
*(p + ...)
---

  ||
  \/

---
p[...]
---

|#

(use srfi-13)

;; return (var . type) or #f
(define (declaration? line)
  (and-let1 ma (rxmatch #/^(?<type>\w+) (?<var>\w+)\;$/ line)
    (cons (ma 'var) (ma 'type))))

(define (for? line)
  (rxmatch #/^for/ line))

(define (for-insert-declaration line env)
  (or
   (and-let1 ma (rxmatch #/^for\((?<var>\w+) (?<rest>.*)$/ line)
     (let1 var (ma 'var)
       (if-let1 type (assoc-ref env var)
         (string-append "for(" type " " var (ma 'rest))
         line)))
   line))

;; drop the first brace pair ('{' and '}')
(define (drop-brace-pair states)
  (and-let1 rest (and (string=? (car states) "{") (cdr states))
    (let loop ([braces 1] [rest rest] [ret '()] [state-num 0])
        (cond [(not (= braces 0))
               (let* ([head (car rest)]
                      [b (+ braces (count-braces head))])
                 (loop b (cdr rest) (cons head ret)
                       (+ state-num
                          (cond [(and (not (= braces b)) (= b 1)) 1] ; end of {}
                                [(declaration? head) 0] ; var decl

                                [(and (= braces b) (= b 1)
                                      (#/;/ (regexp-replace
                                             #/(#|\/\/).*$/ head ""))) 1] ; \;

                                [else 0]))))]

              [(not (= state-num 1)) states]

              [else (append (reverse (cdr ret)) rest)]))))

(define (adjust-acc-for states)
  (let loop ([ret '()] [states states] [env '()] [under-dir #f]) ; env := '((var . type) ...)
    (if (null? states) (reverse! ret)

        (let ([line (car states)] [rest (cdr states)])
          (cond [(rxmatch #/^\#pragma acc (kernels|parallel|loop)/ line)

                 (loop (cons line ret)
                       (if (string=? (car rest) "{")
                           (drop-brace-pair rest) rest)
                       env #t)]

                [(and under-dir (declaration? line))
                 => (^[bind] (loop ret rest (cons bind env) under-dir))]

                [(for? line)
                 (loop (cons (for-insert-declaration line env) ret)
                       rest env #f)]

                [else
                 (loop (cons line ret) rest env under-dir)]
                )))))

(define (count-braces str)
  (rlet1 ret 0
    (string-for-each
     (^[c]
       (cond [(char=? c #\{) (inc! ret)]
             [(char=? c #\}) (dec! ret)]))
     str)))

(define (replace-pointer-ref str)
  (or
   (and-let* ([ma    (#/^\*\(\((?<type>[^\)]*)\)\(\&\((?<expr>.*)\)\)\)$/
                      str)]
              [expr  (adjust-pointer-ref (ma 'expr))])
     #"*((~(ma 'type))(&(~expr)))")

   (and-let* ([ma    (#/^\*\(\((?<type>[^\)]*)\)\((?<expr>.*)\)\)$/ str)]
              [expr  (adjust-pointer-ref #"*(~(ma 'expr))")])
     #"*((~(ma 'type))(&(~expr)))")

   (and-let* ([ma    (#/^\*\((?<var>\w+) \+ (?<index>.*)\)$/ str)]
              [index (adjust-pointer-ref (ma 'index))])
     #"~(ma 'var)[~index]")

   (and-let* ([ma    (#/^\*\(\((?<expr>.*)\) \+ (?<index>.*)\)$/ str)]
              [index (adjust-pointer-ref (ma 'index))])
     #"~(replace-pointer-ref (ma 'expr))[~index]")

   str))

(define (adjust-pointer-ref str)
  (let loop ([ret '()] [stack '()] [lst (string->list str)] [lparams 0])
    (let1 inner (pair? stack)
      (cond [(and inner (= lparams 0))
             (loop (append
                    (reverse
                     (string->list
                      (replace-pointer-ref
                       (list->string
                        (reverse stack)))))
                    ret)
                   '() lst 0)]

            [(null? lst)
             (list->string (reverse (append stack ret)))]

            [else
             (let* ([head (car lst)] [rest (cdr lst)]
                    [lp (char=? #\( head)]
                    [rp (char=? #\) head)])

               (cond [(and lp inner)
                      (loop ret (cons #\( stack) rest (+ lparams 1))]

                     [(and lp (and (pair? ret) (char=? (car ret) #\*)))
                      (loop (cdr ret) (list #\( #\*) rest 1)]

                     [(and rp inner)
                      (loop ret (cons #\) stack) rest (- lparams 1))]

                     [inner
                      (loop ret (cons head stack) rest lparams)]

                     [else
                      (loop (cons head ret) '() rest 0)]))]
            ))))

(define (main args)
  (until (read-line) eof-object? => line

    (if (rxmatch #/^\#pragma acc (kernels|parallel)/ line)

        (let loop ([states (list line)] [braces (count-braces line)])
          (let1 line (read-line)

            (if (rxmatch #/^\#/ line)
                (loop (cons line states) braces)

                (let ([states (cons line states)]
                      [braces (+ braces (count-braces line))])

                  (if (= braces 0)
                      (for-each (.$ print adjust-pointer-ref)
                                (adjust-acc-for (reverse states)))

                      (loop states braces))))))

        (print (adjust-pointer-ref line))))
  0)
