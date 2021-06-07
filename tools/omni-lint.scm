;;      This file is part of JACC and is licenced under terms contained in the COPYING file
;;
;;      Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

#|

OMNI LINT

|#

(define (output line-num msg)
  (print #"~line-num : ~msg"))

(define (main args)
  (let1 line-num 0
    (until (read-line) eof-object? => line
      (inc! line-num)
      (cond [(#/SIZE_MAX/ line)
             (output line-num "Can't use SIZE_MAX")]
            )))
  0)
