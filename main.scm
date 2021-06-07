;;      This file is part of JACC and is licenced under terms contained in the COPYING file
;;
;;      Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

(use sxml.ssax)
(use sxml.serializer)

(add-load-path "." :relative)
(use xm)
(use pass0)
(use pass1)
(use pass2)

(define (macc xm)
  ((.$ pass2 pass1 pass0) xm))

(define (main args)
  (let* ([iport    (current-input-port)]
         [oport    (current-output-port)]
         [sxml-in  (ssax:xml->sxml iport '())]
         [xm-in    (sxml->xm (~ sxml-in 2))]
         [xm-out   (xm->sxml (macc xm-in))]
         [sxml-out (append (take sxml-in 2) (list xm-out))])
    (srl:sxml->xml sxml-out oport))
  0)
