#!/usr/bin/env bash
exec -a "$0" guile -L $(realpath $(dirname $0)) -e '(gms)' -c '' "$@"
;; !#

(define-module (gms) #:export (main))
(import (ice-9 popen)
        (ice-9 rdelim))

(define-syntax-rule (read-first-line command)
  (let* ((port (open-input-pipe command))
         (res (read-line port)))
    (close-pipe port)
    res))

(define (main args)
  (display (read-first-line "ls"))
  (newline))
