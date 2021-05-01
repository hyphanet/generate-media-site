#!/usr/bin/env bash
exec -a "$0" guile -L $(realpath $(dirname $0)) -e '(gms)' -c '' "$@"
;; !#

;; Requirements:
;; - run on a GNU/Linux machine
;; - Guile
;; - ffmpeg
;; - mplayer

;; Usage:
;; - adjust style in template.html, video.html, audio.html, and style.css.
;; - put video and audio files into ../media/
;; - run ./gms.scm to grab a random file from ../media/ and create an index.html site with all the already converted audio ready for streaming.   

;; Plan:
;; - unprocessed media-files are in ../media/
;; - processed entries are in ../entries as ~03d.html
;; - stream files are in $(basename filename).m3u and $(basename filename)-~03d.ogv to keep the structure simple.
;; - new streams are inserted at the top.
;; - the currently processed video is in current-video/ so it can be resumed easily if conversion does not finish.
;; - the content of all html files in ../entries is embedded in reverse lexical order.
;; - keep the videos as full ogv encoded and add them shuffled to each m3u-list to give continuous random playback     of later entries.

(define-module (gms) #:export (main))
(import (ice-9 popen)
        (ice-9 rdelim)
        (ice-9 string-fun)
        (ice-9 ftw)
        (ice-9 format))

(define-syntax-rule (read-first-line command)
  (let* ((port (open-input-pipe command))
         (res (read-line port)))
    (close-pipe port)
    res))

(define (read-file-as-string filename)
  (let* ((port (open-input-file filename))
         (content (if (eof-object? (peek-char port)) "" (read-delimited "" port))))
    (close port)
    content))

(define (convert-video filename)
  (define basename (basename filename))
  (define start 0)
  (define len 5)
  (define stop (+ start len))
  (define (step)
    (set! start (+ start len))
    (set! len (+ len 10))
    (set! stop (+ start len)))
  (define (ffmpeg index start stop) 
    (close-pipe (open-input-pipe 
                 (format #f "ffmpeg -ss ~d -to ~d -accurate_seek -i ~a -y -g 360 -q:a 3 -q:v 3 -filter:v scale=640:-1 ~a-~3'0d.ogv"
                         start stop filename index filename))))
  (map (λ(x) (ffmpeg x start stop)(step)) (iota 27))
  (close-pipe (open-input-pipe (format #f "mplayer ~a -ss 5 -nosound -vf scale -zoom -xy 600 -vo jpeg:outdir=. -frames 1" filename)))
  (close-pipe (open-input-pipe (format #f "ls ~a-*ogv > ~a-stream.m3u" filename filename))))

(define (main args)
  (define next-video (read-first-line "ls ../media/*.* | shuf | head -n 1"))
  (define template (read-file-as-string "template.html"))
  (define entry-filenames (map (λ (x) (string-append "../entries/" x)) (reverse (sort (scandir "../entries/" (λ (x) (string-suffix? ".html" x))) string-ci<=)))) ;; TODO fill entries with video files
  (define entries (map read-file-as-string entry-filenames))
  (define replaced (string-replace-substring template "{{{STREAMS}}}" (string-join entries "\n")))
  (let* ((port (open-output-file "index.html")))
    (display replaced port)
    (close port))
  (display entry-filenames)
  (display next-video)
  (display replaced)
  (newline))
