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
        (ice-9 format)
        (srfi srfi-1))

(define-syntax-rule (read-first-line command)
  (let* ((port (open-input-pipe command))
         (res (read-line port)))
    (close-pipe port)
    res))

(define-syntax-rule (read-all-lines command)
  (let* ((port (open-input-pipe command))
         (res '()))
    (while (not (eof-object? (peek-char port)))
      (set! res (cons (read-line port) res)))
    (close-pipe port)
    (reverse (cdr res))))

(define (read-file-as-string filename)
  (let* ((port (open-input-file filename))
         (content (if (eof-object? (peek-char port)) "" (read-delimited "" port))))
    (close port)
    content))

(define (filename-extension filename)
  (string-append "." (car (take-right (string-split filename #\.) 1))))

(define (basename->title basename)
  (string-join
   (string-tokenize
    basename (char-set-complement (list->char-set
                                   (apply append
                                          (map char-set->list
                                               (list char-set:symbol char-set:punctuation char-set:iso-control char-set:blank))))))
   " "))

(define levenshtein
  (let ((cache '()))
    (λ (s t)
      (define key `(,s ,t))
      (or (and=> (assoc key cache) cdr)
          (let ((res
                 (let lev ((s (string->list s))
                           (sl (string-length s))
                           (t (string->list t))
                           (tl (string-length t)))
                   (cond ((zero? sl) tl)
                         ((zero? tl) sl)
                         (else
	                      (min (+ (lev (cdr s) (- sl 1) t tl) 1)
                               (+ (lev s sl (cdr t) (- tl 1)) 1)
                               (+ (lev (cdr s) (- sl 1) (cdr t) (- tl 1))
		                          (if (char=? (car s) (car t)) 0 1))))))))
            (set! cache (alist-cons key res cache))
            res)))))

(define (levenshtein-sort stream-files streamname)
  (display 'stream-files) 
  (write stream-files)
    (newline)

(define (convert-video filename)
  "Convert a video file to a freenet stream"
  (define name (basename filename))
  (define basename-without-extension (basename filename (filename-extension filename)))
  (define streamname (format #f "~a-stream.m3u" basename-without-extension))
  (define start 0)
  (define len 5)
  (define stop (+ start len))
  (define (step)
    (set! start (+ start len))
    (set! len (+ len 10))
    (set! stop (+ start len)))
  (define duration-seconds
    (inexact->exact
     (string->number (read-first-line (format #f "ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 ~a" filename)))))
  (define (ffmpeg index start stop)
    (when (and #false (< start duration-seconds)) ;; skip early when the video is finished.
      (close-pipe (open-input-pipe 
                   (format #f "ffmpeg -ss ~d -to ~d -accurate_seek -i ~a -y -g 360 -q:a 3 -q:v 3 -filter:v scale=640:-1 ~a-~3'0d.ogv"
                           start stop filename basename-without-extension index)))))
  ;; convert the video in segments
  (map (λ(x) (ffmpeg x start stop)(step)) (iota 999))
  (close-pipe (open-input-pipe (format #f "mplayer ~a -ss 5 -nosound -vf scale -zoom -xy 600 -vo jpeg:outdir=. -frames 1 && cp 00000001.jpg ~a.jpg" filename basename-without-extension)))
  ;; move the file to the current directory if needed
  (when (not (equal? name filename))
    (close-pipe (open-input-pipe (format #f "mv ~a ~a" filename name))))
  ;; create stream playlist that continues with random other playlists after finishing. This might benefit from heuristics like sorting later streams by similarity to the original stream
  (close-pipe (open-input-pipe (format #f "ls ~a-*ogv > ~a" basename-without-extension streamname)))
  (let*  ((stream-files (read-all-lines "ls *-stream.m3u"))
          (sorted (levenshtein-sort stream-files streamname)))
    (map (λ (filename)(format #f "echo '~a' >> ~a" filename streamname)) sorted)
    (list (cons 'filename filename)
          (cons 'basename name)
          (cons 'streamname streamname)
          (cons 'title (basename->title basename-without-extension)))))

(define (add-video next-video)
  (define next-video-metadata (convert-video next-video))
  (define entry-template (read-file-as-string "video.html"))
  (define next-entry
    (string-replace-substring
     (string-replace-substring
      (string-replace-substring
       (string-replace-substring
        entry-template "{{{TITLE}}}" (assoc-ref next-video-metadata 'title))
       "{{{M3ULINK}}}" (assoc-ref next-video-metadata 'streamname))
      "{{{FILELINK}}}" (assoc-ref next-video-metadata 'basename))
     "{{{FILENAME}}}" (assoc-ref next-video-metadata 'basename)))
  (let* ((port (open-output-file (string-append "../entries/" (assoc-ref next-video-metadata 'basename)))))
    (display next-entry port)
    (close port))
  (define template (read-file-as-string "template.html"))
  (define entry-filenames (map (λ (x) (string-append "../entries/" x)) (read-all-lines "ls --sort=time ../entries/")))
  (define entries (map read-file-as-string entry-filenames))
  (define replaced (string-replace-substring template "{{{STREAMS}}}" (string-join entries "\n\n")))
  (let* ((port (open-output-file "index.html")))
    (display replaced port)
    (close port))
  (display entry-filenames)
  (display next-video)
  (display replaced)
  (newline))

(define (main args)
  (define next-video (read-first-line "ls ../media/*.* | shuf | head -n 1"))
  (when (not (eof-object? next-video))
    (add-video next-video)))
