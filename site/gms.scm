#!/usr/bin/env bash
# -*- scheme -*-
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
;; - upload the folder containing gms.scm
;; - automatic weekly update: install pyFreenet. Then install a crontab-line with freesitemgr using `crontab -e`:
;;   MINUTE HOUR * * WEEKDAY (sleep $((($RANDOM % 1800))) && cd path/to/site/ && nice ./gms.scm && ~/pyFreenet/freesitemgr --max-manifest-size=1500 update watch-36c3-incrementally)
;;   replace minute and hour with the insert time and weekday with the day of week (1 is monday). The random sleep provides some obfuscation.
;;
;; ./gms.scm --rebuild just creates the site without converting and adding a new video. Use it for experimenting.


;; Approach:
;; - stream files are in $(basename filename).m3u and $(basename filename)-~03d.ogv to keep the structure simple.
;; - the content of all html files in ../entries is embedded in reverse temporal order (newest first).
;; - add older m3u's in random order to the current m3u so people can just watch all videos by letting the video play
;; - get the source with history at http://hg.sr.ht/~arnebab/guile-media-site

(define-module (gms) #:export (main))
(import (ice-9 popen)
        (ice-9 rdelim)
        (ice-9 optargs)
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
    (reverse res)))

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
    ;; compromise between linar increase for fast start and exponential increase for fewer breaks. 5 11 18 27 38 52 70 
    (set! len (+ len 5 (inexact->exact (truncate (/ len 4)))))
    (set! stop (+ start len)))
  (define duration-seconds
    (inexact->exact
     (string->number (read-first-line (format #f "ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 ~a" filename)))))
  (define (ffmpeg index start stop)
    (when (< start duration-seconds) ;; skip early when the video is finished.
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
  (close-pipe (open-input-pipe (format #f "(ls ~a-*ogv; ls *-stream.m3u | shuf) > ~a" basename-without-extension streamname)))
  (list (cons 'filename filename)
        (cons 'basename name)
        (cons 'streamname streamname)
        (cons 'title (basename->title basename-without-extension))))

;; Guile 2 compat
(define* (string-replace-substring s substr replacement #:optional (start 0) (end (string-length s)))
  "Replace every instance of substring in s by replacement."
  (let ((substr-length (string-length substr)))
    (if (zero? substr-length)
	(error "string-replace-substring: empty substr")
	(let loop
	    ((start start)
	     (pieces (list (substring s 0 start))))
	  (let ((idx (string-contains s substr start end)))
	    (if idx
		(loop (+ idx substr-length)
		      (cons* replacement
			     (substring s start idx)
			     pieces))
		(string-concatenate-reverse
		 (cons (substring s start)
		       pieces))))))))
	       
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
  (display next-video)
  (newline)
  (sync))

(define videos-on-first-page 2)

(define (create-site)
  (define template (read-file-as-string "template.html"))
  (define entry-filenames (map (λ (x) (string-append "../entries/" x)) (read-all-lines "ls --sort=time ../entries/*.*")))
  (define entries (map read-file-as-string entry-filenames))
  (define replaced-first (string-replace-substring template "{{{STREAMS}}}" (string-join (take entries (min videos-on-first-page (length entries))) "\n\n")))
  (define replaced (string-replace-substring template "{{{STREAMS}}}" (string-join (drop entries (min videos-on-first-page (length entries))) "\n\n")))
  (let* ((port (open-output-file "index.html")))
    (display replaced-first port)
    (close port))
  (let* ((port (open-output-file "archive.html")))
    (display replaced port)
    (close port))
  (display entry-filenames)
  (display replaced)
  (newline))

(define (main args)
  (define next-video (read-first-line "ls ../media/*.* | shuf | head -n 1"))
  (when (not (member "--rebuild" args))
    (when (not (eof-object? next-video))
      (add-video next-video)))
  (create-site))
