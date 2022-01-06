#!/usr/bin/env bash
# -*- scheme -*-
exec -a "$0" guile -L $(realpath $(dirname $0)) -e '(gms)' -c '' "$@"
;; !#

;;; gms.scm --- Guile Media Site - simple fake streaming via m3u playlists

;; Copyright (C) 2021 ArneBab
;; Copyright (C) 2021 Morrow_Singh@sJr0A3bn8e-NHUYydXFSyDDio~43O7m5fDBZUQj4lQY

;; Author: ArneBab and Morrow Singh

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


;; gms creates a video site. It selects random videos from ../media,
;; prepares them for video-on-demand, and creates an index.html and an
;; archive.html page with the videos. Each video is followed by random
;; selections of previous videos as with playlists on youtube.
;; It keeps at most 10 files to prevent endless growth of the site.

;; Requirements:
;; - run on a GNU/Linux machine
;; - Guile
;; - ffmpeg
;; - mplayer
;; - xargs
;; - grep
;; - sed

;; Usage:
;; - adjust style in template.html, video.html, audio.html, and style.css.
;; - put video and audio files into ../media/
;; - run ./gms.scm to grab a random file from ../media/ and create an index.html site with all the already converted videos ready for streaming (up to 10 videos stay available).
;; - upload the folder containing gms.scm
;; - automatic weekly update: install pyFreenet. Then install a crontab-line with freesitemgr using `crontab -e`:
;;   MINUTE HOUR * * WEEKDAY (sleep $((($RANDOM % 1800))) && cd path/to/site/ && nice ./gms.scm && ~/pyFreenet/freesitemgr --max-manifest-size=1500 update watch-36c3-incrementally)
;;   replace minute and hour with the insert time and weekday with the day of week (1 is monday). The random sleep provides some obfuscation.
;;
;; ./gms.scm --rebuild-only just creates the site without converting and adding a new video. Use it for experimenting.
;; ./gms.scm --recycle-removed puts removed files back into ../media/. Use it for infinite updates if you have more than 10 files.

;;;; recreate all m3u-forwards
;;; remove the alternate m3u-lists
;; for i in *-stream.m3u; do grep -v .m3u "$i" > tmp && mv tmp "$i"; done; rm -f playlists; SAVEIFS=$IFS; IFS=$(echo -en "\n\b"); for i in $(ls --sort=time -r ../entries/); do touch "${i%%.*}"*; tac playlists | guile -c '(import (ice-9 rdelim))(set! *random-state* (random-state-from-platform))(let loop ((line (read-line))) (unless (eof-object? line) (when (< (random 5) 3) (display line)(newline)) (loop (read-line))))' >> "${i%%.*}"*-stream.m3u; echo "${i%%.*}"*-stream.m3u >> playlists; sleep 1; done; IFS=$SAVEIFS; rm -f playlists



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

(define videos-on-first-page 1)
(define maximum-video-count 10)

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


(define (format-streamname filename)
  (format #f "~a-stream.m3u" (entry-basename filename)))

(define (entry-basename filename)
  (basename filename (filename-extension filename)))

(define (convert-video filename)
  "Convert a video file to a freenet stream"
  (define name (basename filename))
  (define basename-without-extension (entry-basename filename))
  (define streamname (format-streamname basename-without-extension))
  (define start 0)
  (define len 5) ;; less than 350k, so two files fit into the manifest and the first can be a fast SSK-splitfile (for access in following playlists).
  (define stop (+ start len))
  (define (step)
    (set! start (+ start len))
    ;; exponential increase with small linear component in manifest to minimize breaks.
    ;;  5  8 11 15 20 26 33 41 51 63 77 94 114 138 167 202 244 294 354 426 513
    (set! len (+ 2 (truncate (* len 6/5))))
    (set! stop (+ start len)))
  (define duration-seconds
    (inexact->exact
     (string->number (read-first-line (format #f "ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 \"~a\"" filename)))))
  (define (ffmpeg index start stop)
    (when (< start duration-seconds) ;; skip early when the video is finished.
      (close-pipe (open-input-pipe 
                   (format #f "ffmpeg -threads 4 -ss ~d -to ~d -accurate_seek -i \"~a\" -y -g 360 -b:v 400k  -b:a 56k -filter:v scale=720:-1 \"~a-~3'0d.ogv\""
                           start stop filename basename-without-extension index)))))
  ;; convert the video in segments
  (map (λ (x) (ffmpeg x start stop)(step)) (iota 999))
  (close-pipe (open-input-pipe (format #f "mplayer \"~a\" -ss 5 -nosound -vf scale -zoom -xy 600 -vo jpeg:outdir=. -frames 1 && cp 00000001.jpg \"~a.jpg\"" filename basename-without-extension)))
  ;; move the file to the current directory if needed
  (when (not (equal? name filename))
	(close-pipe (open-input-pipe (format #f "mv \"~a\" \"~a\"" filename name))))
  ;; create stream playlist that continues with random other playlists after finishing. This might benefit from heuristics like sorting later streams by similarity to the original stream. Skip one more than the ones on the index page: the first in the archive can fail because its first segment was in the manifest, so it will only be inserted one insert later.
  (close-pipe (open-input-pipe (format #f "(ls \"~a\"-*ogv; ls --sort=time *-stream.m3u | grep -v \"~a\" | tail +~a | guile -c '(import (ice-9 rdelim))(set! *random-state* (random-state-from-platform))(let loop ((line (read-line))) (unless (eof-object? line) (when (< (random 5) 3) (display line)(newline)) (loop (read-line))))') > \"~a\"" basename-without-extension streamname (+ 2 videos-on-first-page) streamname)))
  (entry-metadata filename))

(define (entry-metadata filename)
  (define name (basename filename))
  (define basename-without-extension (entry-basename filename))
  (define streamname (format-streamname basename-without-extension))
  (let ((first-three (read-all-lines (format #f "ls \"~a-\"*ogv | head -n 2" basename-without-extension))))
    (list (cons 'filename filename)
          (cons 'basename name)
          (cons 'first-chunk (first first-three))
          (cons 'second-chunk (second first-three))
          (cons 'streamname streamname)
          (cons 'title (basename->title basename-without-extension)))))

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

(define (create-video-entry next-video-metadata)
  (define entry-template (read-file-as-string "video.html"))
  (define next-entry
    (string-replace-substring
     (string-replace-substring
      (string-replace-substring
       (string-replace-substring
	(string-replace-substring
	 (string-replace-substring
	  entry-template "{{{TITLE}}}" (assoc-ref next-video-metadata 'title))
	 "{{{M3ULINK}}}" (assoc-ref next-video-metadata 'streamname))
	"{{{FIRSTCHUNK}}}" (assoc-ref next-video-metadata 'first-chunk))
       "{{{SECONDCHUNK}}}" (assoc-ref next-video-metadata 'second-chunk))
      "{{{FILELINK}}}" (assoc-ref next-video-metadata 'basename))
     "{{{FILENAME}}}" (assoc-ref next-video-metadata 'basename)))
  (let* ((port (open-output-file (string-append "../entries/" (assoc-ref next-video-metadata 'basename)))))
    (display next-entry port)
    (close port)))
  

(define (add-video next-video)
  (define next-video-metadata (convert-video next-video))
  (create-video-entry next-video-metadata)
  (display next-video)
  (newline)
  (sync))

(define (recycle-video video-file)
  (when (not (string-null? video-file))
    ;; move video back into media
    (read-first-line (format #f "mv '~a' ../media/" video-file))))

(define (remove-video video-file)
  (when (not (string-null? video-file))
    ;; delete stream and content files
    (let ((cmd (format #f "rm '~a' \"~a-\"[0-9][0-9][0-9]\".ogv\"" (format-streamname video-file) (entry-basename video-file))))
      (display cmd)
      (newline)
      (read-first-line cmd))
    ;; remove stream from other streams
    (read-first-line (format #f "ls --sort=time -r *-stream.m3u | xargs -I % bash -c 'grep -v \"~a\" \"%\" > ../tmpstream.m3u; mv ../tmpstream.m3u \"%\"'" (format-streamname video-file)))
    (read-first-line (format #f "rm '../entries/~a'" video-file))))

(define (create-site)
  (define template (read-file-as-string "template.html"))
  (define entry-filenames (map (λ (x) (string-append "../entries/" x)) (read-all-lines (format #f "ls --sort=time ../entries/ | head -n ~a" maximum-video-count))))
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

(define (help args)
  (format #t "~a [--help | --rebuild-only | --create-entry <video-filename>] [--recycle-removed]\n" (car args)))

(define (main args)
  (define next-video (read-first-line "ls ../media/*.* | shuf | head -n 1"))
  (define help? (member "--help" args))
  (define rebuild-only? (member "--rebuild-only" args))
  (define create-entry? (member "--create-entry" args))
  (define recycle-removed-media? (member "--recycle-removed" args))
  (cond
   (help? (help args))
   (create-entry?
    (if (null? (cdr create-entry?))
	(help args)
	(create-video-entry (entry-metadata (second create-entry?)))))
   ((not rebuild-only?)
    (when (not (eof-object? next-video))
      ;; recycle before removing the old videos
      (when recycle-removed-media?
        (let ((old-files (read-all-lines (format #f "ls --sort=time ../entries/ | tail -n +~a" (+ 1 maximum-video-count)))))
          (map recycle-video old-files)))
      ;; remove old videos before adding the new
      (map remove-video (read-all-lines (format #f "ls --sort=time ../entries/ | tail -n +~a" (+ 1 maximum-video-count))))
      ;; create and add new video
	  (add-video next-video))
    (create-site))
   (else
    (create-site))))
