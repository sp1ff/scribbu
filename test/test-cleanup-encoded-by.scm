;;;;
;;;; test-cleanup-encoded-by.scm
;;;;
;;;; Copyright (C) 2015-2019 Michael Herstine <sp1ff@pobox.com>
;;;;
;;;; This file is part of scribbu.
;;;;
;;;; scribbu is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; scribbu is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with scribbu.  If not, see <http://www.gnu.org/licenses/>.;;;;
;;;;
;;;;


(use-modules (ice-9 format))
(use-modules (ice-9 regex))
(use-modules (scribbu))

(setlocale LC_ALL "")

(define (report-on-encoded-by track)
  "Utility routine for evaluating the 'encoded by' attribute of TRACK.

This function is for testing & debugging purposes. It will simply print the track
path, the ID3v1 comment (if any) and the ID3v2 TENC frame (if any)."
  (format #t "~s: " (scribbu/get-path track))
  (if (scribbu/has-id3v1-tag track)
	  (format #t "ID3v1 comment: ~s" (scribbu/get-id3v1-string track 'scribbu/comment))
	  (display "<no ID3v1 match>"))
  (let ((num-tags (scribbu/get-id3v2-tag-count track)))
	  (do ((i 0 (1+ i)))
		    ((>= i num-tags))
	    (if (< 0 (scribbu/has-frame track i 'scribbu/encoded-by))
		      (format #t ", tag ~d/encoded-by: ~s" i
				          (scribbu/get-frame track i 'scribbu/encoded-by)))))
  (format #t "\n"))

(define (cleanup-encoded-by track)
  "Clean-up the 'encoded-by' attribute of TRACK.

If TRACK does not have an ID3v1 comment field matching /.*winamp.*/,
do nothing.

Else, if TRACK has an ID3v2 tag without a TENC frame, add a TENC frame
of 'Winamp'.  If TRACK has no ID3v2 tag. create one with only a TENC
frame of 'Winamp'. Otherwise, print a warning consisting of the TENC
frames in the extant ID3v2 frames."

  (if (scribbu/has-id3v1-tag track)
	    (let ((r (make-regexp ".*winamp.*" regexp/icase)))
		    (if (regexp-exec r (scribbu/get-id3v1-string track 'scribbu/comment))
			      (begin
			        (let ((num-tags (scribbu/get-id3v2-tag-count track)))
				        (if (eq? num-tags 0)
					          (begin
					            (scribbu/make-id3v2-tag track 0)
					            (scribbu/set-frame track 0 'scribbu/encoded-by "Winamp")
					            (scribbu/write-id3v2-tag
					             track 0
					             (string-join (list (basename (scribbu/get-path track)) "out") ".")))
					          (let ((encoders '()))
					            (begin
						            (do ((i 0 (1+ i)))
							              ((>= i num-tags))
						              (if (scribbu/has-frame track i 'scribbu/encoded-by)
							                (set!
							                 encoders
							                 (cons (scribbu/get-frame track i 'scribbu/encoded-by) encoders))
							                (begin
							                  (scribbu/set-frame track i 'scribbu/encoded-by "Winamp")
							                  (scribbu/write-id3v2-tag
							                   track i
							                   (string-join (list
											                         (basename (scribbu/get-path track))
											                         (number->string i)
											                         "out") "."))))))
                      (if (eq? num-tags (length encoders))
                          (format #t "~s: already encoded by ~s\n"
                                  (scribbu/get-path track) encoders))))))))))

(define (main dir)
  "Cleanup the 'Encoded By' attribute for all tags in DIR."
  (let ((data (string-join (list dir "data") "/" 'infix)))
    (display "Report:\n=======\n")
	  (scribbu/with-track-in data report-on-encoded-by)
    (display "\n")
    (display "Cleanup:\n========\n")
	  (scribbu/with-track-in data cleanup-encoded-by)))

(let ((cl (cdr (command-line))))
  (if (= 1 (length cl))
	    (main (car cl))
	    (begin
		    (format #t "Usage: test-cleanup-encoded-by.scm ${srcdir}\n")
		    (exit 2))))
