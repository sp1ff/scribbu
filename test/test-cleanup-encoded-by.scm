;;;;
;;;; test-cleanup-encoded-by.scm
;;;;
;;;; Copyright (C) 2015-2022 Michael Herstine <sp1ff@pobox.com>
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
(use-modules (oop goops))

(setlocale LC_ALL "en_US.UTF-8")

(define (report-on-encoded-by tags pth v1)
  "Utility routine for evaluating the 'encoded by' attribute of TRACK.

This function is for testing & debugging purposes. It will simply print the track
path, the ID3v1 comment (if any) and the ID3v2 TENC frame (if any)."
  (format #t "~s: " pth)
  (if (null? v1)
	    (display "<no ID3v1 match> ")
	    (format #t "ID3v1 comment: ~s " (slot-ref v1 'comment)))
  (while (not (null? tags))
         (let ((tag (caar tags)))
           (format #t "tag ~a" tag)
           (if (has-frame? tag 'encoded-by-frame)
               (let ((tenc (slot-ref
                            (car (get-frames tag 'encoded-by-frame))
                            'text)))
                 (format #t ", tag encoded-by: ~s" tenc))))
         (set! tags (cdr tags)))
  (format #t "\n"))

(define (cleanup-encoded-by tags pth v1)
  "Clean-up the 'encoded-by' attribute of TRACK.

If TRACK does not have an ID3v1 comment field matching /.*winamp.*/,
do nothing.

Else, if TRACK has an ID3v2 tag without a TENC frame, add a TENC frame
of 'Winamp'.  If TRACK has no ID3v2 tag. create one with only a TENC
frame of 'Winamp'. Otherwise, print a warning consisting of the TENC
frames in the extant ID3v2 frames."

  (unless (null? v1)
	  (let ((r (make-regexp ".*winamp.*" regexp/icase)))
		  (if (regexp-exec r (slot-ref v1 'comment))
			    (begin
				    (if (eq? 0 (length tags))
					      (let* ((frames (list (make <text-frame>
                                       #:id 'encoded-by-frame
                                       #:text "Winamp")))
                       (tag (make <id3v2-tag> #:frames frames))
                       (out (string-join (list (basename pth) "out") ".")))
                  (format #t "~s gets new tag ~s => ~s\n" pth tag out)
                  (let ((tmp (slot-ref tag 'frames)))
                    (format #t "its frames are: ~s | ~s | ~s\n"
                            tmp
                            (car tmp)
                            (slot-ref (car tmp) 'text)))
                  (write-tagset (list (list tag 3)) out))
                (let ((x tags)
                      (i 0)
                      (encoders '()))
                  (while (not (null? x))
                         (let* ((tag (caar tags))
                                (enc (get-frames tag 'encoded-by-frame)))
                           (if (eq? 0 (length enc))
                               (begin
                                 (slot-set! tag 'frames
                                            (append (slot-ref tag 'frames)
                                                    (list
                                                     (make <id3v2-text-frame>
                                                       #:id 'encoded-by-frame
                                                       #:text "Winamp"))))
                                 (format #t "New frames: ~s"
                                         (slot-ref tag 'frames))
                                 (write-tagset
                                  (list (list tag 3))
                                  (string-join (list (basename pth)
                                                     (number->string i)
                                                     "out") ".")))
                               (set! encoders (cons (car enc) encoders))))
                         (set! x (cdr x))
                         (set! i (+ i 1)))
                  (if (eq? (length encoders) (length tags))
                      (format #t "~s: already encoded by ~s\n"
                              pth encoders)))))))))

(define (main dir)
  "Cleanup the 'Encoded By' attribute for all tags in DIR."
  (let ((data (string-join (list dir "data") "/" 'infix)))
    (display "Report:\n=======\n")
	  (with-track-in data report-on-encoded-by)
    (display "\nReport done.\n")
    (display "Cleanup:\n========\n")
	  (with-track-in data cleanup-encoded-by)))

(let ((cl (cdr (command-line))))
  (if (= 1 (length cl))
	    (main (car cl))
	    (begin
		    (format #t "Usage: test-cleanup-encoded-by.scm ${srcdir}\n")
		    (exit 2))))
