;;;;
;;;; test-cleanup-from-audacity.scm
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

(define (main file)
  "Cleanup a file output by Audacity"
  (let ((track (scribbu/make-track file #:load-data #true)))
    (let ((frames (scribbu/get-frame track 0 (list 'scribbu/track 'scribbu/title))))
      (scribbu/make-id3v2-tag track 0 #:padding 1024)
      (scribbu/set-frame track 0 (list  (list 'scribbu/artist "Rock the Joint")
                                        (list 'scribbu/album "Live from Limrick, Ireland")
                                        (list 'scribbu/encoded-by "Audacity")
                                        (list 'scribbu/title (list-ref frames 1))
                                        (list 'scribbu/track (string-join
                                                              (list (list-ref frames 0) "14")
                                                              "/"))
                                        (list 'scribbu/content-type "Celtic")))
      (scribbu/add-id3v2-comment
       track 0
       "Ripped by StreamRipper from liveireland.com, cut into tracks by Audacity, tagged by scribbu"
       #:language "eng")
      (scribbu/add-user-defined-text
       track 0 "tags=live&sub-genres=rock,punk" #:description "sp1ff@pobox.com")
      (scribbu/delete-id3v2-tag track 1)
      (scribbu/write-track track "test.tag"))))

(let ((cl (cdr (command-line))))
  (if (= 1 (length cl))
	    (main (car cl))
	    (begin
		    (format #t "Usage: test-cleanup-from-audacity.scm ${srcdir}/<file>\n")
		    (exit 2))))
