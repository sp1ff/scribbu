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
(use-modules (oop goops))
(use-modules (scribbu))

(setlocale LC_ALL "")

(define (main file)
  "Cleanup a file output by Audacity"
  (let ((cp "test-audacity.tag")
        (bu "test-audacity.tag.1"))
    (copy-file file cp)
    (chmod cp #o664)
    (let* ((tag (caar (read-tagset cp)))
           (title (car (get-frames tag 'title-frame)))
           (track (car (get-frames tag 'track-frame)))
           (new-frames
            (list
             (make <text-frame> #:id 'artist-frame #:text "Rock the Joint")
             (make <text-frame> #:id 'album-frame  #:text "Live from Limrick, Ireland")
             (make <text-frame> #:id 'encoded-by-frame #:text "Audacity")
             title
             (make <text-frame> #:id 'track-frame
                   #:text (string-join (list (slot-ref track 'text) "14") "/"))
             (make <text-frame> #:id 'genre-frame #:text "Celtic")
             (make <comment-frame> #:id 'comment-frame
                   #:text "Ripped by StreamRipper from liveireland.com, cut into tracks by Audacity, tagged by scribbu")
             (make <user-defined-text-frame> #:id 'udt-frame #:dsc "sp1ff@pobox.com"
                   #:text "tags=live&sub-genres=rock,punk")))
           (new-tag (make <id3v2-tag> #:frames new-frames #:padding 1024)))
      (write-tagset (list (list new-tag 3)) cp))
    (delete-file bu)))

(let ((cl (cdr (command-line))))
  (if (= 1 (length cl))
	    (main (car cl))
	    (begin
		    (format #t "Usage: test-cleanup-from-audacity.scm ${srcdir}/<file>\n")
		    (exit 2))))
