;;;; test-display.scm
;;;;
;;;; Copyright (C) 2023 Michael Herstine <sp1ff@pobox.com>
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


(use-modules (scribbu))
(use-modules (oop goops))

(setlocale LC_ALL "en_US.UTF-8")


(define (main srcdir)
  "Exercise pretty-printing tags."
  (let ((tag (read-id3v1-tag (format #f "~a/data/elliot-goldenthal.id3v1.tag" srcdir))))
    (format (current-output-port) "~a\n" tag))
  (let ((tag (caar (read-tagset (format #f "~a/data/lorca.mp3" srcdir)))))
    (format (current-output-port) "~a\n" tag)))


(let ((cl (cdr (command-line))))
  (if (= 1 (length cl))
	    (main (car cl))
	    (begin
		    (format #t "Usage: test-display.scm ${srcdir}\n")
		    (exit 2))))
