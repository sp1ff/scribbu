;;;;
;;;; test.scm
;;;;
;;;; Copyright (C) 2015-2024 Michael Herstine <sp1ff@pobox.com>
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
(define test-dir "/tmp/scribbu-test/scheme")

(define G (scribbu/file-system-tree-generator test-dir))

(if (not (string=? (G) (string-append test-dir "/foo"))) (exit 1))
(if (not (string=? (G) (string-append test-dir "/a/bar"))) (exit 1))
(if (not (string=? (G) (string-append test-dir "/a/b/splat"))) (exit 1))

