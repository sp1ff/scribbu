;;;; scribbu.scm --- Scheme modules for scribbu
     
;;;;   Copyright (C) 2017 Michael Herstine <sp1ff@pobox.com>

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;; 

;;; Author: Michael Herstine <sp1ff@pobox.com>

;;; Homepage: https://www.github.com/sp1ff/scribbu

;;; Commentary:

;; This file contains Scheme code useful for customizing & automating
;; scribbu.

(define-module (scribbu)
  #:export (scribbu/flat-file-system-tree scribbu/file-system-tree-generator))

(use-modules (ice-9 ftw))
(use-modules (ice-9 match))
(use-modules (rnrs bytevectors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;							  utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://gist.github.com/sasaki-shigeo/5352496
(define *eof*
  (let ((port (open-input-string "")))
    (read port)))

;; flatten a list-of-lists
;; https://stackoverflow.com/questions/8387583/writing-flatten-method-in-scheme
(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;						  exported functions                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scribbu/strip-stat-from-file-system-tree parent entry)
  (match entry
    ((name stat) (string-append parent "/" name))
	((name stat children ...)
	 (map 
	  (lambda (x) 
		(scribbu/strip-stat-from-file-system-tree (string-append parent "/" name) x))
	  children))))

;; Walk the tree rooted at `root' and produce a flat list of regular files. This
;; implementation will maintain the entire list in memory.
(define (scribbu/flat-file-system-tree root)
  ;; make `root' absolute if it's not already...
  (let* ((full-root 
		  (if (absolute-file-name? root) root
			  (string-append (getcwd) "/" root)))
		 (parent (dirname full-root)))
	;; special case "/"
    (if (string=? parent "/")
		(set! parent ""))
	;; strip the stat fields, flatten the tree structure
	(flatten (scribbu/strip-stat-from-file-system-tree parent (file-system-tree full-root)))))

;; Produce a generator that will return every regular file in the tree at `root'
(define (scribbu/file-system-tree-generator root)
  (let ((root-port (opendir root)))
	(let  ((dir-stack (list (list root root-port))))
	  (lambda ()
		(let ((next #f))
		  (while (and (not (null? dir-stack)) (eq? next #f))
				 (let* ((here (car dir-stack))
						(stm (cadr here))
						(pth (car here))
						(entry (readdir stm))) ; may be *eof*
				   ;; (display entry) (newline)
				   (set! next
						 ;; Evaluates to either #f or the next entry
						 (while (not (eof-object? entry))
								(let* ((full (string-append pth "/" entry))
									   (type (stat:type (stat full))))
								  (if (and (not (string=? "." entry)) 
										   (not (string=? ".." entry)))
									  (if (eqv? 'directory type)
										  (let ((stm2 (opendir full)))
											(set! dir-stack 
												(append dir-stack (list (list full stm2)))))
										  (if (eqv? 'regular type)
											  (break full)))
									  ))
								(set! entry (readdir stm))))
				   (if next
					   next
					   (let ()
						 (closedir (cadr here))
						 (set! dir-stack (cdr dir-stack))))))
		  (if (not next)
			  *eof*
			  next))))))
