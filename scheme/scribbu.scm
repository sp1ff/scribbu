;;;; scribbu.scm --- Scheme modules for scribbu

;;;; Copyright (C) 2019-2022 Michael Herstine <sp1ff@pobox.com>

;;;; Author: Michael Herstine <sp1ff@pobox.com>

;;;; Homepage: https://www.github.com/sp1ff/scribbu

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
;;;; along with scribbu.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains Scheme code useful for customizing & automating
;; scribbu.

(define-module (scribbu)
  #:export (flat-file-system-tree fs-tree-generator
            <id3v1-tag> <id3v2-frame> <unk-frame> <text-frame>
            <comment-frame> <user-defined-text-frame> <play-count-frame>
            <pop-frame> <tag-cloud-frame> <id3v2-tag>
            unknown-frame album-frame artist-frame
            has-frame? get-frames))

(use-modules (ice-9 ftw))
(use-modules (ice-9 match))
(use-modules (rnrs bytevectors))
(use-modules (oop goops))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          utility functions                                ;;
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

(define (strip-stat-from-file-system-tree parent entry)
  (match entry
         ((name stat) (string-append parent "/" name))
	       ((name stat children ...)
	        (map
	         (lambda (x)
		         (strip-stat-from-file-system-tree
              (string-append parent "/" name) x))
	         children))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          exported functions                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (flat-file-system-tree root)
  "Walk the tree rooted at `root' and produce a flat list of regular
files. This implementation will maintain the entire list in memory."
  ;; make `root' absolute if it's not already...
  (let* ((full-root
		      (if (absolute-file-name? root) root
			        (string-append (getcwd) "/" root)))
		     (parent (dirname full-root)))
	  ;; special case "/"
    (if (string=? parent "/")
		    (set! parent ""))
	  ;; strip the stat fields, flatten the tree structure
	  (flatten (strip-stat-from-file-system-tree
              parent (file-system-tree full-root)))))

(define (fs-tree-generator root)
  "Produce a generator that will return every regular file in the tree
at `root'"
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
												                          (append dir-stack
                                                          (list (list full
                                                                      stm2)))))
										                      (if (eqv? 'regular type)
											                        (break full)))))
								                (set! entry (readdir stm))))
				           (if next
					             next
					             (let ()
						             (closedir (cadr here))
						             (set! dir-stack (cdr dir-stack))))))
		      (if (not next)
			        *eof*
			        next))))))

(define-class <id3v1-tag> ()
  (title      #:init-value ""  #:accessor title      #:init-keyword #:title)
  (artist     #:init-value ""  #:accessor artist     #:init-keyword #:artist)
  (album      #:init-value ""  #:accessor album      #:init-keyword #:album)
  (year       #:init-value '() #:accessor year       #:init-keyword #:year)
  (comment    #:init-value ""  #:accessor comment    #:init-keyword #:comment)
  (genre      #:init-value 255 #:accessor genre      #:init-keyword #:genre)
  (track-no   #:init-value '() #:accessor track-no   #:init-keyword #:track-no)
  (enh-genre  #:init-value ""  #:accessor enh-genre  #:init-keyword #:enh-genre)
  (speed      #:init-value '() #:accessor speed      #:init-keyword #:speed)
  (start-time #:init-value ""  #:accessor start-time #:init-keyword #:start-time)
  (end-time   #:init-value ""  #:accessor end-time   #:init-keyword #:end-time))

(make-symbol "unknown-frame")

(make-symbol "album-frame")                 ;; TAL/TALB
(make-symbol "artist-frame")                ;; TP1/TPE1
(make-symbol "band-frame")                  ;; TP2/TPE2
(make-symbol "bpm-frame")                   ;; TBP/TBPM
(make-symbol "comment-frame")               ;; COM/COMM
(make-symbol "composer-frame")              ;; TCM/TCOM
(make-symbol "conductor-frame")             ;; TP3/TPE3
(make-symbol "content-group-frame")         ;; TT1/TIT1
(make-symbol "copyright-frame")             ;; TCR/TCOP
(make-symbol "date-frame")                  ;; TDA/TDAT
(make-symbol "encoded-by-frame")            ;; TEN/TENC
(make-symbol "file-owner-frame")            ;; TOWN
(make-symbol "file-type-frame")             ;; TFT/TFLT
(make-symbol "genre-frame")                 ;; TCO/TCON
(make-symbol "initial-key-frame")           ;; TKE/TKEY
(make-symbol "interpreted-by-frame")        ;; TP4/TPE4
(make-symbol "isrc-frame")                  ;; TRC/TSRC
(make-symbol "langs-frame")                 ;; TLA/TLAN
(make-symbol "length-frame")                ;; TLE/TLEN
(make-symbol "lyricist-frame")              ;; TXT/TEXT
(make-symbol "media-type-frame")            ;; TMT/TMED
(make-symbol "original-album-frame")        ;; TOT/TOAL
(make-symbol "original-artist-frame")       ;; TOA/TOPE
(make-symbol "original-filename-frame")     ;; TOF/TOFN
(make-symbol "original-lyricist-frame")     ;; TOL/TOLY
(make-symbol "original-release-year-frame") ;; TOR/TORY
(make-symbol "part-of-a-set-frame")         ;; TPA/TPOS
(make-symbol "play-count-frame")            ;; CNT/PCNT
(make-symbol "playlist-delay-frame")        ;; TDY/TDLY
(make-symbol "pop-frame")                   ;; POP/POPM
(make-symbol "publisher-frame")             ;; TPB/TPUB
(make-symbol "recording-dates-frame")       ;; TRD/TRDA
(make-symbol "settings-frame")              ;; TSS/TSSE
(make-symbol "size-frame")                  ;; TSI/TSIZ
(make-symbol "station-name-frame")          ;; TRSN
(make-symbol "station-owner-frame")         ;; TRSO
(make-symbol "subtitle-frame")              ;; TT3/TIT3
(make-symbol "tag-cloud-frame")             ;; XTG/XTAG
(make-symbol "time-frame")                  ;; TIM/TIME
(make-symbol "title-frame")                 ;; TT2/TIT2
(make-symbol "track-frame")                 ;; TRK/TRCK
(make-symbol "udt-frame")                   ;; TXX/TXXX
(make-symbol "year-frame")                  ;; TYE/TYER

(define-class <id3v2-frame> ()
  (id      #:init-value 'unknown-frame #:accessor id      #:init-keyword #:id)
  (tap     #:init-value '()            #:accessor tap     #:init-keyword #:tap)
  (fap     #:init-value '()            #:accessor fap     #:init-keyword #:fap)
  (ro      #:init-value '()            #:accessor ro      #:init-keyword #:ro)
  (unsync  #:init-value '()            #:accessor unsync  #:init-keyword #:unsync))

(define-class <unk-frame> (<id3v2-frame>)
  (id-text #:init-value ""     #:accessor frameid #:init-keyword #:frameid)
  (data    #:init-value #vu8() #:accessor data    #:init-keyword #:data))

(define-class <text-frame> (<id3v2-frame>)
  (text #:init-value "" #:accessor text #:init-keyword #:text))

(define-class <comment-frame> (<id3v2-frame>)
  (lang  #:init-value "eng" #:accessor lang #:init-keyword #:lang)
  (dsc   #:init-value ""    #:accessor dsc  #:init-keyword #:dsc)
  (text  #:init-value ""    #:accessor text #:init-keyword #:text))

(define-class <user-defined-text-frame> (<id3v2-frame>)
  (dsc   #:init-value "" #:accessor dsc  #:init-keyword #:dsc)
  (text  #:init-value "" #:accessor text #:init-keyword #:text))

(define-class <play-count-frame> (<id3v2-frame>)
  (count #:init-value 0 #:accessor count #:init-keyword #:count))

(define-class <pop-frame> (<id3v2-frame>)
  (e-mail #:init-value "" #:accessor e-mail #:init-keyword #:e-mail)
  (rating #:init-value 0  #:accessor rating #:init-keyword #:rating)
  (count  #:init-value 0  #:accessor count  #:init-keyword #:count))

(define-class <tag-cloud-frame> (<id3v2-frame>)
  (owner #:init-value ""  #:accessor owner #:init-keyword #:owner)
  (tags  #:init-value '() #:accessor tags  #:init-keyword #:tags))

(define-class <id3v2-tag> ()
  (experimental #:init-value '() #:accessor experimental
                #:init-keyword experimental)
  (frames       #:init-value '() #:accessor frames  #:init-keyword #:frames)
  (padding      #:init-value   0 #:accessor padding #:init-keyword #:padding))

(define (has-frame-internal frms f)
  (cond ((null? frms) #f)
        ((let ((x (car frms))) (eq? f (slot-ref x 'id))) #t)
        (else (has-frame-internal (cdr frms) f))))

(define-method (has-frame? (tag <id3v2-tag>) (f <symbol>))
  "Return #t if TAG has any frames with symbol F"
  (has-frame-internal (slot-ref tag 'frames) f))

(define-method (get-frames (x <id3v2-tag>) (f <symbol>))
  "Return a list (possibly empty) of frames in TAG with symbol F"
  (let ((frms (slot-ref x 'frames))
        (ret '()))
    (while (not (null? frms))
           (if (eq? f (slot-ref (car frms) 'id))
               (set! ret (append ret (list (car frms)))))
           (set! frms (cdr frms)))
    ret))


