;;;;
;;;; test-tagsets-from-scheme.scm
;;;;
;;;; Copyright (C) 2019 Michael Herstine <sp1ff@pobox.com>
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
(use-modules (oop goops))
(use-modules (oop goops describe))
(use-modules (scribbu))

(setlocale LC_ALL "")

(define (test-id3v1 dir)
  "Test reading & writing ID3v1 tags in Scheme"
  (define x (read-id3v1-tag
             (string-join (list dir "id3v1.tag") "/" 'infix)))
  (unless (string= (slot-ref x 'title) "Lorca's Novena")
    (throw 'failure "Unexpected title"))
  (unless (string= (slot-ref x 'artist) "The Pogues")
    (throw 'failure "Unexpected artist"))
  (unless (string= (slot-ref x 'album) "Hell's Ditch [Expanded] (US Ve")
    (throw 'failure "Unexpected album"))
  (unless (string= (slot-ref x 'comment) "Amazon.com Song ID: 20355825")
    (throw 'failure "Unexpected comment"))
  (unless (string= (slot-ref x 'year) "1990")
    (throw 'failure "Unexpected year"))
  (unless (eq? (slot-ref x 'genre) 255)
    (throw 'failure "Unexpected genre"))
  (slot-set! x 'artist "Pogues, The")
  (write-id3v1-tag x "test.tag")
  (let ((y (read-id3v1-tag "test.tag")))
    (unless (string= "Pogues, The" (slot-ref x 'artist))
      (throw 'failure "Didn't write artist"))))

(define (test-id3v2 dir)
  "Test reading & writing ID3v2 tags in Scheme"

  (let ((x (read-tagset
            (string-join (list dir "id3v2.3.tag") "/" 'infix))))
    (format #t "checkpoint 1\n")
    (unless (eq? 1 (length x))
      (throw 'failure "Unexpected number of tags"))
    (let* ((y (car x))
           (tag (car y))
           (v (car (cdr y))))
      (unless (eq? 3 v)
        (throw 'failure (format #f "Unexpected version ~d" v)))
      (let ((frms (slot-ref tag 'frames)))
        (if (slot-ref tag 'experimental)
            (throw 'failure "experimental should be false"))
        (let ((padding (slot-ref tag 'padding)))
          (unless (eq? 335921 padding)
            (throw 'failure (format #f "unexpected padding ~a" padding))))
        (let ((nfrms (length frms)))
          (unless (eq? 14 nfrms)
            (throw 'failure (format #f "~d frames" nfrms))))
        ;; Find the artist frame ("The Pogues") and change it to
        ;; "Pogues, The"
        (unless (has-frame? tag 'artist-frame)
          (throw 'failure "no artist frame!?"))
        (let ((work frms))
          (while (not (null? work))
                 (let ((frame (car work)))
                   (if (eq? (slot-ref frame 'id) 'artist-frame)
                       (slot-set! frame 'text "Pogues, The")))
                 (set! work (cdr work))))
        (format #t "checkpoint 2\n")
        (let ((cp "test2.tag"))
          (write-tagset (list (list tag 3)) cp #:apply-unsync 'as-needed)
          (format #t "checkpoint 3\n")
          (let ((new-tags (read-tagset cp)))
            (format #t "checkpoint 4\n")
            (let ((num-new-tags (length new-tags)))
              (unless (eq? 1 num-new-tags)
                (throw 'failure (format #f "got ~d new tags!?" num-new-tags)))
              (let* ((new-tag (caar new-tags))
                     (new-frames (get-frames new-tag 'artist-frame))
                     (num-artists (length new-frames)))
                (unless (eq? 1 num-artists)
                  (throw 'failure (format #f "wrong # of artists: ~d"
                                          num-artists)))
                (let ((new-artist (slot-ref (car new-frames) 'text)))
                  (unless (equal? "Pogues, The" new-artist)
                    (throw 'failure (format #f "wrong artist: ~s" new-artist)))))))
          (delete-file cp))))))

(define (main dir)
  "Test reading & writing tagsets in Scheme"
  (test-id3v1 dir)
  (test-id3v2 dir))

(let ((cl (cdr (command-line))))
  (if (= 1 (length cl))
	    (main (car cl))
	    (begin
		    (format #t "Usage: test-tagsets-from-scheme.scm ${srcdir}/data\n")
		    (exit 2))))
