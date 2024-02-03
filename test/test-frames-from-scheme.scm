;;;;
;;;; test-frames-from-scheme.scm
;;;;
;;;; Copyright (C) 2019-2024 Michael Herstine <sp1ff@pobox.com>
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

(define (test-play-count dir)
  "Run some smoke tests for reading & writing tags, including PCNT."

  (let ((tags (read-tagset (string-join (list dir "id3v2.3.tag") "/" 'infix))))
    (unless (eq? 1 (length tags))
      (throw 'failure
             (format #f "expected 1 tag, got ~d" (length tags))))
    (let* ((ver-tag (car tags))
           (tag (car ver-tag))
           (ver (cadr ver-tag)))
      (unless (eq? 3 ver)
        (throw 'failure (format #f "expected ID3v2.3, got ~d" ver)))
      (if (slot-ref tag 'experimental)
          (throw 'failure (format #f "tag should not be experimental")))
      (unless (eq? 335921 (slot-ref tag 'padding))
        (throw 'failure (format #f "expected 335921 bytes of padding; got ~d"
                                (slot-ref tag 'padding))))
      ;; Brute-force test for deserializing

      ;;   class           id                  'text or 'id-text
      ;; ========================================================
      ;; 0 <text-frame>    title-frame         "Lorca's Novena"
      ;; 1 <text-frame>    artist-frame        "The Pogues"
      ;; 2 <text-frame>    album-frame         "Hell's Ditch [Expanded] (US Version)"
      ;; 3 <text-frame>    genre-frame         "Pop"
      ;; 4 <text-frame>    composer-frame      ""
      ;; 5 <text-frame>    conductor-frame     ""
      ;; 6 <text-frame>    track-frame         "5"
      ;; 7 <text-frame>    year-frame          "1990"
      ;; 8 <text-frame>    band-frame          "The Pogues"
      ;; 9 <comment-frame> comment-frame
      ;; <text-frame>    copyright-frame     "2004 Warner Music UK Ltd."
      ;; <text-frame>    part-of-a-set-frame "1"
      ;; <unk-frame>     unknown-frame       "APIC"
      ;; <unk-frame>     unknown-frame       "PRIV"
      (let ((frames (slot-ref tag 'frames)))
        (let* ((F (car frames))
               (frames (cdr frames)))
          (unless (and (eq? 'title-frame (slot-ref F 'id))
                       (equal? "Lorca's Novena" (slot-ref F 'text)))
            (throw 'failure (format #f "unexpected frame ~a" F)))
          (let* ((F (car frames))
                 (frames (cdr frames)))
            (unless (and (eq? 'artist-frame (slot-ref F 'id))
                         (equal? "The Pogues" (slot-ref F 'text)))
              (throw 'failure (format #f "unexpected frame ~a" F)))
            (let* ((F (car frames))
                   (frames (cdr frames)))
              (unless (and (eq? 'album-frame (slot-ref F 'id))
                           (equal? "Hell's Ditch [Expanded] (US Version)"
                                   (slot-ref F 'text)))
                (throw 'failure (format #f "unexpected frame ~a" F)))
              (let* ((F (car frames))
                     (frames (cdr frames)))
                (unless (and (eq? 'genre-frame (slot-ref F 'id))
                             (equal? "Pop" (slot-ref F 'text)))
                  (throw 'failure (format #f "unexpected frame ~a" F)))
                (let* ((F (car frames))
                       (frames (cdr frames)))
                  (unless (and (eq? (class-of F) <text-frame>)
                               (equal? "" (slot-ref F 'text)))
                    (throw 'failure (format #f "unexpected frame ~a" F)))
                  (let* ((F (car frames))
                         (frames (cdr frames)))
                    (unless (and (eq? (class-of F) <text-frame>)
                                 (equal? "" (slot-ref F 'text)))
                      (throw 'failure (format #f "unexpected frame ~a" F)))
                    (let* ((F (car frames))
                           (frames (cdr frames)))
                      (unless (and (eq? (class-of F) <text-frame>)
                                   (equal? "5" (slot-ref F 'text)))
                        (throw 'failure (format #f "unexpected frame ~a" F)))
                      (let* ((F (car frames))
                             (frames (cdr frames)))
                        (unless (and (eq? (class-of F) <text-frame>)
                                     (equal? "1990" (slot-ref F 'text)))
                          (throw 'failure (format #f "unexpected frame ~a" F)))
                        (let* ((F (car frames))
                               (frames (cdr frames)))
                          (unless (and (eq? (class-of F) <text-frame>)
                                       (equal? "The Pogues" (slot-ref F 'text)))
                            (throw 'failure (format #f "unexpected frame ~a" F)))
                          (let* ((F (car frames))
                                 (frames (cdr frames)))
                            (unless (eq? (class-of F) <comment-frame>)
                              (throw 'failure (format #f "unexpected frame ~a" F)))
                            (let* ((F (car frames))
                                   (frames (cdr frames)))
                              (unless (and (eq? (class-of F) <text-frame>)
                                           (equal? "2004 Warner Music UK Ltd." (slot-ref F 'text)))
                                (throw 'failure (format #f "unexpected frame ~a" F)))
                              (let* ((F (car frames))
                                     (frames (cdr frames)))
                                (unless (and (eq? (class-of F) <text-frame>)
                                             (equal? "1" (slot-ref F 'text)))
                                  (throw 'failure (format #f "unexpected frame ~a" F)))
                                (let* ((F (car frames))
                                       (frames (cdr frames)))
                                  (unless (and (eq? (class-of F) <unk-frame>)
                                               (equal? "APIC" (slot-ref F 'id-text)))
                                    (throw 'failure (format #f "unexpected frame ~a" F)))
                                  (let* ((F (car frames))
                                         (frames (cdr frames)))
                                    (unless (and (eq? (class-of F) <unk-frame>)
                                                 (equal? "PRIV" (slot-ref F 'id-text)))
                                      (throw 'failure (format #f "unexpected frame ~a" F))))))))))))))))))

      ;; Now let's take the first few frames, add a PCNT, & write it back out:
      (let* ((new-frames
              (append
               (list
                (make <play-count-frame> #:id 'play-count-frame #:count 11))
               (list-head (slot-ref tag 'frames) 4)))
             (new-tag (make <id3v2-tag> #:frames new-frames))
             (tmpfp (mkstemp! (string-copy "/tmp/test-play-count-XXXXXX")))
             (tmpf (port-filename tmpfp)))

        (close-port tmpfp)
        (write-tagset (list (list new-tag 3)) tmpf)
        ;; Read that file back in
        (let ((tags (read-tagset tmpf)))
          (unless (eq? 1 (length tags))
            (throw 'failure (format #f "expected one tag, got ~d" (length tags))))
          (let* ((vtag (car tags))
                 (tag (car vtag))
                 (ver (cadr vtag)))
            (unless (eq? 3 ver)
              (throw 'failure (format #f "expected version 3, got ~d" ver)))
            (let ((fexp (slot-ref tag 'experimental))
                  (frames (slot-ref tag 'frames))
                  (pad (slot-ref tag 'padding)))
              (unless (eq? fexp #f)
                (throw 'failure "did not expect experimental tag"))
              (unless (eq? 0 pad)
                (throw 'failure (format #f "expected no padding, got ~d" pad))))

            (let ((frames (slot-ref tag 'frames)))
              (let* ((F (car frames))
                     (frames (cdr frames)))
                (unless (and (eq? 'play-count-frame (slot-ref F 'id))
                             (eq? 11 (slot-ref F 'count)))
                  (throw 'failure (format #f "unexpected frame ~a" F)))
                (let* ((F (car frames))
                       (frames (cdr frames)))
                  (unless (and (eq? 'title-frame (slot-ref F 'id))
                               (equal? "Lorca's Novena" (slot-ref F 'text)))
                    (throw 'failure (format #f "unexpected frame ~a" F)))
                  (let* ((F (car frames))
                         (frames (cdr frames)))
                    (unless (and (eq? 'artist-frame (slot-ref F 'id))
                                 (equal? "The Pogues" (slot-ref F 'text)))
                      (throw 'failure (format #f "unexpected frame ~a" F)))
                    (let* ((F (car frames))
                           (frames (cdr frames)))
                      (unless (and (eq? 'album-frame (slot-ref F 'id))
                                   (equal? "Hell's Ditch [Expanded] (US Version)"
                                           (slot-ref F 'text)))
                        (throw 'failure (format #f "unexpected frame ~a" F)))
                      (let* ((F (car frames))
                             (frames (cdr frames)))
                        (unless (and (eq? 'genre-frame (slot-ref F 'id))
                                     (equal? "Pop" (slot-ref F 'text)))
                          (throw 'failure (format #f "unexpected frame ~a" F)))))))))

            (let* ((new-frames (get-frames tag 'play-count-frame))
                   (num-pc (length new-frames)))
              (unless (eq? 1 num-pc)
                (throw 'failure (format #t "got ~d pcnt frames" num-pc)))
              (let* ((pc (car new-frames))
                     (count (slot-ref pc 'count)))
                (unless (eq? count 11)
                  (throw 'failure (format #t "expected 11 plays, got ~d" count)))))))))))

(define (test-popm dir)
  "Run some smoke tests for reading & writing POPM."

  (let ((tags (read-tagset (string-join (list dir "id3v2.3.tag") "/" 'infix))))
    (unless (eq? 1 (length tags))
      (throw 'failure
             (format #f "expected 1 tag, got ~d" (length tags))))
    (let* ((ver-tag (car tags))
           (tag (car ver-tag))
           (ver (cadr ver-tag)))
      (unless (eq? 3 ver)
        (throw 'failure (format #f "expected ID3v2.3, got ~d" ver)))
      (if (slot-ref tag 'experimental)
          (throw 'failure (format #f "tag should not be experimental")))
      (unless (eq? 335921 (slot-ref tag 'padding))
        (throw 'failure (format #f "expected 335921 bytes of padding; got ~d"
                                (slot-ref tag 'padding))))
      ;; Brute-force test for deserializing

      ;; class           id                'text or 'id-text
      ;; ===================================================
      ;; <text-frame>    title-frame       "Lorca's Novena"
      ;; <text-frame>    artist-frame      "The Pogues"
      ;; <text-frame>    album-frame       "Hell's Ditch [Expanded] (US Version)"
      ;; <text-frame>    genre-frame       "Pop"
      ;; <unk-frame>     unknown-frame     "TCOM"
      ;; <unk-frame>     unknown-frame     "TPE3"
      ;; <text-frame>    track-frame       "5"
      ;; <text-frame>    year-frame        "1990"
      ;; <unk-frame>     unknown-frame     "TPE2"
      ;; <unk-frame>     unknown-frame     "COMM"
      ;; <unk-frame>     unknown-frame     "TCOP"
      ;; <unk-frame>     unknown-frame     "TPOS"
      ;; <unk-frame>     unknown-frame     "APIC"
      ;; <unk-frame>     unknown-frame     "PRIV"

      (let* ((new-frames
              (append
               (list
                (make <popm-frame> #:id 'popm-frame #:e-mail "sp1ff@pobox.com" #:rating 211 #:count 11))
               (list-head (slot-ref tag 'frames) 2)))
             (new-tag (make <id3v2-tag> #:frames new-frames))
             (tmpfp (mkstemp! (string-copy "/tmp/test-popm-XXXXXX")))
             (tmpf (port-filename tmpfp)))
        (close-port tmpfp)
        (write-tagset (list (list new-tag 3)) tmpf)
        ;; Read that file back in
        (let ((tags (read-tagset tmpf)))
          (unless (eq? 1 (length tags))
            (throw 'failure (format #f "expected one tag, got ~d" (length tags))))
          (let* ((vtag (car tags))
                 (tag (car vtag))
                 (ver (cadr vtag)))
            (unless (eq? 3 ver)
              (throw 'failure (format #f "expected version 3, got ~d" ver)))
            (let ((fexp (slot-ref tag 'experimental))
                  (frames (slot-ref tag 'frames))
                  (pad (slot-ref tag 'padding)))
              (unless (eq? fexp #f)
                (throw 'failure "did not expect experimental tag"))
              (unless (eq? 0 pad)
                (throw 'failure (format #f "expected no padding, got ~d" pad))))

            (let ((frames (slot-ref tag 'frames)))
              (let* ((F (car frames))
                     (frames (cdr frames)))
                (unless (and (eq? 'popm-frame (slot-ref F 'id))
                             (eq? 11 (slot-ref F 'count))
                             (equal? "sp1ff@pobox.com" (slot-ref F 'e-mail))
                             (eq? 211 (slot-ref F 'rating)))
                  (throw 'failure (format #f "unexpected frame ~a" F)))))))))))

(define (test-tag-cloud dir)
  "Run some smoke tests for reading & writing XTAG."

  (let ((tag (make <id3v2-tag>
               #:frames (list (make <tag-cloud-frame>
                                #:id 'tag-cloud-frame
                                #:owner "sp1ff@pobox.com"
                                #:tags (acons
                                        "sub-genres"
                                        '("a" "b")
                                        (acons
                                         "mood" "sentimental"
                                         (acons
                                          "90s" '() '())))))))
        (out "test-xtag.tag"))

    (write-tagset (list (list tag 3)) out)

    ;; 000000 49 44 33 03 00 00 00 00 00 4b                    ID3v2.3 tag, no flags, 0x4b = 75 bytes
    ;; 00000a                               58 54 41 47 00 00  XTAG, 0x41 = 65 bytes, no flags
    ;; 000010 00 41 00 00
    ;; 000014             01                                   version 1
    ;; 000015                73 70 31 66 66 40 70 6f 62 6f 78  sp1ff@pobox.com
    ;; 000020 2e 63 6f 6d 00
    ;; 000025                39 30 73 00 00 00 00 00           "90s", no values
    ;; 00002d                                        6d 6f 6f  "mood", 12 bytes, "sentimental"
    ;; 000030 64 00 00 00 00 0c 73 65 6e 74 69 6d 65 6e 74 61
    ;; 000040 6c 00
    ;; 000042       73 75 62 2d 67 65 6e 72 65 73 00 00 00 00  "sub-genres", 4 bytes, "a", "b"
    ;; 000050 04 61 00 62 00
    ;; 000055

    (let ((tags (read-tagset out)))
      (unless (eq? 1 (length tags))
        (throw 'failure (format #f "expected one tag, got ~d" (length tags))))
      (let* ((vtag (car tags))
             (tag (car vtag))
             (ver (cadr vtag)))
        (unless (eq? 3 ver)
          (throw 'failure (format #f "expected version 3, got ~d" ver)))
        (let ((fexp (slot-ref tag 'experimental))
              (frames (slot-ref tag 'frames))
              (pad (slot-ref tag 'padding)))
          (unless (eq? fexp #f)
            (throw 'failure "did not expect experimental tag"))
          (unless (eq? 0 pad)
            (throw 'failure (format #f "expected no padding, got ~d" pad))))

        (let ((frames (slot-ref tag 'frames)))
          (let* ((F (car frames))
                 (cloud (slot-ref F 'tags)))
            (unless (eq? 'tag-cloud-frame (slot-ref F 'id))
              (throw 'failure (format #f "unexpected frame ~a" F)))
            (unless (equal? "sp1ff@pobox.com" (slot-ref F 'owner))
              (throw 'failure (format #f "unexpected owner ~s" (slot-ref F 'owner))))
            (let ((sg (list-tail (assoc "sub-genres" cloud) 1)))
              (unless (eq? 2 (length sg))
                (throw 'failure "expected two sub-genes, got ~d" (length sg)))
              (unless (and (equal? "a" (list-ref sg 0)) (equal? "b" (list-ref sg 1)))
                (throw 'failure "expected (a b), got ~a" sg)))
            (let ((mood (list-tail (assoc "mood" cloud) 1)))
              (unless (eq? 1 (length mood))
                (throw 'failure (format #f "exected one mood; got ~d" (length mood))))
              (unless (equal? (car mood) "sentimental")
                (throw 'failure "expected sentimental, got ~s" (car mood))))
            (let ((nineties (list-tail (assoc "90s" cloud) 1)))
              (unless (eq? 0 (length nineties))
                (throw 'failure "expected '() got ~a" nineties)))))))
    (delete-file out)))

(define (main dir)
  "Test reading & writing assorted frames in Scheme"
  (format #t "`test-play-count'...")
  (test-play-count dir)
  (format #t "...done.\n")
  (format #t "`test-popm'...")
  (test-popm dir)
  (format #t "...done.\n")
  (format #t "`test-tag-cloud'...")
  (test-tag-cloud dir)
  (format #t "...done.\n"))

(let ((cl (cdr (command-line))))
  (if (= 1 (length cl))
	    (main (car cl))
	    (begin
		    (format #t "Usage: test-frames-from-scheme.scm ${srcdir}/data\n")
		    (exit 2))))
