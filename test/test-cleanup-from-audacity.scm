(use-modules (ice-9 format))
(use-modules (ice-9 regex))
(use-modules (scribbu))

(setlocale LC_ALL "")

(define (main file)
  "Cleanup a file output by Audacity"
  (let ((track (scribbu/make-track file)))
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
