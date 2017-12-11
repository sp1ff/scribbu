(use-modules (ice-9 format))
(use-modules (ice-9 regex))
(use-modules (scribbu))

(setlocale LC_ALL "")

(define (report-on-encoded-by track)
  (format #t "~s: " (scribbu/get-path track))
  (if (scribbu/has-id3v1-tag track)
	  (format #t "ID3v1 comment: ~s" (scribbu/get-id3v1-string track 'comment))
	  (display "<no ID3v1 match>"))
  (let ((num-tags (scribbu/get-id3v2-tag-count track)))
	(do ((i 0 (1+ i)))
		((>= i num-tags))
	  (if (scribbu/has-id3v2-attribute track i 'encoded-by)
		  (format #t ", ~d/encoded-by: ~s" i
				  (scribbu/get-id3v2-attribute track i 'encoded-by)))))
  (format #t "\n"))

;; If `track' has an ID3v1 comment field matching /.*winamp.*/:

;;   If `track' has an ID3v2 tag without a TENC frame, add a TENC frame of "Winamp"
;;   else if `track' has no ID3v2 tag. create one with only a TENC frame of "Winamp"
;;   else print a warning consisting of the TENC frames in the extant ID3v2 frames.
(define (cleanup-encoded-by-1 track)
  (format #t "cleanup-encoded-by-1: ~s\n" (scribbu/get-path track))
  (if (scribbu/has-id3v1-tag track)
	  (let ((r (make-regexp ".*winamp.*" regexp/icase)))
		(format #t "~s => ~s\n"
				(scribbu/get-id3v1-string track 'comment)
				(regexp-exec r (scribbu/get-id3v1-string track 'comment)))
		(if (regexp-exec r (scribbu/get-id3v1-string track 'comment))
			(begin
			  (display "... evaluated as True!\n")
			  (let ((num-tags (scribbu/get-id3v2-tag-count track)))
				(if (eq? num-tags 0)
					(begin
					  (display "... no ID3v2 tags\n")
					  (scribbu/make-id3v2-tag track 0)
					  (scribbu/set-id3v2-attribute track 0 'encoded-by "Winamp")
					  (format #t "Writing ~s...\n"
							  (string-join (list (basename (scribbu/get-path track)) "out") "."))
					  (scribbu/write-id3v2-tag
					   track 0
					   (string-join (list (basename (scribbu/get-path track)) "out") "."))
					  (format #t "Writing ~s...done.\n"
							  (string-join (list (basename (scribbu/get-path track)) "out") ".")))
					(let ((encoders '()))
					  (begin
						(format #t "... ~d ID3v2 tags\n" num-tags)
						(do ((i 0 (1+ i)))
							((>= i num-tags))
						  (if (scribbu/has-id3v2-attribute track i 'encoded-by)
							  (set!
							   encoders
							   (cons (scribbu/get-id3v2-attribute track i 'encoded-by) encoders))
							  (begin
							  (scribbu/set-id3v2-attribute track i 'encoded-by "Winamp")
							  (scribbu/write-id3v2-tag
							   track i
							   (string-join (list
											 (basename (scribbu/get-path track))
											 (number->string i)
											 "out") "."))))))
					  (if (eq? num-tags (length encoders))
						  (format #t "~s: already encoded by ~s\n"
								  (scribbu/get-path track) encoders))))))))))

(define (main dir)
  (let ((data (string-join (list dir "data") "/" 'infix)))
	(scribbu/with-track-in data report-on-encoded-by)
	(scribbu/with-track-in data cleanup-encoded-by-1)))

(let ((cl (cdr (command-line))))
  (if (= 1 (length cl))
	  (main (car cl))
	  (begin
		(format #t "Usage: test-cleanup-encoded-by.scm ${srcdir}\n")
		(exit 2))))
