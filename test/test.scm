(use-modules (scribbu))
(define test-dir "/tmp/scribbu-test/scheme")

(define G (scribbu/file-system-tree-generator test-dir))

(if (not (string=? (G) (string-append test-dir "/foo"))) (exit 1))
(if (not (string=? (G) (string-append test-dir "/a/bar"))) (exit 1))
(if (not (string=? (G) (string-append test-dir "/a/b/splat"))) (exit 1))

