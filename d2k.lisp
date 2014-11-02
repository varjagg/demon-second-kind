(in-package #:d2k)

(defvar *wordlist* '())

(defvar *word-trie* '())

(defun read-wordlist (path)
  (with-open-file (wordfile path :direction :input)
    (let ((data (make-string (file-length wordfile))))
      (read-sequence data wordfile)
      (split-sequence:split-sequence #\Newline data))))

(defun follow (trie entropy)
  "Traversing trie while entropy blows our sail"
  (let* ((raw (read-byte entropy))
	 (node (code-char (logand raw 127)))
	 (sub (btrie:obtain-seq trie (string node))))
   (if sub
	(cons node (follow sub entropy))
	nil)))

(defun demon ()
  (with-open-file (entropy "/dev/urandom" :direction :input :element-type '(unsigned-byte 8))
    (loop for chant = (coerce (follow *word-trie* entropy) 'string)
       do
	 (when (> (length chant) 2)
	   (when (find chant *wordlist* :test #'string=) ;making sure it's a complete word
	    (cond ((and (= (length chant) 3) (= (random 50) 1))
		   (format t "~%~A" chant))
		  ((> (length chant) 3) (format t "~%~A" chant))))))))

(defun initialize (path)
  (setq *wordlist* (cdr (sort (read-wordlist path) ;short words first
			      #'< :key #'length)))
  (setq *word-trie* (btrie:make-trie *wordlist*))
  nil)
