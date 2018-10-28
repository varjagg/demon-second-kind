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

(defun demon (&optional (min-length 3))
  (with-open-file (entropy "/dev/urandom" :direction :input :element-type '(unsigned-byte 8))
    (loop for chant = (coerce (follow *word-trie* entropy) 'string)
       do
	 (when (>= (length chant) min-length)
	   (when (find chant *wordlist* :test #'string=) ;making sure it's a complete word
	    (cond ((and (= (length chant) 3) (= (random 50) 1))
		   (format t "~%~A" chant))
		  ((> (length chant) 3) (format t "~%~A" chant))))))))

(defun initialize (path)
  (setq *wordlist* (cdr (sort (read-wordlist path) ;short words first
			      #'< :key #'length)))
  (setq *word-trie* (btrie:make-trie *wordlist*))
  nil)

(defun canonicalize-token (string)
  "Remove non-alphabetic characters from the string and make it upcase"
  (nstring-upcase (remove-if-not #'alpha-char-p string)))

(defun demonkey (&optional (text-path "shakespeare_lovers_complaint.txt"))
  (with-open-file (s text-path :direction :input)
    (let ((text (loop for line = (read-line s nil) while line
		   nconc (split-sequence:split-sequence #\Space line :remove-empty-subseqs t))))
      (with-open-file (entropy "/dev/random" :direction :input :element-type '(unsigned-byte 8))
	(loop for token = (pop text)
	   for canonical = (canonicalize-token token)
	   do (loop with pos = 0
		 for char = (code-char (logand (read-byte entropy) 127))
		 until (= pos (length canonical))
		 if (char-equal (aref canonical pos) char) do (incf pos)
		 else do (setf pos 0))
	     (format t "~A " token))))))
