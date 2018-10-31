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

(declaim (inline fivebit canonicalize-token chain-bits))

(defun fivebit (code)
  (logand code 31))

(defun chain-bits (bits)
  (reduce #'(lambda (a &optional b)
	      (if b (logior (ash a 5) b) a))
	  bits))

(defun chain-string (string)
  (chain-bits (map 'list #'(lambda (x) (fivebit (char-code x))) string)))

(defun canonicalize-token (string)
  "Return bit-sequence of 5 bit alpha char segments"
  (let ((alphas (remove-if-not #'alpha-char-p string)))
    (chain-string alphas)))

(defun demonkey (&optional (text-path "shakespeare_lovers_complaint.txt"))
  (with-open-file (s text-path :direction :input)
    (let ((text (loop for line = (read-line s nil) while line
		   nconc (split-sequence:split-sequence #\Space line :remove-empty-subseqs t))))
      (with-open-file (entropy "/dev/random" :direction :input :element-type '(unsigned-byte 8))
	(loop for token = (pop text)
	   for canonical = (canonicalize-token token)
	   for num-bits = (1+ (floor (log canonical 2)))
	   for mask = (1- (expt 2 num-bits))
	   for expanded-mask = (1- (expt 2 (+ num-bits 8)))
	   for num-bytes = (ceiling num-bits 8)
	   do (loop
		 for rands = (chain-bits (loop repeat num-bytes collecting (read-byte entropy))) then (let ((new (logand expanded-mask (ash rands 1))))
													(if (zerop (logand 255 new)) ;shifted a byte out
													    (logior new (read-byte entropy))
													    new))
		 until (= canonical (ash rands -8)))
	     (format t "~A " token))))))
