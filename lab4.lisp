(load "splitseq/split-sequence.lisp")
(in-package :split-sequence)

; Sentence class
(defclass sentence()
	((content :accessor sentence_content
			  :initform (error "No sentence content supplied")
			  :initarg :content))
)

; Input class

(defclass input()
	((content :accessor input_content
			  :initform (error "No input content supplied")
			  :initarg :content))
)

; Read file

(defun file-string (path)
	(with-open-file (stream path)
		(let ((data (make-string (file-length stream)))) (read-sequence data stream) data)
	)
)

; Printing methods

(defmethod print_sentence((s sentence) &key)
	(print (slot-value s 'content))
)

(defun print_sentence_list (ls)
	(cond 
		((null ls) 0)
		(t (print_sentence (car ls)) (print_sentence_list (cdr ls)))
	)
)

; Reading cleanup


(defmethod cleanup-sentence ((s Sentence))
	(if (string= (subseq (sentence_content s) 0 1) " ") 
		(make-instance 'sentence :content (subseq (sentence_content s) 1))
		s
	)
)

(defun cleanup-sentences (ls)
	(cond
		((null ls) '())
		(T (cons (cleanup-sentence (car ls)) (cleanup-sentences (cdr ls))))
	)
)

; Exchange words

(defun get-last-element-of-list (ls)
	(cond
		((not (cdr ls)) (car ls))
		(T (get-last-element-of-list (cdr ls)))
	)
)

(defmethod exchange-words-in-sentence ((s Sentence))
	(setq all-words (remove-if #'(lambda (x) (equal "" x)) (split-sequence-if-not 'alpha-char-p (sentence_content s))))
	(setq first-word (car all-words))
	(setq last-word (get-last-element-of-list all-words))
	(setq sentence_length (length (sentence_content s)))
	(if (> sentence_length 2)
		(make-instance 'sentence :content 
			(concatenate 'string (string-capitalize last-word)
				(subseq (sentence_content s) 
						(length first-word)
						(- sentence_length (length last-word))	
				)
			(string-downcase first-word)	
			)
		)
		s
	)
)

(defun exchange-words-in-sentence-list (ls)
	(cond
		((null ls) '())
		(T (cons (exchange-words-in-sentence (car ls)) (exchange-words-in-sentence-list (cdr ls))))
	)
) 


(setq fname "input_text.txt")
(setq text (subseq (file-string fname) 1))

(setq list_all (remove-if #'(lambda (x) (equal "" x)) (split-sequence #\. text)))
(setq all_sentences_list (map 'list (lambda (x) (make-instance 'sentence :content x)) list_all ))
(setq all_sentences_list_cleaned (cleanup-sentences all_sentences_list))
(setq all (make-instance 'input :content all_sentences_list_cleaned))

(print_sentence_list (input_content all))

(print_sentence_list (exchange-words-in-sentence-list (input_content all)))
