; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Yakup Genc                       *
; *********************************************

;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"

(defun read-as-list (filename)
	(with-open-file (stream filename)
		(loop for words = (read stream nil)
		while words
		collect words
		)
	)
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."

)

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***

(defun spell-checker-0 (word)

	(loop for counter in (read-as-list "dictionary1.txt")			;this is my test dictionary.you can change here.
		do (if (eql counter word) (return-from spell-checker-0 T))
		)
	;checks using brute force method.(linear search)
)

(defun spell-checker-1 (word)
	(setq hashtable (make-hash-table))
	(setf key "exist")
	(loop for counter in (read-as-list "dictionary1.txt")           ;this is my test dictionary.you can change here.
		do (progn 
			(setf (gethash counter hashtable) key)
		)
	)
	(if (equal (gethash word hashtable) "exist") (return-from spell-checker-1 T))
	(return-from spell-checker-1 nil)
 	;checks using hashmap method.(checks the key is valid or not.)
)

;; -----------------------------------------------------
;; DECODE FUNCTIONS


(defun check-encoded-paragraph (paragraph)
	(setq check T)
	(loop for word in paragraph
		do (if (eql T (spell-checker-0 word) ) () (setq check nil))
	)
	(return-from check-encoded-paragraph check)
	;splits the paragraph between words and call spell checker in order to the word is valid or not.
)

(defun switch-characters (word alphabet encrypt-alphabet)
	(setq listword (coerce (string word) 'list))
	(setq decryptword '())
	(loop for i in listword

		do(loop for j from 0 to (- (length alphabet) 1)
			do (if (char-equal i (char (string (nth j alphabet)) 0)) 		
				(push (nth j encrypt-alphabet) decryptword)
			)
		)
	)
	(return-from switch-characters (intern (reverse (format nil "~{~A~}" decryptword))))
	;switches the words to cipher alphabet
)


(defun permutation (paragraph originalphabet alphabet left right numberturn)
	(if (eql left right)
		(progn
			(setq decryptparagraph '())
			(loop for j in paragraph						;tries to find correct cipher alphabet in order to decrypt words.
				do (push (switch-characters j originalphabet alphabet) decryptparagraph)
			)
			(setf numberturn (+ numberturn 1))
			(if (eql T (check-encoded-paragraph decryptparagraph))
				(progn 
					(print (reverse decryptparagraph))							
					(terpri)								;prints the decrypted words.
					(princ "Found after ")
					(princ numberturn)						;prints the number of turns to decrypt words.
					(princ " tries.")
					(terpri)
				)
			)
		)
		(loop for i from left to right
			do (progn
				(rotatef (nth left alphabet) (nth i alphabet))
				(setf numberturn (permutation paragraph originalphabet alphabet (+ left 1) right numberturn))	;calculates every permutation of alphabet
				(rotatef (nth left alphabet) (nth i alphabet))
				)
		)
	)
	(return-from permutation numberturn)
)

(defun most-frequency-word (paragraph alphabet fsize)
	(setq paraglist '())
	(setf freqwords '())
	(setf freqsize '())
	(loop for i in paragraph
		do (push (coerce (string i) 'list) paraglist)
	)
	(loop for j in paraglist
		do (loop for k in j
			do(progn
				(setf temp (position k freqwords))					;finds the most frequency words in the encrypted paragraph.
				(if (equal nil temp)
					(progn 
						(push k freqwords)
						(push 1 freqsize)
					)
					(setf (nth temp freqsize) (+ 1 (nth temp freqsize)))
				)
			)
		)
	)
	(setf returnlist '())
	(loop for n from 0 to (- fsize 1)
	do(progn
		(setf index (search-highest freqsize))
		(push (intern (string (nth index freqwords))) returnlist)
		(setf (nth index freqsize) 0)
		(setf (nth index freqwords) nil)
		)
	)
	
	(return-from most-frequency-word (reverse returnlist))
)

(defun search-highest (list)
	(setf highest-val (nth 0 list))
	(setf highest-index 0)
	(loop for m from 0 to (- (length list) 1)
		do(if(< highest-val (nth m list)) 
			(progn
				(setf hightest-val (nth m list))				;searches the highest value in a list.
				(setf highest-index m)
			) 
		)
	)
	(return-from search-highest highest-index)
)

(defun Gen-Decoder-A (paragraph alphabet)
	(print "------Gen-Decoder-A------")
	(permutation paragraph alphabet (copy-list alphabet) 0 (- (length alphabet) 1) 0)
	;using brute force method (with random order)
)

(defun Gen-Decoder-B-0 (paragraph alphabet mostfreq)
	(print "------Gen-Decoder-B-0------")
	(setq copyalphabet (copy-list alphabet))
	(setq mostfreqencoded (most-frequency-word paragraph alphabet (length mostfreq)))
	(loop for i in mostfreq
	do(setf freqalphabet (delete i copyalphabet))			;find frequency words, switches the order of alphabet in order to find words more easily, then uses brute force method again.
	)
	(setf freqalphabet (append freqalphabet mostfreq))
	(setq copyalphabet (copy-list alphabet))
	(loop for j in mostfreqencoded
	do(setf freqencodedalphabet (delete j copyalphabet))
	)
	(setf freqencodedalphabet (append freqencodedalphabet mostfreqencoded))
	(permutation paragraph freqalphabet freqencodedalphabet 0 (- (length alphabet) 1) 0)
)

(defun Gen-Decoder-B-1 (paragraph)
	(print "This decoder is out of usage")
  	;this method is not written.
)

(defun Code-Breaker (document decoder)

	(setq paragraphs (read-as-list document))
	(setq myalphabet '(E H I T S))					;this is my own alphabet. if you want to add another words, add here.
	(setq mostfrequency '(T S))						;this is my own frequency words. if you want to add another, add here.
	(if(equal "A" decoder)
		(Gen-Decoder-A paragraphs myalphabet)
		(if(equal "B-0" decoder)
			(Gen-Decoder-B-0 paragraphs myalphabet mostfrequency)
			(if(equal "B-1" decoder)
				(Gen-Decoder-B-1 paragraphs)
				(print "Wrong Decoder.")
			)
		)
	)
  	;selects the correct document and decoder.
)

;; -----------------------------------------------------
;; Test code...

(defun test_on_test_data ()
	(print "....................................................")
	(print "Testing ....")
	(print "....................................................")
	(setq documentname "document3.txt")						;this is my own test document.you can change the file from here.
	(Code-Breaker documentname "A")							;test function
	(Code-Breaker documentname "B-0")
	(print "Spell-checker-1 test(Hashmap)")
	(print (spell-checker-1 (car '(this))))
	(print (spell-checker-1 (car '(tea))))
)


;; test code...
(test_on_test_data)