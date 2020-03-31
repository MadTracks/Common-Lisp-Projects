(defun read-file (filename)
    (with-open-file (stream filename)
		(loop for chars = (read-char stream nil)		;;Reads file char-by-char.
		while chars
		collect chars
		)
	)
)

(defun gppinterpreter (&optional filename)

	(setq file (read-file filename))
	(setq wordlist '())
	(setq charlist '())
	(setq skip 0)
	(setq quote 0)
	(loop for i in file
	do(progn
		
		(if (equal quote 1)
			(if (equal i #\" )
				(setq quote 0)
			)
			(progn
			
				(if (equal skip 2)
					(if (equal i #\Newline)						;;If the file has comment line, it skips the lines until finds new line character.
					(setq skip 0)
				)
				(progn
				
					(if (equal i #\")							;;If the file has quote mark, it skips the words until finds the second quote mark.
					(setq quote 1)
					)

					(if (equal i #\;)
					(setq skip (+ skip 1))
					(setq skip 0)
					)


					(if (or (equal i #\Space) (equal i #\Newline) (equal i #\" ) (equal i #\( ) (equal i #\) ) (equal i #\Tab) )
						(if (not (equal nil charlist))
							(progn
							(setq wordlist (append wordlist (list charlist)))		;;Divide the words from (,),",\Space,\Tab,\Newline.
							(setq charlist '())
							)
						)
					)
				
					(if (and (not (equal i #\Space)) (not (equal i #\Newline)) (not (equal i #\Tab)) )
						(setq charlist (append charlist (list i)))
					)

					(if (or (equal i #\Space) (equal i #\Newline) (equal i #\" ) (equal i #\( ) (equal i #\) ) (equal i #\Tab) )
						(if (not (equal nil charlist))
							(progn
							(setq wordlist (append wordlist (list charlist)))
							(setq charlist '())
							)
						)
					)		
				)
				)		
			)
		)	
	)
	)

	(loop for i from 0 to (- (length wordlist) 1)
	do(setf (nth i wordlist) (format nil "~{~A~}" (nth i wordlist)))
	)
	
	(loop for i in wordlist
	do(progn
		(cond ((equal i "and")  (print "KW_AND"))
				((equal i "or") (print "KW_OR"))
				((equal i "not") (print "KW_NOT"))
				((equal i "equal") (print "KW_EQUAL"))
				((equal i "less") (print "KW_LESS"))
				((equal i "nil") (print "KW_NIL"))
				((equal i "list") (print "KW_LIST"))
				((equal i "append") (print "KW_APPEND"))
				((equal i "concat") (print "KW_CONCAT"))
				((equal i "set") (print "KW_SET"))
				((equal i "deffun") (print "KW_DEFFUN"))
				((equal i "for") (print "KW_FOR"))
				((equal i "if") (print "KW_IF"))
				((equal i "exit") (print "KW_EXIT"))
				((equal i "load") (print "KW_LOAD"))
				((equal i "disp") (print "KW_DISP"))
				((equal i "true") (print "KW_TRUE"))
				((equal i "false") (print "KW_FALSE"))
				((equal i "+") (print "OP_PLUS"))
				((equal i "-") (print "OP_MINUS"))
				((equal i "/") (print "OP_DIV"))
				((equal i "*") (print "OP_MULT"))
				((equal i "(") (print "OP_OP"))
				((equal i ")") (print "OP_CP"))
				((equal i "**") (print "OP_DBLMULT"))
				((equal i "\"") (print "OP_C"))
				((equal i ",") (print "OP_COMMA"))
				((equal i ";;") (print "COMMENT"))
				((and (char>= (char i 0) #\1) (char<= (char i 0) #\9)) (progn 
					(setf printa 1)
					(loop for j from 1 to (- (length i) 1) do 
						(if (and (char>= (char i j) #\0) (char<= (char i j) #\9)) 
						(setf printa 1) 
						(progn 
							(setf printa 0)
							(setq j (+ (length i) 1))
						)
						)
					) 
					(if (equal printa 1) 
					(print "VALUE")
					(print "SYNTAX ERROR. Identfier must not start with numbers.")
					)
					)
				)
				((equal i "0") (print "VALUE"))
				((or (and (char>= (char i 0) #\a) (char<= (char i 0) #\z))(and (char>= (char i 0) #\A) (char<= (char i 0) #\Z))(equal (char i 0) #\_) )(print "IDENTIFIER"))
				(t (print "SYNTAX ERROR."))
		)
	)	
	)
)

(defun main()
	(gppinterpreter "test.gpp")
)

(main)