(defun read-file (filename)
    (with-open-file (stream filename)
		(loop for chars = (read-char stream nil)		;;Reads file char-by-char.
		while chars
		collect chars
		)
	)
)

(defun compare-same-lists(list1 list2)
	(setq ret T)
	(loop for i from 0 to (- (length list1) 1)
		do
		(if(equal (nth i list1) (nth i list2))
				()
				(progn 									;;Helper function to compare states.
				(setq ret nil)
				(setq i (+ (length list1) 1))
				)
		)
	)
	(return-from compare-same-lists ret)
)

(defun rule-control(tokens)							;;This function selects the CFG rules.
	(if (listp tokens)
		(progn 
			(setq control (copy-list tokens))
			(loop for i from 0 to (- (length control) 1)			;;Format changer
			do
				(if (listp (nth i control))
				(setf (nth i control) (nth 0 (nth i control)))
				)
			)
		)
		(setq control (list tokens))
	)
	
	(cond 						;;EXPI rules
		((compare-same-lists control (list "OP_OP" "OP_PLUS" "EXPI" "EXPI" "OP_CP")) (return-from rule-control (list "EXPI" (+ (nth 1 (nth 2 tokens)) (nth 1 (nth 3 tokens))))))
		((compare-same-lists control (list "OP_OP" "OP_MINUS" "EXPI" "EXPI" "OP_CP")) (return-from rule-control (list "EXPI" (- (nth 1 (nth 2 tokens)) (nth 1 (nth 3 tokens))))))
		((compare-same-lists control (list "OP_OP" "OP_MULT" "EXPI" "EXPI" "OP_CP")) (return-from rule-control (list "EXPI" (* (nth 1 (nth 2 tokens)) (nth 1 (nth 3 tokens))))))
		((compare-same-lists control (list "OP_OP" "OP_DIV" "EXPI" "EXPI" "OP_CP")) (return-from rule-control (list "EXPI" (/ (nth 1 (nth 2 tokens)) (nth 1 (nth 3 tokens))))))
		((compare-same-lists control (list "OP_OP" "OP_DBLMULT" "EXPI" "EXPI" "OP_CP")) (return-from rule-control (list "EXPI" (expt (nth 1 (nth 2 tokens)) (nth 1 (nth 3 tokens))))))
		((compare-same-lists control (list "OP_OP" "KW_DISP" "EXPI" "OP_CP")) (return-from rule-control (list "EXPI" (print (nth 1 (nth 2 tokens))))))
		((compare-same-lists control (list "OP_OP" "EXPI" "OP_CP")) (return-from rule-control (list "EXPI" (nth 1 (nth 1 tokens)))))
		((compare-same-lists control (list "OP_OP" "KW_IF" "EXPB" "EXPI" "OP_CP")) (return-from rule-control (list "EXPI" (if (nth 1 (nth 2 tokens)) (nth 1 (nth 3 tokens))))))
		((compare-same-lists control (list "OP_OP" "KW_IF" "EXPB" "EXPI" "EXPI" "OP_CP")) (return-from rule-control (list "EXPI" (if (nth 1 (nth 2 tokens)) (nth 1 (nth 3 tokens))(nth 1 (nth 4 tokens))))))
		((and (equal (nth 0 control) "IDENTIFIER") (> (length tokens) 1)) (return-from rule-control (list "EXPI" (nth 2 tokens))))
		((equal (nth 0 control) "VALUE") (return-from rule-control (list "EXPI" (nth 1 tokens))))
								;;EXPB rules
		((compare-same-lists control (list "OP_OP" "KW_AND" "EXPB" "EXPB" "OP_CP")) (return-from rule-control (list "EXPB" (and (nth 1 (nth 2 tokens)) (nth 1 (nth 3 tokens))))))
		((compare-same-lists control (list "OP_OP" "KW_OR" "EXPB" "EXPB" "OP_CP")) (return-from rule-control (list "EXPB" (or (nth 1 (nth 2 tokens)) (nth 1 (nth 3 tokens))))))
		((compare-same-lists control (list "OP_OP" "KW_NOT" "EXPB" "OP_CP")) (return-from rule-control (list "EXPB" (not (nth 1 (nth 2 tokens))))))
		((compare-same-lists control (list "OP_OP" "KW_EQUAL" "EXPB" "EXPB" "OP_CP")) (return-from rule-control (list "EXPB" (equal (nth 1 (nth 2 tokens)) (nth 1 (nth 3 tokens))))))
		((compare-same-lists control (list "OP_OP" "KW_EQUAL" "EXPI" "EXPI" "OP_CP")) (return-from rule-control (list "EXPB" (equal (nth 1 (nth 2 tokens)) (nth 1 (nth 3 tokens))))))
		((compare-same-lists control (list "OP_OP" "KW_LESS" "EXPI" "EXPI" "OP_CP")) (return-from rule-control (list "EXPB" (< (nth 1 (nth 2 tokens)) (nth 1 (nth 3 tokens))))))
		((compare-same-lists control (list "OP_OP" "KW_DISP" "EXPB" "OP_CP")) (return-from rule-control (list "EXPB" (print (nth 1 (nth 2 tokens))))))
		((equal (nth 0 control) "KW_NIL") (return-from rule-control (list "EXPB" nil)))
		((equal (nth 0 control) "KW_TRUE") (return-from rule-control (list "EXPB" T)))
		((equal (nth 0 control) "KW_FALSE") (return-from rule-control (list "EXPB" nil)))

	)
	(return-from rule-control "Error")
)


(defun gppparser (tokens lis)
	(setf jump 0)									
	(setf index 0)
	(loop for i in tokens
	do(progn
		(setf index (+ index 1))
		(if (> jump 0)
			(if (equal "OP_CP" i)
				(setf jump (- jump 1))
				(if (equal "OP_OP" i)
					(setf jump (+ jump 1))
				)					
			)

			(if (equal "OP_CP" i)
				(progn 
				(setq lis (append lis (list i)))
				(return-from gppparser (list (rule-control lis)))			;;My parser.
				)
				(if (equal "OP_OP" i)
					(progn
						(setf tempindex index)
						(setq lis (append lis (gppparser (nthcdr index tokens) (list i) )))
						(setf index tempindex)
						(setf jump (+ jump 1))
					)
					(if (or (listp i) (or (equal "KW_NIL" i) (or (equal "KW_FALSE" i) (equal "KW_TRUE" i))))
						(setq lis (append lis (list (rule-control i))))
						(setq lis (append lis (list i)))
					)	
				)
	    	)
		)	
	  )
	)
	(return-from gppparser lis)
)

(defun gppinterpreter (&optional filename)

	(setq file (read-file filename))
	(setq wordlist '())											;;From my previous HW2 homework.
	(setq charlist '())
	(setq returnlist '())
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
		(cond ((equal i "and")  (setq returnlist (append returnlist (list "KW_AND"))))
				((equal i "or") (setq returnlist (append returnlist (list "KW_OR"))))
				((equal i "not") (setq returnlist (append returnlist (list "KW_NOT"))))
				((equal i "equal") (setq returnlist (append returnlist (list "KW_EQUAL"))))
				((equal i "less") (setq returnlist (append returnlist (list "KW_LESS"))))
				((equal i "nil") (setq returnlist (append returnlist (list "KW_NIL"))))
				((equal i "list") (setq returnlist (append returnlist (list "KW_LIST"))))
				((equal i "append") (setq returnlist (append returnlist (list "KW_APPEND"))))
				((equal i "concat") (setq returnlist (append returnlist (list "KW_CONCAT"))))
				((equal i "set") (setq returnlist (append returnlist (list "KW_SET"))))
				((equal i "deffun") (setq returnlist (append returnlist (list "KW_DEFFUN"))))
				((equal i "for") (setq returnlist (append returnlist (list "KW_FOR"))))
				((equal i "if") (setq returnlist (append returnlist (list "KW_IF"))))
				((equal i "exit") (setq returnlist (append returnlist (list "KW_EXIT"))))
				((equal i "load") (setq returnlist (append returnlist (list "KW_LOAD"))))
				((equal i "disp") (setq returnlist (append returnlist (list "KW_DISP"))))
				((equal i "true") (setq returnlist (append returnlist (list "KW_TRUE"))))
				((equal i "false") (setq returnlist (append returnlist (list "KW_FALSE"))))
				((equal i "+") (setq returnlist (append returnlist (list "OP_PLUS"))))
				((equal i "-") (setq returnlist (append returnlist (list "OP_MINUS"))))
				((equal i "/") (setq returnlist (append returnlist (list "OP_DIV"))))
				((equal i "*") (setq returnlist (append returnlist (list "OP_MULT"))))
				((equal i "(") (setq returnlist (append returnlist (list "OP_OP"))))
				((equal i ")") (setq returnlist (append returnlist (list "OP_CP"))))
				((equal i "**") (setq returnlist (append returnlist (list "OP_DBLMULT"))))
				((equal i "\"") (setq returnlist (append returnlist (list "OP_C"))))
				((equal i ",") (setq returnlist (append returnlist (list "OP_COMMA"))))
				((equal i ";;") (setq returnlist (append returnlist (list "COMMENT"))))
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
					(setq returnlist (append returnlist (list (list "VALUE" (read-from-string i)))))
					(print "SYNTAX ERROR. Identfier must not start with numbers.")
					)
					)
				)
				((equal i "0") (setq returnlist (append returnlist (list (list "VALUE" (read-from-string i))))))
				((or (and (char>= (char i 0) #\a) (char<= (char i 0) #\z))(and (char>= (char i 0) #\A) (char<= (char i 0) #\Z))(equal (char i 0) #\_) )(setq returnlist (append returnlist (list (list "IDENTIFIER" i)))))
				(t (print "SYNTAX ERROR."))
		)
	)	
	)
	(print (setq result (gppparser returnlist '())))				;;Prints reduced form of code.
	(print "The result is:")
	(print (nth 1 (nth 0 result)))
	(return-from gppinterpreter returnlist)
)

(defun main()
	(gppinterpreter "test.gpp")
)

(main)