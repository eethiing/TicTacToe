;;(userInput)
(defun player1()	
    (format t "Welcome to TicTacToe!~%")
    (format t "~%Player 1, enter marker : ")
    (setq marker1(read))
    (format t "Player 1, Marker : ~a ~%" marker1))

(defun player2()
    (format t " ")
    (format t "~%Player 2, enter marker : ")
    (setq marker2(read))
    (format t "Player 2, Marker : ~a ~%" marker2))

(defun generateBoard()
	(setq Board (make-array 9 :initial-contents'(1 2 3 4 5 6 7 8 9))))
	
;;array starts from 0
(defun boxVal (p)
  (aref Board (- p 1)))

;;working
(defun showBoard ()
  (setq a 1)(setq b 2)(setq c 3)
  (dotimes (x 3)
		(format t "~%")
		(format t "     |     |     ~%")
		(format t "   ~a |   ~a |   ~a  ~%" (boxVal a) (boxVal b) (boxVal c))
		(format t "     |     |     ~%")
        (format t "____________________~%")
    (incf a 3)(incf b 3)(incf c 3)))
	

(defun winGame ()
	(or
	;;check for row 
	(and (equal (boxVal 1) (boxVal 2)) (equal (boxVal 2) (boxVal 3)))
	(and (equal (boxVal 4) (boxVal 5)) (equal (boxVal 5) (boxVal 6)))
	(and (equal (boxVal 7) (boxVal 8)) (equal (boxVal 8) (boxVal 9)))
	
	;;check for columns
	(and (equal (boxVal 1) (boxVal 4)) (equal (boxVal 4) (boxVal 7)))
	(and (equal (boxVal 2) (boxVal 5)) (equal (boxVal 5) (boxVal 8)))
	(and (equal (boxVal 3) (boxVal 6)) (equal (boxVal 6) (boxVal 9)))
	
	;check for diagonals
	(and (equal (boxVal 1) (boxVal 5)) (equal (boxVal 5) (boxVal 9)))
	(and (equal (boxVal 3) (boxVal 5)) (equal (boxVal 5) (boxVal 7)))))

(defun drawGame ()
	(and
	;;check whether the elements are number
		(not (winGame))
		(not (numberp (boxVal 1)))
		(not (numberp (boxVal 2)))
		(not (numberp (boxVal 3)))
		(not (numberp (boxVal 4)))
		(not (numberp (boxVal 5)))
		(not (numberp (boxVal 6)))
		(not (numberp (boxVal 7)))
		(not (numberp (boxVal 8)))
		(not (numberp (boxVal 9)))))
	

;;choice can be any integers from 1-9
(defun selectBox()
	(loop 
		(showBoard)
		(checkWin)
		(checkDraw)
		(format t "Please enter position : ")
		(let ((choice (parse-integer (read-line) :junk-allowed t)))
			;;check integer
			(cond 
				((not choice)
				(progn
					(format t "Only integer value from 1 to 9 is accepted.~%") 
					(selectBox)))
					
				((and(< choice 10)(> choice 0)
					(checkBox choice)))
					
				((and(< choice 1)(> choice 9)
					(progn
					(format t "Invalid Choice. Please select integer 1-9.~%")))
					(selectBox))))))

;;check whether position has marker or not
;;force-output initiates the emptying of any internal buffers but does not wait for completion or acknowledgment to return.
(defun checkBox(choice)
	(cond
		((or (equal (boxVal choice) marker1) (equal (boxVal choice) marker2))
		(progn
			(format t "Position taken. Please select another box.~%")
			(force-output nil)
			(checkBox (selectBox))))
		
		((and (not (winGame)) (not (drawGame)))
			(setq actualPos (- choice 1))
			(if (evenp counter)
				(setf (aref Board actualPos) marker1)
				(setf (aref Board actualPos) marker2))
				(incf counter))))	

(defun checkWin()
	(cond
		((winGame)
			(progn 
				(decf counter)
				(if (evenp counter)
					(format t "Player1 has won! ")
					(format t "Player2 has won! "))
					(playAgain)))))

(defun checkDraw()
	(cond 
		((drawGame)
			(progn
				(format t "Is a tie!")
				(playAgain)))))

(defun end()
	(format t "Thanks for playing! Goodbye!")
	(exit))
	
				
(defun playAgain()
	(format t "~%")
	(format t "Would you like to play again?[yes/no] ~%")
	(setq answer(read))
	(if (string-equal answer "yes")
		(startGame)
	  (end)))
	
;;main
(defun startGame()
	(setq counter 0)
	(player1)
	(player2)
	(format t "Player 1 will start first.")
	(generateBoard)
	(selectBox))

(startGame)