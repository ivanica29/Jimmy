(defun start()
	(kreiraj_globalne_promenljive)
  (unos_dimenzija)
  (izbor_redosleda)
        (defvar tabla)
  (setq tabla (crtajTablu n n n '0 nizSlova))

  ;;(defvar niz)
 ;; (setq niz (stampajniz n nizSlova '0))
	)
(defun kreiraj_globalne_promenljive()
  (defvar n)
  (defvar nizSlova)
  (setq nizSlova '(A B C D I F G H I J K L M N O P Q R S T U V W X Y Z))

   (defvar nizBrojeva)
  (setq nizBrojeva '(1 2 3 4 5 6)) 
   (defvar prvi_igrac)
  (defvar player)
    	(setq player '0)
   (defvar lista1) 
	)
(defun unos_dimenzija()
	(format t "Unesite velicinu sestougla.~%")
  (setq n (read))
  )

(defun stampajBlanko(br)
	(cond ((equal br 0) (format t '""))
			((> br 0) (format t " ") (stampajBlanko(- br 1)))
	)
)
(defun stampajCrtice(br)
	(cond ((equal br 0) (format t "~%"))
			((> br 0) (format t "_ ") (stampajCrtice(- br 1)))
	)
)
;;ko prvi igra
(defun izbor_redosleda()
    (format t "Unesite 0 ako zelite da prvi igra racunar ili unesite 1 ako Vi zelite da igrate prvi.~%")
      (setq prvi_igrac (read))
       (cond
		((equal prvi_igrac 0) (progn (setf player '0)(igraj 0)))
		((equalp prvi_igrac 1) (progn (setf player '0)(igraj 1)))
                (t (format t "Pogresno ste uneli. ~%")(izbor_redosleda) )
        )
)


(defun igraj (el)
  (progn (if (equal player '0)  (format t "Igra X ~%") (format t "Igra O ~%") )
     (cond ((equalp el '0)(format t "Racunar igra.~%"))
           (t (format t "Covek igra.~%")))
  )
) 

;;(stampajBrojeve nizBrojeva)

(defun crtajTablu(br dimBlanko dimCrtice n niz)
  (defvar lista1)
    (setq lista1 '())
  (cond ((equal n (-(* br 2) 1)))
       (
        (if(< n (- br 1)) (list (append (list(stampajSlovo(car niz))) (list(cons (stampajBlanko (+ dimBlanko 1)) (stampajCrtice dimCrtice) )))
                                (list(crtajTablu br (- dimBlanko 1) (+ dimCrtice 1) (+ n 1) (cdr niz))))
          
          ((setq lista1 (list  (append (list(stampajSlovo(car niz)))(list(cons (stampajBlanko (+ dimBlanko 1)) (stampajCrtice dimCrtice))))
                                (list(crtajTablu br (+ dimBlanko 1) (- dimCrtice 1) (+ n 1) (cdr niz))))
          ) (format t "~a" lista1))

        )
       
        )

     ;;   (stampaj lista1)        
        )


  )
  
(defun stampajSlovo(slovo)
	 (format t "~a" slovo)
  )

(start)
 ;;(defun stampajBrojeve(broj)
  ;;(cond((null broj) '())
;;	 (t(print (car broj) (stampajBrojeve (cdr broj))))
  ;;      )
    ;;    )
   



