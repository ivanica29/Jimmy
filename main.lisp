;;caooos
(defun start()
	(kreirajGlobalnePromenljive)
 	(unosDimenzija)
 	(izborPrvogIgraca)
  	(setq trenutnoStanjeTable (generisiTablu dimenzije 0))

	(stampajPrviRed 0)

	(stampajTablu trenutnoStanjeTable	)
	(igraj prviIgrac)
  	;(stampajTablu (generisiTablu dimenzije 0))
 	;(izbor_redosleda)
 	;(defvar tabla)
  	;(setq tabla (crtajTablu n n n '0 nizSlova))
  	;(format t "~a" (nadjiRed 'B))

)

(defun kreirajGlobalnePromenljive()
	(defvar dimenzije)
	(defvar nizSlova)
  		(setq nizSlova '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
  	(defvar nizVrednostiSlova)
  		(setq nizVrednostiSlova '((A 0) (B 1) (C 2) (D 3) (E 4) (F 5) (G 6) (H 7) (I 8) (J 9) (K 10) (L 11) (M 12) (N 13) (O 14) (P 15) (Q 16) (R 17) (S 18) (T 19) (U 20) (V 21) (W 22) (X 23) (Y 24) (Z 25)))
  	(defvar trenutnoStanjeTable)
  	(defvar prviIgrac)
 	(defvar igracNaRedu)
 	(defvar trenutniRed)
 	(defvar trenutnaKolona)
	(defvar potez)
)

; ===================
; Dimenzije
;====================


(defun unosDimenzija()
	(format t "Unesite velicinu sestougla: ")
	(setq dimenzije (read))
	(cond 
		((< dimenzije 6) (format t "Dimenzije nisu validne, moraju da budu u intervalu od 6 do 12. Pokusajte ponovo.") (unosDimenzija) )
		((> dimenzije 12) (format t "Dimenzije nisu validne, moraju da budu u intervalu od 6 do 12. Pokusajte ponovo.") (unosDimenzija) )
		)
)

; ===================
; Sve za iscrtavanje table
;====================

(defun dodajCrte(br)
	(cond ((equal 0 br) '())
		(t (append (list '_) (dodajCrte (- br 1))))
	)
)

(defun dodajNule(br)
	(cond ((equal 0 br) '())
		(t (append (list 0) (dodajNule (- br 1))))
	)
)

(defun dodajSlovo(red)
	(cond
		(t (append (list (nth red nizSlova))))
	)
)

(defun dodajPoslednjiBroj(br)
	(cond ((equal br 0) '())
		((> br (- dimenzije 1)) '())
		(t (list (+ br (- dimenzije 1))))
	)
)

(defun generisiRed(br red)
		(cond
			(t (append (dodajSlovo red) (dodajNule (- (* dimenzije 2) br)) (dodajCrte br) (dodajPoslednjiBroj red)))
		)
)

(defun generisiTablu(n tmp)
	(cond ((equal tmp (- (* dimenzije 2) 1)) '())
		((equal tmp (- dimenzije 1)) (append (list (generisiRed n tmp)) (generisiTablu (- n 1) (+ 1 tmp))))
		((< tmp (- dimenzije 1)) (append (list (generisiRed n tmp)) (generisiTablu (+ n 1) (+ 1 tmp))))
		((> tmp (- dimenzije 1)) (append (list (generisiRed n tmp)) (generisiTablu (- n 1) (+ 1 tmp))))
	)
)

(defun stampajRed(l)
	(cond ((null (car l)) (format t "~%"))
			((equal 0 (car l)) (format t " ") (stampajRed (cdr l)))
			((equal '_ (car l)) (format t "_ ") (stampajRed (cdr l)))
			(t (format t "~a" (car l)) (stampajRed (cdr l)))
	)
)

(defun stampajPrviRed(tmp)
	(cond ((equal tmp (* 2 dimenzije)) (format t "~%"))
		((< tmp dimenzije) (format t " ") (stampajPrviRed (+ 1 tmp)))
		((equal 0 (mod tmp dimenzije)) (format t " 0 ") (stampajPrviRed (+ 1 tmp)))
		((equal 0 (mod dimenzije tmp)) (format t "0 ") (stampajPrviRed (+ 1 tmp)))
		((< tmp (* 2 dimenzije)) (format t "~a " (mod tmp dimenzije)) (stampajPrviRed (+ 1 tmp)))
	)
)

(defun stampajTablu(l)
	(cond ((null (car l)))
			(t (stampajRed (car l)) (stampajTablu (cdr l)))
	)
)

; ===================
; Ko prvi igra
;====================

(defun izborPrvogIgraca()
    (format t "Unesite 0 ako zelite da prvi igra racunar ili unesite 1 ako Vi zelite da igrate prvi:~%")
      (setq prviIgrac (read))
       (cond
		((equal prviIgrac 0) (progn (setf prviIgrac 'racunar) (setf igracNaRedu 'racunar)))
		((equalp prviIgrac 1) (progn (setf prviIgrac 'covek) (setf igracNaRedu 'covek)))
        (t (format t "Pogresno ste uneli prvog igraca, molimo Vas pokusajte ponovo: ~%")(prviIgrac) )
       )
)


; ===================
; Nevalidan potez
;====================

(t (and (and ( and (if(< (nth 0 trenutnaKolona) 0) nil t) (if (> (nth 0 trenutnaKolona) (+ (nth 0 trenutniRed) (- dimenzije 1))) nil t)) (if(not (praznoMesto)) (format t "Mesto je zauzeto!~%") t)) (proveriValidnostReda trenutnoStanjeTable)))


(defun validanPotez()
	(cond
		(t (and (and ( and (if(< (nth 0 trenutnaKolona) 0) nil t) (if (> (nth 0 trenutnaKolona) (+ (nth 0 trenutniRed) (- dimenzije 1))) nil t)) (if(not (praznoMesto)) (format t "Mesto je zauzeto!~%") t)) (proveriValidnostReda trenutnoStanjeTable)))

	)
 )

(defun praznoMesto()
	(cond
		((< (nth 0 trenutniRed) (- dimenzije 1)) (if (equal (nth (+ 1 (+ (nth 0 trenutnaKolona) (- dimenzije (nth 0 trenutniRed)))) (nth (nth 0 trenutniRed) trenutnoStanjeTable)) '_ ) t nil ))
		((>= (nth 0 trenutniRed) (- dimenzije 1)) (if (equal (nth (+ 2 (nth 0 trenutnaKolona)) (nth (nth 0 trenutniRed) trenutnoStanjeTable)) '_ ) t nil ))
	)
)

(defun proveriValidnostReda(l)
	; (format t "~a" (car l))
	; (format t "~a" (car potez))
	; (format t "~a" (listp (member (car potez) (car l))))
	(cond 
		((null (car l)) nil)
		((listp (member (car potez) (car l))) t)
		(t (proveriValidnostReda (cdr l)))
	)
)

; ===================
; Igraj
;====================

(defun upisiPotez(s)
	(cond
		((not (validanPotez)) (format t "Nevalidan potez, pokusajte ponovo. ~%")(igraj igracNaRedu))
		((< (nth 0 trenutniRed) (- dimenzije 1)) (setf (nth (+ 1 (+ (nth 0 trenutnaKolona) (- dimenzije (nth 0 trenutniRed)))) (nth (nth 0 trenutniRed) trenutnoStanjeTable)) s ))
		((>= (nth 0 trenutniRed) (- dimenzije 1)) (setf (nth (+ 2 (nth 0 trenutnaKolona)) (nth (nth 0 trenutniRed) trenutnoStanjeTable)) s ))
	)
	(stampajTablu trenutnoStanjeTable)
)


(defun nadjiRed(slovo)
	(cond
		(t (cdr (assoc slovo nizVrednostiSlova)))
	)
)

(defun dodajPotez(potez s)
	(setq trenutniRed (nadjiRed (car potez)))
	;(format t "~a" trenutniRed)
	(setq trenutnaKolona (cdr potez))
	(upisiPotez s)
)

(defun dodajCovekovPotezUTrenutnoStanje(potez)
	(cond
		((if (equal igracNaRedu prviIgrac) (dodajPotez potez '"X ") (dodajPotez potez '"O ")))
	)
)

(defun covekIgra()
	(format t "Unesite potez: ")
		(setq potez (list (read) (read)))
	(dodajCovekovPotezUTrenutnoStanje potez)

	(setq igracNaRedu 'racunar)
	(racunarIgra)
)

(defun racunarIgra()
	(format t "Unesite potez: ")
	(defvar potez)
		(setq potez (list (read) (read)))
	(dodajCovekovPotezUTrenutnoStanje potez)


	(setq igracNaRedu 'covek)
	(covekIgra)
)

(defun igraj(prvi)
	(cond
		((equal prvi 'racunar) (racunarIgra))
		((equal prvi 'covek) (covekIgra))
	)
)


(start)
