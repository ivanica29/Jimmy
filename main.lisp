(defun start()
	(kreirajGlobalnePromenljive)
 	(unosDimenzija)
 	(izborPrvogIgraca)
	
  	(setq trenutnoStanjeTable (generisiTablu dimenzije 0))
	(stampajPrviRed 0)
	(stampajTablu trenutnoStanjeTable	)
	
	(setq trenutnoStanjeX (generisiTabluXO dimenzije 0 'X))
	(stampajPrviRed 0)
	(stampajTabluXO trenutnoStanjeX 'X)
	
	(setq trenutnoStanjeO (generisiTabluXO dimenzije 0 'O))
	(stampajPrviRed 0)
	(stampajTabluXO trenutnoStanjeO 'O)
	
	;;dodato za cvorove
	(setq listaCvorova (listaSvihCvorova))
	 (format t "~a " listaCvorova)
	 (format t "~%")
	 
	(igraj prviIgrac)
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

(defun dodajZnak (br s) 
	(cond ((equal 0 br) '())
		((if (equal s 'X) (append (list 'X) (dodajZnak (- br 1) s)) '()))
		((if (equal s 'O) (append (list 'O) (dodajZnak (- br 1) s)) '()))
		(t (append (list s) (dodajZnak (- br 1) s)))
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
			(t (append (dodajSlovo red) (dodajNule (- (* dimenzije 2) br)) (dodajCrte br ) (dodajPoslednjiBroj red)))
		)
)

(defun generisiRedXO(br red s)
		(cond
			((if (equal s 'X) (append (dodajSlovo red) (dodajZnak br 'X))))
			((if (equal s 'O) (append (dodajSlovo red) (dodajZnak br 'O))))
		(t (append (dodajSlovo red)(dodajZnak br s)))
		)
)

(defun generisiTablu(n tmp)
	(cond ((equal tmp (- (* dimenzije 2) 1)) '())
		((equal tmp (- dimenzije 1)) (append (list (generisiRed n tmp)) (generisiTablu (- n 1) (+ 1 tmp))))
		((< tmp (- dimenzije 1)) (append (list (generisiRed n tmp)) (generisiTablu (+ n 1) (+ 1 tmp))))
		((> tmp (- dimenzije 1)) (append (list (generisiRed n tmp)) (generisiTablu (- n 1) (+ 1 tmp))))
	)
)

(defun generisiTabluXO(n tmp s)
	(cond 
		((equal tmp (- (* dimenzije 2) 1)) '())
		((and(equal tmp (- dimenzije 1))(equal s 'X)) (append (list (generisiRedXO n tmp 'X)) (generisiTabluXO (- n 1) (+ 1 tmp) s)))
		((and(< tmp (- dimenzije 1))(equal s 'X)) (append (list (generisiRedXO n tmp 'X)) (generisiTabluXO (+ n 1) (+ 1 tmp) s)))
		((and(> tmp (- dimenzije 1))(equal s 'X)) (append (list (generisiRedXO n tmp 'X)) (generisiTabluXO (- n 1) (+ 1 tmp) s)))
		
		((and(equal tmp (- dimenzije 1))(equal s 'O)) (append (list (generisiRedXO n tmp 'O)) (generisiTabluXO (- n 1) (+ 1 tmp) s)))
		((and(< tmp (- dimenzije 1))(equal s 'O)) (append (list (generisiRedXO n tmp 'O)) (generisiTabluXO (+ n 1) (+ 1 tmp) s)))
		((and(> tmp (- dimenzije 1))(equal s 'O)) (append (list (generisiRedXO n tmp 'O)) (generisiTabluXO (- n 1) (+ 1 tmp) s)))
	)
)

(defun stampajRed(l)
	(cond ((null (car l)) (format t "~%"))
			((equal 0 (car l)) (format t " ") (stampajRed (cdr l)))
			((equal '_ (car l)) (format t "_ ") (stampajRed (cdr l)))
			(t (format t "~a" (car l)) (stampajRed (cdr l)))
	)
)

(defun stampajRedXO(l s)
	(cond ((null (car l)) (format t "~%"))
			((and(equal 0 (car l))(equal s 'X)) (format t " ") (stampajRedXO (cdr l) s))
			((and(equal 'X (car l))(equal s 'X)) (format t "X ") (stampajRedXO (cdr l) s))
			
			((and(equal 0 (car l))(equal s 'O)) (format t " ") (stampajRedXO (cdr l) s))
			((and(equal 'O (car l))(equal s 'O)) (format t "O ") (stampajRedXO (cdr l) s))
			
		(t (format t "~a" (car l)) (stampajRedXO (cdr l) s))
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

(defun stampajTabluXO (l s)
	(cond ((null (car l)))
		((equal s 'X) (stampajRedXO (car l) 'X) (stampajTabluXO (cdr l) s))
		((equal s 'O) (stampajRedXO (car l) 'O)(stampajTabluXO (cdr l) s))
		
	(t (stampajRedXO (car l) s) (stampajTabluXO (cdr l) s))
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
(defun validanPotez()
	 (cond
	 	((not (proveriValidnostReda trenutnoStanjeTable)) (format t "Nepostojeci red!~%"))
	 	(t (and ( and (if(< (nth 0 trenutnaKolona) 0) nil t) (if (> (nth 0 trenutnaKolona) (+ (nth 0 trenutniRed) (- dimenzije 1))) nil t))
		(if(not (praznoMesto)) (format t "Mesto je zauzeto!~%") t)))
	 )
 )
 (defun validanPotezX()
	 (cond
	 	((not (proveriValidnostReda trenutnoStanjeX)) (format t "Nepostojeci red!~%"))
	 	(t (and ( and (if(< (nth 0 trenutnaKolona) 0) nil t) (if (> (nth 0 trenutnaKolona) (+ (nth 0 trenutniRed) (- dimenzije 1))) nil t))
		(if(not (praznoMestoX)) (format t "Mesto je zauzeto!~%") t)))
	 )
 )
 (defun validanPotezO()
	 (cond
	 	((not (proveriValidnostReda trenutnoStanjeO)) (format t "Nepostojeci red!~%"))
	 	(t (and ( and (if(< (nth 0 trenutnaKolona) 0) nil t) (if (> (nth 0 trenutnaKolona) (+ (nth 0 trenutniRed) (- dimenzije 1))) nil t))
		(if(not (praznoMestoO)) (format t "Mesto je zauzeto!~%") t)))
	 )
 )

(defun praznoMesto()
	(cond
		((< (nth 0 trenutniRed) (- dimenzije 1)) (if (equal (nth (+ 1 (+ (nth 0 trenutnaKolona) (- dimenzije (nth 0 trenutniRed)))) (nth (nth 0 trenutniRed) trenutnoStanjeTable)) '_ ) t nil ))
		((>= (nth 0 trenutniRed) (- dimenzije 1)) (if (equal (nth (+ 2 (nth 0 trenutnaKolona)) (nth (nth 0 trenutniRed) trenutnoStanjeTable)) '_ ) t nil ))
	)
)
(defun praznoMestoX()
	(cond
		((< (nth 0 trenutniRed) (- dimenzije 1)) (if (equal (nth (+ 1 (+ (nth 0 trenutnaKolona) (- dimenzije (nth 0 trenutniRed)))) (nth (nth 0 trenutniRed) trenutnoStanjeX)) 'X ) t nil ))
		((>= (nth 0 trenutniRed) (- dimenzije 1)) (if (equal (nth (+ 2 (nth 0 trenutnaKolona)) (nth (nth 0 trenutniRed) trenutnoStanjeX)) 'X ) t nil ))
	)
)
(defun praznoMestoO()
	(cond
		((< (nth 0 trenutniRed) (- dimenzije 1)) (if (equal (nth (+ 1 (+ (nth 0 trenutnaKolona) (- dimenzije (nth 0 trenutniRed)))) (nth (nth 0 trenutniRed) trenutnoStanjeO)) 'O ) t nil ))
		((>= (nth 0 trenutniRed) (- dimenzije 1)) (if (equal (nth (+ 2 (nth 0 trenutnaKolona)) (nth (nth 0 trenutniRed) trenutnoStanjeO)) 'O ) t nil ))
	)
)

(defun proveriValidnostReda(l)
	(cond 
		((> (nth 0 trenutniRed) (- (* dimenzije 2) 2) ) '())
		((listp (member (car potez) (car l))) t)
		(t (proveriValidnostReda (cdr l)))
	)
)

; ===================
; Igraj
;====================

(defun upisiPotez(s)
	(cond
		((not (validanPotez)) (format t "Nevalidan potez, pokusajte ponovo. ~%") (igraj igracNaRedu))
		((< (nth 0 trenutniRed) (- dimenzije 1)) (setf (nth (+ 1 (+ (nth 0 trenutnaKolona) (- dimenzije (nth 0 trenutniRed)))) (nth (nth 0 trenutniRed) trenutnoStanjeTable)) s ))
		((>= (nth 0 trenutniRed) (- dimenzije 1)) (setf (nth (+ 2 (nth 0 trenutnaKolona)) (nth (nth 0 trenutniRed) trenutnoStanjeTable)) s ))
	)
	(stampajPrviRed 0)
	(stampajTablu trenutnoStanjeTable)
)
 (defun upisiPotezX(s)
	(cond
		((not (validanPotezX)) (format t "Nevalidan potez, pokusajte ponovo. ~%") (igraj igracNaRedu))
		((< (nth 0 trenutniRed) (- dimenzije 1)) (setf (nth (+ 1 (+ (nth 0 trenutnaKolona) (- dimenzije (nth 0 trenutniRed)))) (nth (nth 0 trenutniRed) trenutnoStanjeX)) s ))
		((>= (nth 0 trenutniRed) (- dimenzije 1)) (setf (nth (+ 2 (nth 0 trenutnaKolona)) (nth (nth 0 trenutniRed) trenutnoStanjeX)) s ))
	)
	(stampajPrviRed 0)
	(stampajTabluXO trenutnoStanjeX 'X)
)
 (defun upisiPotezO(s)
	(cond
		((not (validanPotezO)) (format t "Nevalidan potez, pokusajte ponovo. ~%") (igraj igracNaRedu))
		((< (nth 0 trenutniRed) (- dimenzije 1)) (setf (nth (+ 1 (+ (nth 0 trenutnaKolona) (- dimenzije (nth 0 trenutniRed)))) (nth (nth 0 trenutniRed) trenutnoStanjeO)) s ))
		((>= (nth 0 trenutniRed) (- dimenzije 1)) (setf (nth (+ 2 (nth 0 trenutnaKolona)) (nth (nth 0 trenutniRed) trenutnoStanjeO)) s ))
	)
	(stampajPrviRed 0)
	(stampajTabluXO trenutnoStanjeO 'O)
)

(defun nadjiRed(slovo)
	(cond
		(t (cdr (assoc slovo nizVrednostiSlova)))
	)
)
(defun dodajPotez(potez s)
	(setq trenutniRed (nadjiRed (car potez)))
	(setq trenutnaKolona (cdr potez))
	(upisiPotez s)
)
	
(defun dodajPotezX(potez s)
	(setq trenutniRed (nadjiRed (car potez)))
	(setq trenutnaKolona (cdr potez))
	(upisiPotezX s)
)
(defun dodajPotezO(potez s)
	(setq trenutniRed (nadjiRed (car potez)))
	(setq trenutnaKolona (cdr potez))
	(upisiPotezO s)
)

(defun dodajCovekovPotezUTrenutnoStanje(potez)
	(cond
		((if (equal igracNaRedu prviIgrac) (dodajPotez potez '"X ") (dodajPotez potez '"O ")))
	)
)
(defun dodajCovekovPotezUTrenutnoStanjeX(potez)
	(cond
		((if (equal igracNaRedu prviIgrac) (dodajPotezX potez '"1 ") (dodajPotezX potez '"-1 ")))
	)
)
(defun dodajCovekovPotezUTrenutnoStanjeO(potez)
	(cond
		((if (equal igracNaRedu prviIgrac) (dodajPotezO potez '"-1 ") (dodajPotezO potez '"1 ")))
	)
)

(defun covekIgra()
	(format t "Unesite potez: ")
		(setq potez (list (read) (read)))
	(dodajCovekovPotezUTrenutnoStanje potez)
	(dodajCovekovPotezUTrenutnoStanjeX potez)
	(dodajCovekovPotezUTrenutnoStanjeO potez)
	(setq igracNaRedu 'racunar)
	(racunarIgra)
)

(defun racunarIgra()
	(format t "Unesite potez: ")
	(defvar potez)
		(setq potez (list (read) (read)))
	(dodajCovekovPotezUTrenutnoStanje potez)
	(dodajCovekovPotezUTrenutnoStanjeX potez)
	(dodajCovekovPotezUTrenutnoStanjeO potez)

	(setq igracNaRedu 'covek)
	(covekIgra)
)

(defun igraj(prvi)
	(cond
		((equal prvi 'racunar) (racunarIgra))
		((equal prvi 'covek) (covekIgra))
	)
)
;dodato za cvorove
;;u zavisnosti od prosledjenog reda pronalazi slovo u nizu
(defun nadjiSlovo(red)
	(cond
		(t (car(nth red nizVrednostiSlova)))
	)
)
;primer za 6x6
 (defun listaKrajnjihCvorova()
	(setq listaKrajnjih
	(list (list 'A '0) ;;(A 0)
		  (list 'A (- dimenzije 1)) ;;(A 5)
		  (list (nadjiSlovo(- dimenzije 1)) '0) ;;(F 0)
		  (list (nadjiSlovo(- dimenzije 1)) (-(* dimenzije 2) 2) ) ;;(F 10)
	      (list (nadjiSlovo(-(* dimenzije 2) 2)) (- dimenzije 1)) ;;(K 5)
		  (list (nadjiSlovo(-(* dimenzije 2) 2)) (- (* dimenzije 2) 2)) ;;(K 10)
	)
	)
) 
(defun listaSvihCvorova()
		(list
		(listaCvorova_1 0 1 0)
		(listaCvorova_2 0 dimenzije 0)		
		(listaCvorova_3 1 0 0)	
		(listaCvorova_4 dimenzije 1 0)
		(listaCvorova_5 1 dimenzije 0)	
		(listaCvorova_6 dimenzije (-(* 2 dimenzije) 2) 0)
		
		)
)
;prvi red
(defun listaCvorova_1(tRed tKolona brojac)
		(cond ((>= brojac (- dimenzije 2)) '())
		((< brojac (- dimenzije 2))
			(cons
			(list (nadjiSlovo tRed) (+ brojac tKolona)) (listaCvorova_1 0 1 (+ brojac 1))
			))
		)
)
;poslednji red
(defun listaCvorova_2(tRed tKolona brojac)
	(cond ((>= brojac (- dimenzije 2)) '())
	((< brojac (- dimenzije 2))
		(cons
		(list (nadjiSlovo (- (* 2 dimenzije) 2)) (+ tKolona brojac)) (listaCvorova_2 0 dimenzije (+ brojac 1))
		)
	)
))
;B0-E0
(defun listaCvorova_3(tRed tKolona brojac)
		(cond ((>= brojac (- dimenzije 2)) '())
		((< brojac (- dimenzije 2))
			(cons
			(list (nadjiSlovo (+ tRed brojac)) tKolona) (listaCvorova_3 1 0 (+ brojac 1))
			))
		)
)
;g1-j4
(defun listaCvorova_4(tRed tKolona brojac)
		(cond ((>= brojac (- dimenzije 2)) '())
		((< brojac (- dimenzije 2))
			(cons
			(list (nadjiSlovo (+ tRed brojac)) (+ brojac tKolona)) (listaCvorova_4 dimenzije 1 (+ brojac 1))
			))
		)
)
;b6-e9
(defun listaCvorova_5(tRed tKolona brojac)
		(cond ((>= brojac (- dimenzije 2)) '())
		((< brojac (- dimenzije 2))
			(cons
			(list (nadjiSlovo (+ tRed brojac)) (+ brojac tKolona)) (listaCvorova_5 1 dimenzije (+ brojac 1))
			))
		)
)
;f10-j10
(defun listaCvorova_6(tRed tKolona brojac)
		(cond ((>= brojac (- dimenzije 2)) '())
		((< brojac (- dimenzije 2))
			(cons
			(list (nadjiSlovo (+ tRed brojac)) tKolona) (listaCvorova_6 dimenzije (-(* 2 dimenzije) 2) (+ brojac 1))
			))
		)
)


(start)