;;caoooss
(defun stampajBlanko(br)
	(cond ((equal br 0) (format t '""))
			((> br 0) (format t " ") (stampajBlanko(- br 1)))
	)
)

(defun stampajBlankoObrnuto(br)
	(cond ((equal br 0) (format t '""))
			((> br 0) (format t " ") (stampajBlanko(+ br 1)))
	)
)

(defun stampajCrtice(br)
	(cond ((equal br 0) (format t "~%"))
			((> br 0) (format t "_ ") (stampajCrtice(- br 1)))
	)
)

(defun stampajCrticeObrnuto(br)
	(cond ((equal br 0) (format t "~%"))
			((> br 0) (format t "_ ") (stampajCrtice(- br 1)))
	)
)

;;; dgtedtt

(defun crtajTablu(dimBlanko dimCrtice n)
	(cond ((equal n 11))
		((< n 6) (stampajBlanko dimBlanko) (stampajCrtice dimCrtice) (crtajTablu (- dimBlanko 1) (+ dimCrtice 1) (+ 1 n)))
		((equal n 6) (stampajBlanko 2) (stampajCrtice 10) (crtajTablu 3 9 (+ 1 n)))
		((> n 6) (stampajBlanko dimBlanko) (stampajCrtice dimCrtice) (crtajTablu (+ dimBlanko 1) (- dimCrtice 1) (+ 1 n)))
	)
)

;;;; (crtajTablu 6 6 0)

(defun crtajElementUKoloni(br kolona)
	(cond ((equal br 0) (format t "X"))
			((equal br kolona) (format t "X ") (crtajElementUKoloni(- br 1) kolona))
			((> br 0) (format t "_ ") (crtajElementUKoloni(- br 1) kolona))
	)
)

(defun crtajElement(dimBlanko dimCrtice red kolona n)
	(cond ((equal n 11))
		((equal n red) (stampajBlanko dimBlanko) (crtajElementUKoloni dimCrtice kolona) () (crtajElement (- dimBlanko 1) (+ dimCrtice 1) red kolona (+ 1 n)))
		((< n 6) (stampajBlanko dimBlanko) (stampajCrtice dimCrtice) (crtajElement (- dimBlanko 1) (+ dimCrtice 1) red kolona (+ 1 n) ))
		((equal n 6) (stampajBlanko 2) (stampajCrtice 10) (crtajElement 3 9  red kolona (+ 1 n)))
		((> n 6) (stampajBlanko dimBlanko) (stampajCrtice dimCrtice) (crtajElement (+ dimBlanko 1) (- dimCrtice 1) red kolona (+ 1 n)))
	)
)

(crtajElement 6 6 0 3 0)