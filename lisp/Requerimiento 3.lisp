;; ========================================================
;; FUNCIÓN: calcularTiempo
;; NATURALEZA: Impura
;; ESTRATEGIA: Codicional
;; IMPACTO: No destructiva
;; ========================================================
(defun calcularTiempo (tiempo color-actual)
(let ((listaColor (transicion color-actual (timer tiempo))))
(format t "~%Tiempo ~a: la luz ha cambiado de ~a a ~a" 
            tiempo 
            color-actual 
            (cadr listaColor))))