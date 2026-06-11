;; =======================================================
;; FUNCIÓN: informe-por-hora
;; NATURALEZA: Pura 
;; ESTRATEGIA: Evaluación simple / secuencial 
;; IMPACTO: No destructiva
;; =======================================================
(defun informe-por-hora ()
  (let ((total-rojo (* 90 16))
        (total-amarillo (* 6 16))
        (total-verde (* 120 16)))

    (let ((tiempo-total (+ total-rojo total-amarillo total-verde)))
      
      (list 'rojo '%(* (/ total-rojo tiempo-total) 100.0)
            'amarillo '%(* (/ total-amarillo tiempo-total) 100.0)
            'verde '%(* (/ total-verde tiempo-total) 100.0)))))