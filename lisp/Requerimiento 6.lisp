;; =======================================================
;; FUNCIÓN: informe-por-hora
;; NATURALEZA: Pura 
;; ESTRATEGIA: Evaluación simple / secuencial 
;; IMPACTO: No destructiva
;; =======================================================
(defun informe-por-hora ()
  (let ((total-rojo (* 90 16))
        (total-rojo-inter (* 3 16))
        (total-verde (* 120 16))
        (total-verde-inter (* 3 16))
        (total-amarillo (* 6 16))
        (total-amarillo-inter (* 3 16)))


    (let ((tiempo-total (+ total-rojo total-rojo-inter total-verde total-verde-inter total-amarillo total-amarillo-inter)))
      
      (list 'rojo '%(* (/ total-rojo tiempo-total) 100.0)
            'rojo-intermitente '%(* (/ total-rojo-inter tiempo-total) 100.0)
            'verde '%(* (/ total-verde tiempo-total) 100.0)
            'verde-intermitente '%(* (/ total-verde-inter tiempo-total) 100.0)
            'amarillo '%(* (/ total-amarillo tiempo-total) 100.0)
            'amarillo-intermitente '%(* (/ total-amarillo-inter tiempo-total) 100.0)))))
