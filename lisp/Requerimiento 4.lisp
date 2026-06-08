;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Impura
;; ESTRATEGIA: Secuencial
;; IMPACTO: No destructiva
;; ========================================================
(defun duracion-ciclo (tiempo)
  (let ((ciclosTotales (floor (/ tiempo 216))))
    (format t "en ~a segundos se realizan ~a ciclos completos" tiempo ciclosTotales)))

;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================
(defun recomendacion-ciclo (duracionCiclo)
  (cond ((< duracionCiclo 35) "El ciclo es muy corto. Aumentar la duracion del ciclo")
        ((> duracionCiclo 150) "El ciclo es demasiado largo. Disminuir la duracion del ciclo")
        (t "El ciclo esta en la duracion optima")))