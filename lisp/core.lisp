;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional (case)
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion (color-actual cambiar-a)
  (cond
    ((and (eq color-actual 'en-rojo)
          (eq cambiar-a 'verde))
     (list 'en-rojo "cambiar-a-verde"))

    ((and (eq color-actual 'en-verde)
          (eq cambiar-a 'amarillo))
     (list 'en-verde "cambiar-a-amarillo"))

    ((and (eq color-actual 'en-amarillo)
          (eq cambiar-a 'rojo))
     (list 'en-amarillo "cambiar-a-rojo"))

    (t
     (list color-actual 'accion-por-defecto))))



;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun timer (timestamp)
  (let ((posicion (mod timestamp 216)))
    (cond
      ((< posicion 90) 'rojo)
      ((< posicion 96) 'amarillo)
      (t 'verde))))

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

;; NATURALEZA: Pura
;; ESTRATEGIA: Funcional
;; ============================================================

(defun ciclos-por-tiempo (minutos) ; definimos la funcion + parámetro
  "Calcula la cantidad de ciclos completos que ocurren en un tiempo dado en minutos."
  (let* ((segundos-totales (* minutos 60)) ; variable local - pasar el tiempo a segundos
         (duracion-ciclo 216)) ; 90s(Rojo) + 6s(Amarillo) + 120s(Verde) ; define constante fija con el tiempo total del ciclo
    (floor (/ segundos-totales duracion-ciclo)))) ; divide el total de segundos por lo que dura un ciclo - floor redondea hacia abajo
