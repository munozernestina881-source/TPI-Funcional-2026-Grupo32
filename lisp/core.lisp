;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion (color-actual cambiar-a)
  (cond
    ((and (eq color-actual 'en-rojo) (eq cambiar-a 'rojo-intermitente))
     (list 'en-rojo "cambiar-a-rojo-intermitente"))

    ((and (eq color-actual 'rojo-intermitente) (eq cambiar-a 'verde))
     (list 'rojo-intermitente "cambiar-a-verde"))

    ((and (eq color-actual 'en-verde) (eq cambiar-a 'verde-intermitente))
     (list 'en-verde "cambiar-a-verde-intermitente"))

    ((and (eq color-actual 'verde-intermitente) (eq cambiar-a 'amarillo))
     (list 'verde-intermitente "cambiar-a-amarillo"))

    ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'amarillo-intermitente))
     (list 'en-amarillo "cambiar-a-amarillo-intermitente"))

    ((and (eq color-actual 'amarillo-intermitente) (eq cambiar-a 'rojo))
     (list 'amarillo-intermitente "cambiar-a-rojo"))

    (t (list color-actual 'accion-por-defecto))))

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun timer (timestamp)
  (let ((posicion (mod timestamp 225)))
    (cond
      ((< posicion 90) 'rojo)

      ((< posicion 93) 'rojo-intermitente)

      ((< posicion 213) 'verde)

      ((< posicion 216) 'verde-intermitente)

      ((< posicion 222) 'amarillo)

      (t 'amarillo-intermitente))))

;; ========================================================
;; FUNCIÓN: formatoFecha
;; NATURALEZA: Pura
;; ESTRATEGIA: Directa
;; IMPACTO: No destructiva
;; ========================================================
(defun formatoFecha (tiempo)
  (let ((fecha-actual (local-time:now)))
    (let ((segundos (local-time:timestamp+ fecha-actual tiempo :sec)))
      (local-time:format-timestring nil segundos))))

;; ========================================================
;; FUNCIÓN: formatoColor
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================
(defun formatoColor (listaColor)
  (let ((primerColor (car listaColor)) 
        (segundoColor (cadr listaColor)))
    (cond 
      ((and (equal primerColor 'en-verde) (equal segundoColor "cambiar-a-verde-intermitente")) (list 'VERDE 'VERDE-INTERMITENTE))
      ((and (equal primerColor 'en-verde-intermitente) (equal segundoColor "cambiar-a-amarillo")) (list 'VERDE-INTERMITENTE 'AMARILLO))
      ((and (equal primerColor 'en-amarillo) (equal segundoColor "cambiar-a-amarillo-intermitente")) (list 'AMARILLO 'AMARILLO-INTERMITENTE))
      ((and (equal primerColor 'en-amarillo-intermitente) (equal segundoColor "cambiar-a-rojo")) (list 'AMARILLO-INTERMITENTE 'ROJO))
      ((and (equal primerColor 'en-rojo) (equal segundoColor "cambiar-a-rojo-intermitente")) (list 'ROJO 'ROJO-INTERMITENTE))
      ((and (equal primerColor 'en-rojo-intermitente) (equal segundoColor "cambiar-a-verde")) (list 'ROJO-INTERMITENTE 'VERDE))
      (t (list primerColor 'ACCION-POR-DEFECTO)))))

;; ========================================================
;; FUNCIÓN: calcularTiempo
;; NATURALEZA: Impura
;; ESTRATEGIA: Secuencial
;; IMPACTO: No destructiva
;; ========================================================
(defun calcularTiempo (registro stream)
  (let ((listaColor (formatoColor (transicion (cadr registro) (timer (car registro))))))
    (format stream "~% ~a - Transición: ~a -> ~a" (formatoFecha (car registro)) (car listaColor) (cadr listaColor))))

;; ========================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura (Crea, abre y modifica un archivo en disco)
;; ESTRATEGIA: Secuencial / Iterativa (Mapeo de lista)
;; IMPACTO: No destructiva
;; ========================================================
(defun informe (datos) 
 (with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output) 
   (format stream "Informe de Ejecución del Sistema Semafórico~%") 
   (format stream "=========================================~%")
   (mapcar (lambda (registro) (calcularTiempo registro stream)) datos)
   (format stream "~% --- Fin del Informe ---")))

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Impura
;; ESTRATEGIA: Secuencial
;; IMPACTO: No destructiva
;; ========================================================
(defun duracion-ciclo (tiempo cicloEstandar)
  (let ((ciclosTotales (floor (/ tiempo cicloEstandar))))
    (format t "en ~a segundos se realizan ~a ciclos completos" tiempo ciclosTotales)
    (format t "~%Recomendacion: ~a~%" (recomendacion-ciclo cicloEstandar))))

;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================
(defun recomendacion-ciclo (cicloEstandar)
  (cond ((< cicloEstandar 35) "El ciclo es muy corto. Aumentar la duracion del ciclo")
        ((> cicloEstandar 150) "El ciclo es demasiado largo. Disminuir la duracion del ciclo")
        (t "El ciclo esta en la duracion optima")))

;; ============================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Funcional
;; IMPACTO: No destructiva
;; ============================================================

(defun ciclos-por-tiempo (minutos) 
  "Calcula la cantidad de ciclos completos que ocurren en un tiempo dado en minutos."
  (let* ((segundos-totales (* minutos 60)) 
         (duracion-ciclo 216)) ; 90s(Rojo) + 6s(Amarillo) + 120s(Verde) 
    (floor (/ segundos-totales duracion-ciclo)))) 

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
