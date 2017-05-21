
;;;*************
;;;Codi CLIPS
;;;*************

;;;Template per a la llista de recomanacions sense ordenar
(deftemplate llista-rec-desordenada
  (multislot recomanacions (type INSTANCE))
)

;;;Template per a la llista de recomanacions ordenada
(deftemplate llista-rec-ordenada
  (multislot recomanacions (type INSTANCE))
)

;;; Message handlers Palto
(defmessage-handler Plato imprimir primary ()
  (format t "%s, plato %s de %s Precio: %g euros %n" ?self:nombre ?self:temperatura ?self:tipo ?self:precio)
  (printout t crlf)
)

(defmessage-handler Plato has-ingredient primary (?ing)
  (bind ?i 1)
  (bind ?b FALSE)
  (while (and (not ?b) (<= ?i (length$ ?self:contiene)))
    (if (eq (str-cat ?ing) (send (nth$ ?i ?self:contiene) get-nombre)) then
      (bind ?b TRUE)
    else (bind ?i (+ ?i 1))
    )
  )
  ?b
)

;;;Message handlers begudes
(defmessage-handler Agua imprimir primary ()
  (format t "Agua %s " ?self:tipo_ag)
  (printout t)
  (send ?self imprecio)
)

(defmessage-handler Refresco imprimir primary ()
  (format t "%s " ?self:tipo_ref)
  (printout t)
  (send ?self imprecio)
)

(defmessage-handler Vino imprimir primary ()
  (format t "Vino %s " ?self:tipo_vin)
  (printout t)
  (send ?self imprecio)
)

(defmessage-handler Cerveza imprimir primary ()
  (format t "Cerveza de %s " ?self:tipo_cer)
  (printout t)
  (send ?self imprecio)
)

(defmessage-handler Bebida imprimir ()
  (printout "Bebida")
)

(defmessage-handler Bebida imprecio  primary ()
  (format t "Precio: %g euros" ?self:precio)
  (printout t crlf)
)

;;; Message handlers Manu
(defmessage-handler Menu imprimir primary ()
  (printout t "-------------------------------" crlf)
  (printout t "Primer plato: ")
  (send ?self:primero imprimir)
  (printout t "Segundo plato: ")
  (send ?self:segundo imprimir)
  (printout t "Postre: ")
  (send ?self:postre imprimir)
  (format t "Bebidas: %n")
  (printout t)
  (send ?self:bebida1 imprimir)
  (if(neq ?self:bebida2 nil) then (send ?self:bebida2 imprimir))
  (printout t crlf "Precio total: " (send ?self calc-precio) "euros" crlf)
  (printout t "-------------------------------" crlf)
)

(defmessage-handler Menu calc-precio primary ()
    (bind ?sum (send ?self:primero get-precio))
    (bind ?sum (+ ?sum (send ?self:segundo get-precio)))
    (bind ?sum (+ ?sum (send ?self:postre get-precio)))
    (bind ?sum (+ ?sum (send ?self:bebida1 get-precio)))
    (if (neq ?self:bebida2 nil) then
      (bind ?sum (+ ?sum (send ?self:bebida2 get-precio)))
    )
    ?sum
)
