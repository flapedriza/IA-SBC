
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

(defmessage-handler Plato imprimir primary ()
  (format t "%s, plato %s de %s Precio: %s€ %n" ?self:nombre ?self:temperatura ?self:tipo ?self:precio)
  (printout t crlf)
)

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
  (format t "Precio: %g€" ?self:precio)
  (printout t crlf)
)

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
  (send ?self:bebida2 imprimir)
  (printout t "-------------------------------" crlf)
)


(deffunction pregunta-text(?pregunta)
  (format t "%s" ?pregunta)
  (bind ?resposta (read))
  ?resposta
)

(deffunction pregunta-num(?pregunta ?min ?max)

)
