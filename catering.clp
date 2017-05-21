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

;;;Utils
(deffunction preu-baix (?min ?max)
  (bind ?diff (- ?max ?min))
  (bind ?diff (/ ?diff 3))
  (bind ?diff (+ ?min ?diff))
  ?diff
)

(deffunction preu-mig (?min ?max)
  (bind ?diff (- ?max ?min))
  (bind ?diff (/ ?diff 3))
  (bind ?diff (+ ?min (* 2 ?diff)))
  ?diff
)

(deffunction preu-alt (?min ?max)
  (bind ?diff (- ?max ?min))
  (bind ?diff (/ ?diff 3))
  (bind ?diff (+ ?min (* 3 ?diff)))
  ?diff
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

;;; Message handlers Menu
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

(defmodule MAIN (export ?ALL))

(defrule prova ""
  (declare (salience 100))
  =>
  (printout t "Hola")
  (assert (start))
  (assert (Vegetariano TRUE))
  (assert (IngredienteProhibido "butifarra blanca"))
  (assert (IngredienteProhibido "tomate"))
  (focus filtre)
)

;;;*********
;;;Filtratge
;;;*********
(defmodule filtre
  (import MAIN ?ALL)
  (export ?ALL)
)

(defrule borra-no-vegetarians "Elimina els plats no vegetarians si el client ho ha demanat"
  (Vegetariano TRUE)
  ?plat <- (object (is-a Plato))
  (test (not (send ?plat get-vegetariano)))
  =>
  (printout t "Eliminado " (send ?plat get-nombre) " por no ser vegetariano" crlf)
  (send ?plat delete)
)

(defrule borra-ingredients-prohibits "Elimina els plats que continguin ingredients prohibits"
  (IngredienteProhibido ?ingr)
  ?plat <- (object (is-a Plato))
  (test (send ?plat has-ingredient ?ingr))
  =>
  (printout t "Eliminado " (send ?plat get-nombre) " por contener " ?ingr crlf)
  (send ?plat delete)
)

(defrule focus-menus ""
  (declare (salience -10))
  =>
  (assert (menus))
  (focus menus)
)
;;;*************
;;;Recomanacions
;;;*************
;(do-for-all-instances ((?p Plato))  (or (eq (send ?p get-orden) primero) (eq (send ?p get-orden) ambos)) (send ?p imprimir))

(defmodule menus
  (import MAIN ?ALL)
  (import filtre ?ALL)
  (export =ALL)
)

(defrule genera-menus ""
  ?v <- (menus)
  =>
  (bind $?primers (find-all-instances ((?p Plato))  (or (eq (send ?p get-orden) primero) (eq (send ?p get-orden) ambos))))
)

