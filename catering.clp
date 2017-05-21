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
(deffunction price-range (?min ?max ?val)
  (bind ?diff (/ (- ?max ?min) 3))
  (bind ?low (+ ?min ?diff))
  (bind ?med (+ ?min (* ?diff 2)))
  (if (<= ?val ?low) then (return bajo))
  (if (<= ?val ?med) then (return medio))
  alto
)

(deffunction create-menu (?primer ?segon ?postre ?maridaje ?bebi1 ?min ?max)
  (bind ?beb1 ?bebi1)
  (bind ?beb2 nil)
  (if ?maridaje then
    (bind ?beb1 (send ?primer get-marida_con))
    (if (eq ?beb1 [nil]) then (bind ?beb1 ?bebi1))
    (bind ?beb2 (send ?segon get-marida_con))
    (if (eq ?beb2 [nil]) then (bind ?beb2 nil))
  )
  (bind ?newMen
    (make-instance (sym-cat Menu:(send ?primer get-nombre)-(send ?segon get-nombre)-(send ?postre get-nombre)) of Menu
      (primero ?primer)
      (segundo ?segon)
      (postre ?postre)
      (bebida1 ?beb1)
      (bebida2 ?beb2)
      (maridaje ?maridaje)
    )
  )
  (send ?newMen calc-coste ?min ?max)
  ?newMen
)

(deffunction busca-inst (?tipus)
  (switch ?tipus
    (case AguaMineral then (find-instance((?a Agua)) (eq (send ?a get-tipo_ag) mineral)))
    (case AguaGas then (find-instance((?a Agua)) (eq (send ?a get-tipo_ag) gas)))
    (case CervezaTrigo then (find-instance((?a Cerveza)) (eq (send ?a get-tipo_cer) trigo)))
    (case CervezaCebada then (find-instance((?a Cerveza)) (eq (send ?a get-tipo_cer) cebada)))
    (case VinoTinto then (find-instance((?a Vino)) (eq (send ?a get-tipo_vin) tinto)))
    (case VinoRosado then (find-instance((?a Vino)) (eq (send ?a get-tipo_vin) rosado)))
    (case VinoBlanco then (find-instance((?a Vino)) (eq (send ?a get-tipo_vin) blanco)))
    (case Cava then (find-instance((?a Vino)) (eq (send ?a get-tipo_vin) cava)))
    (case CocaCola then (find-instance((?a Refresco)) (eq (send ?a get-tipo_ref) cocacola)))
    (case Fanta then (find-instance((?a Refresco)) (eq (send ?a get-tipo_ref) fanta)))
    (case Nestea then (find-instance((?a Refresco)) (eq (send ?a get-tipo_ref) nestea)))
  )
)

(deffunction busca-beguda (?tipus)
  (bind ?ret (nth$ 1 (busca-inst ?tipus)))
  ?ret
)



;;; Message handlers Plato
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

(defmessage-handler Menu calc-coste primary (?min ?max)
  (bind ?precio (send ?self calc-precio))
  (bind ?self:coste (price-range ?min ?max ?precio))
)

(deffunction pregunta-general (?pregunta)
  (format t "%s" ?pregunta)
  (bind ?respuesta (read))
  ?respuesta
)

(deffunction pregunta-numerica (?pregunta ?rangini ?rangfi)
  (format t "%s [%d, %d] " ?pregunta ?rangini ?rangfi)
  (bind ?respuesta (read))
  (while (not(and(>= ?respuesta ?rangini)(<= ?respuesta ?rangfi))) do
    (format t "%s [%d, %d] " ?pregunta ?rangini ?rangfi)
    (bind ?respuesta (read))
  )
  ?respuesta
)

(deffunction pregunta-booleana (?pregunta)
  (format t "%s (s/n) " ?pregunta)
  (bind ?resp (read))
  (while (not (or (eq ?resp s)(eq ?resp n)(eq ?resp si)(eq ?resp no))) do
    (format t "%s (s/n)" ?pregunta)
    (bind ?resp (read))
  )
  (if (or (eq ?resp s) (eq ?resp si))
    then TRUE
    else FALSE
  )
)

;;; Funcion para hacer una pregunta con un conjunto definido de valores de repuesta
(deffunction pregunta-lista (?pregunta $?valores_posibles)
  (format t "%s" ?pregunta)
  (bind ?resposta (readline))
  (bind ?res (str-explode ?resposta))
  (loop-for-count (?i 1 (length$ ?res)) do
    (if (not(member (nth$ ?i ?res) ?valores_posibles)) then
      (printout t "El valor " (nth$ ?i ?res) " no es un valor valido" crlf)
      (bind ?valor (pregunta-numerica "Introduce un nuevo valor " 1 (- (length$ ?valores_posibles) 1)))
      (bind $?res (delete$ ?res ?i ?i))
      (bind $?res (insert$ ?res ?i ?valor))
    )
  )
  ?res
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule MAIN (export ?ALL))

;;; Banner de inicio
(defrule banner ""
  (declare (salience 100))
  =>
  (printout t crlf crlf)
  (printout t "Generador de Menus")
  (printout t crlf crlf)
  (printout t "Bienvenido, para obtener las sujerencias de menu responda a las siguientes preguntas." crlf)
  (assert (start))
)

;;;*********************PREGUNTAS*****************

(defrule pregunta-numero "Pregunta para saber el número de comensales"
  (start)
  =>
  (bind ?pers (pregunta-numerica "Cuantos comensales asistirán al evento?" 0 10000))
  (assert (NumComensales ?pers))
)

(defrule pregunta-fecha "Pregunta para saber la fecha"
  (start)
  =>
  (printout t "En que temporada sera el evento? Seleccione una de las siguientes:" crlf)
  (printout t "1 - Invierno" crlf)
  (printout t "2 - Primavera" crlf)
  (printout t "3 - Verano" crlf)
  (printout t "4 - Otono" crlf)
  (bind ?resp (pregunta-numerica "Introduzca un número 1-4:" 1 4))
  (if (= ?resp 1) then (assert (Temporada Invierno)))
  (if (= ?resp 2) then (assert (Temporada Primavera)))
  (if (= ?resp 3) then (assert (Temporada Verano)))
  (if (= ?resp 4) then (assert (Temporada Otono)))
)

(defrule pregunta-precio "Pregunta para saber cuanto quiere gastarse"
  (start)
  =>
  (bind ?pmax (pregunta-numerica "Cual es el precio maximo que quiere gastarse por menu?" 0 1000))
  (assert (PrecioMaximo ?pmax))
  (bind ?pmin (pregunta-numerica "Cual es el precio minimo que quiere gastarse por menu?" 0 ?pmax))
  (assert (PrecioMinimo ?pmin))
)

(defrule pregunta-bebida "Pregunta para saber la/s bebida/s"
  (start)
  =>
  (printout t "Que quiere para beber?" crlf)
  (printout t "1 - Agua" crlf)
  (printout t "2 - Cerveza" crlf)
  (printout t "3 - Refresco" crlf)
  (printout t "4 - Vino" crlf)
  (printout t "5 - Maridaje (el sistema elijirá la bebida mas adecuada para cada plato)" crlf)
  (bind ?resp (pregunta-numerica "Introduzca un número:" 1 5))
  (if (= ?resp 1) then (assert (seleccion-tipo-bebida Agua)))
  (if (= ?resp 2) then (assert (seleccion-tipo-bebida Cerveza)))
  (if (= ?resp 3) then (assert (seleccion-tipo-bebida Refresco)))
  (if (= ?resp 4) then (assert (seleccion-tipo-bebida Vino)))
  (if (= ?resp 5) then (assert (Maridaje TRUE)) else (assert (Maridaje FALSE)))
)

(defrule pregunta-agua "Pregunta para saber tipos de bebidas"
  (start)
  (seleccion-tipo-bebida Agua)
  =>
  (bind ?resp (pregunta-numerica "Agua natural (1) o con gas(2)? Indiferente(0):" 0 2))
  (if (< ?resp 2) then (assert (Bebida AguaMineral)) else (assert(Bebida AguaGas)))
)

(defrule pregunta-cerveza "Pregunta para saber tipos de bebidas"
  (start)
  (seleccion-tipo-bebida Cerveza)
  =>
  (bind ?resp (pregunta-numerica "Prefiere cerveza de trigo(1) o cebada(2)? Indiferente(0):" 0 2))
  (if (< ?resp 2) then (assert (Bebida CervezaTrigo)) else (assert(Bebida CervezaCebada)))
)

(defrule pregunta-refresco "Pregunta para saber tipos de bebidas"
  (start)
  (seleccion-tipo-bebida Refresco)
  =>
  (bind ?resp (pregunta-numerica "Prefiere CocaCola(1), Fanta(2) o Nestea(3)? Indiferente(0):" 0 3))
  (if (< ?resp 2) then (assert (Bebida CocaCola)))
  (if (= ?resp 2) then (assert (Bebida Fanta)))
  (if (= ?resp 3) then (assert (Bebida Nestea)))
)

(defrule pregunta-vino "Pregunta para saber tipos de bebidas"
  (start)
  (seleccion-tipo-bebida Vino)
  =>
  (bind ?resp (pregunta-numerica "Prefiere vino tinto(1), blanco(2), rosado(3) o cava(4)? Indiferente(0):" 0 4))
  (if (< ?resp 2) then (assert (Bebida VinoTinto)))
  (if (= ?resp 2) then (assert (Bebida VinoBlanco)))
  (if (= ?resp 3) then (assert (Bebida VinoRosado)))
  (if (= ?resp 4) then (assert (Bebida Cava)))
)

(defrule pregunta-ingredientes "Pregunta para saber ingredientes prohibidos"
  (start)
  (Bebida ?b)
  =>
  (bind ?lista (find-all-instances ((?x Ingrediente)) TRUE))
  (bind $?valores_permitidos (create$ 0))
  (loop-for-count (?i 1 (length$ ?lista)) do
     (printout t ?i " - " (send (nth$ ?i ?lista) get-nombre) crlf)
     (bind $?valores_permitidos (insert$ ?valores_permitidos (+ (length$ ?valores_permitidos) 1) ?i))
  )
  (bind ?resp (pregunta-lista "Escribe los identificadores de los ingredientes prohibidos separados por espacios: " $?valores_permitidos))
  (if (not (member 0 ?resp))
    then
      (progn$ (?it ?resp)
        (assert (IngredienteProhibido (send (nth$ ?it ?lista) get-nombre)))
      )
  )
)

(defrule pregunta-estilo "Pregunta para saber tipo de cocina"
  (start)
  =>
  (printout t "Que estilo de cocina prefiere?" crlf)
  (bind ?lista (slot-allowed-values Plato estilo))
  (bind $?valores_permitidos (create$ 0))
  (loop-for-count (?i 1 (length$ ?lista)) do
     (printout t ?i " - " (nth$ ?i ?lista) crlf)
     (bind $?valores_permitidos (insert$ ?valores_permitidos (+ (length$ ?valores_permitidos) 1) ?i))
  )
  (bind ?resp (pregunta-numerica "Seleccione uno de los estilos anteriores " 1 (length$ ?lista)))
  (assert (Estilo (nth$ ?resp ?lista)))
)

(defrule pregunta-regional "Pregunta para saber region"
  (start)
  (Estilo regional)
  =>
  (printout t "Escoja la region que prefiera de la lista siguiente" crlf)
  (bind ?lista (slot-allowed-values Plato region))
  (bind $?valores_permitidos (create$ 0))
  (loop-for-count (?i 1 (length$ ?lista)) do
     (printout t ?i " - " (nth$ ?i ?lista) crlf)
     (bind $?valores_permitidos (insert$ ?valores_permitidos (+ (length$ ?valores_permitidos) 1) ?i))
  )
  (bind ?resp (pregunta-numerica "Seleccione una de las regiones anteriores " 1 (length$ ?lista)))
  (assert (Region (nth$ ?resp ?lista)))
)


(defrule pregunta-vegetariano "Pregunta para saber si es vegetariano"
  (start)
  =>
  (bind ?resp (pregunta-booleana "Quiere solo comida vegetariana?"))
  (assert (Vegetariano ?resp))
)

(defrule focus-filter
  (declare (salience -10))
  =>
  (focus filtre)
)

;;;****************************

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

(defrule focus-menus
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
  (export ?ALL)
)

(defrule genera-menus-mari ""
  ?v <- (menus)
  (Maridaje TRUE)
  (PrecioMaximo ?max)
  (PrecioMinimo ?min)
  =>
  (bind $?primers (find-all-instances ((?p Plato))  (eq (send ?p get-orden) primero)))
  (bind $?segons (find-all-instances ((?p Plato))  (eq (send ?p get-orden) segundo)))
  (bind $?postres (find-all-instances ((?p Plato))  (eq (send ?p get-orden) postre)))
  (bind $?priseg (find-all-instances ((?p Plato))  (eq (send ?p get-orden) ambos)))
  (loop-for-count (?i 1 (length$ ?primers)) do
    (bind ?prim (nth$ ?i ?primers))
    (loop-for-count (?j 1 (length$ ?segons)) do
    (bind ?seg (nth$ ?j ?segons))
      (loop-for-count (?k 1 (length$ ?postres)) do
        (bind ?postr (nth$ ?k ?postres))
        (create-menu ?prim ?seg ?postr TRUE (busca-beguda AguaMineral) ?max ?min)
      )
    )
  )
  (assert (menus_generados))
  (retract ?v)
)

(defrule genera-menus-no-mari ""
  ?v <- (menus)
  (Maridaje FALSE)
  (PrecioMaximo ?max)
  (PrecioMinimo ?min)
  (Bebida ?selbeb)
  =>
  (bind $?primers (find-all-instances ((?p Plato))  (eq (send ?p get-orden) primero)))
  (bind $?segons (find-all-instances ((?p Plato))  (eq (send ?p get-orden) segundo)))
  (bind $?postres (find-all-instances ((?p Plato))  (eq (send ?p get-orden) postre)))
  (bind $?priseg (find-all-instances ((?p Plato))  (eq (send ?p get-orden) ambos)))
  (loop-for-count (?i 1 (length$ ?primers)) do
    (bind ?prim (nth$ ?i ?primers))
    (loop-for-count (?j 1 (length$ ?segons)) do
    (bind ?seg (nth$ ?j ?segons))
      (loop-for-count (?k 1 (length$ ?postres)) do
        (bind ?postr (nth$ ?k ?postres))
        (create-menu ?prim ?seg ?postr FALSE (busca-beguda ?selbeb) ?max ?min)
      )
    )
  )
  (assert (menus_generados))
  (retract ?v)
)
