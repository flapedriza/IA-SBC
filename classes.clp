; Sat May 20 20:51:49 CEST 2017
;
;+ (version "3.4.8")
;+ (build "Build 629")


(defclass :CLIPS_TOP_LEVEL_SLOT_CLASS "Fake class to save top-level slot information"
	(is-a USER)
	(role abstract)
	(multislot bebida
;+		(comment "que bebidas se desean (en caso de varias se hara \"media\" en el precio)")
		(type INSTANCE)
;+		(allowed-classes Bebida)
		(cardinality 1 ?VARIABLE)
		(create-accessor read-write))
	(single-slot postre
		(type INSTANCE)
;+		(allowed-classes Plato)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot preferencia
		(type INSTANCE)
;+		(allowed-classes Preferencia)
		(cardinality 3 3)
		(create-accessor read-write))
	(single-slot primero
;+		(comment "primer plato")
		(type INSTANCE)
;+		(allowed-classes Plato)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot contiene
;+		(comment "que ingredientes contiene")
		(type INSTANCE)
;+		(allowed-classes Ingrediente)
		(cardinality 1 ?VARIABLE)
;+		(inverse-slot pertenece)
		(create-accessor read-write))
	(single-slot orden
;+		(comment "si el plato es primero, segundo, postre... ambos...")
		(type SYMBOL)
		(allowed-values primero segundo ambos postre)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot vegetariano
;+		(comment "si es para vegetarianos o no")
		(type SYMBOL)
		(allowed-values FALSE TRUE)
		(default FALSE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot tipo_cer
;+		(comment "de que tipo es")
		(type SYMBOL)
		(allowed-values trigo cebada)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot menus
;+		(comment "los 3 menus que se recomiendan")
		(type INSTANCE)
;+		(allowed-classes Menu)
		(cardinality 0 3)
		(create-accessor read-write))
	(single-slot temperatura
;+		(comment "frio o caliente (segun preferencias y epoca del año para decidir)")
		(type SYMBOL)
		(allowed-values frio caliente)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot compl
;+		(comment "complejidad del plato (el numero de comensales influye en eso)")
		(type SYMBOL)
		(allowed-values baja media alta)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot ing_prohibido
;+		(comment "ingredientes prohibidos para los menus del evento")
		(type INSTANCE)
;+		(allowed-classes Ingrediente)
		(create-accessor read-write))
	(single-slot coste
;+		(comment "coste del menu entre los 3 posibles. Dependera del precio maximo y minimo marcado por el cliente")
		(type SYMBOL)
		(allowed-values bajo medio alto)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot restriccion
;+		(comment "que restriccion tiene asociada el evento (precio, bebida...)")
		(type INSTANCE)
;+		(allowed-classes Restriccion)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot segundo
		(type INSTANCE)
;+		(allowed-classes Plato)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot pertenece
;+		(comment "a que platos pertenece el ingrediente")
		(type INSTANCE)
;+		(allowed-classes Plato)
		(cardinality 1 ?VARIABLE)
;+		(inverse-slot contiene)
		(create-accessor read-write))
	(single-slot num_comensales
;+		(comment "cuantos comensales asistiran")
		(type INTEGER)
		(range 1 ?VARIABLE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot region
;+		(comment "si se desea el plato de alguna region en concreto (si se deja vacio, todos los platos entran, y solo vale si en estilo hay regional)")
		(type SYMBOL)
		(allowed-values Espana Italia Alemania UK Francia)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot preparacion
;+		(comment "como se ha cocinado")
		(type SYMBOL)
		(allowed-values plancha horno brasa estofado hervido frito rebozado otro indiferente sopa/crema)
		(default indiferente)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot tipo_vin
;+		(comment "de que tipo es")
		(type SYMBOL)
		(allowed-values blanco rosado tinto cava)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot precio
;+		(comment "cuanto vale (moneda)")
		(type INTEGER)
		(range 0 ?VARIABLE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot precio_max
;+		(comment "precio maximo del menu del evento")
		(type INTEGER)
		(range 1 ?VARIABLE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot tipo_ag
;+		(comment "si es mineral o con gas")
		(type SYMBOL)
		(allowed-values mineral gas)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot bebida1
;+		(comment "bebida del primero / bebida para todo el menu en caso de no haber bebida2")
		(type INSTANCE)
;+		(allowed-classes Bebida)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot bebida2
;+		(comment "bebida del segundo plato en caso de haber una para cada")
		(type INSTANCE)
;+		(allowed-classes Bebida)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot tipo
;+		(comment "De que tipo es. Si es entrante, carne, pescado, arroz, pasta, pastel, helado, verdura...")
		(type SYMBOL)
		(allowed-values entrante verdura legumbre pasta arroz carne pescado helado pastel fruta indiferente)
		(default indiferente)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot tipo_ref
;+		(comment "el tipo de refresco")
		(type SYMBOL)
		(allowed-values cocacola fanta nestea)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot epoca
		(type SYMBOL)
		(allowed-values primavera verano otono invierno siempre)
		(cardinality 1 3)
		(create-accessor read-write))
	(single-slot precio_min
;+		(comment "precio minimo del menu del evento")
		(type INTEGER)
		(range 0 ?VARIABLE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot estilo
;+		(comment "estilo del plato. Moderno, sibarita, regional...\ndebe tener clasico/moderno (ambos no!) y luego regional y sibarita opcionalmente.")
		(type SYMBOL)
		(allowed-values clasico moderno regional sibarita indiferente)
		(default indiferente)
		(cardinality 1 ?VARIABLE)
		(create-accessor read-write))
	(single-slot maridaje
;+		(comment "si se quiere una bebida adecuada para los platos, una que combine bien,")
		(type SYMBOL)
		(allowed-values FALSE TRUE)
		(default FALSE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot nombre
;+		(comment "nombre del ingrediente / plato")
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot marida_con
;+		(comment "con que bebida se asocia mejor")
		(type INSTANCE)
;+		(allowed-classes Bebida)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Plato "datos sobre cada plato conocido"
	(is-a USER)
	(role concrete)
	(single-slot compl
;+		(comment "complejidad del plato (el numero de comensales influye en eso)")
		(type SYMBOL)
		(allowed-values baja media alta)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot preparacion
;+		(comment "como se ha cocinado")
		(type SYMBOL)
		(allowed-values plancha horno brasa estofado hervido frito rebozado otro indiferente sopa/crema)
		(default indiferente)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot precio
;+		(comment "cuanto vale (moneda)")
		(type INTEGER)
		(range 0 ?VARIABLE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot orden
;+		(comment "si el plato es primero, segundo, postre... ambos...")
		(type SYMBOL)
		(allowed-values primero segundo ambos postre)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot vegetariano
;+		(comment "si es para vegetarianos o no")
		(type SYMBOL)
		(allowed-values FALSE TRUE)
		(default FALSE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot tipo
;+		(comment "De que tipo es. Si es entrante, carne, pescado, arroz, pasta, pastel, helado, verdura...")
		(type SYMBOL)
		(allowed-values entrante verdura legumbre pasta arroz carne pescado helado pastel fruta indiferente)
		(default indiferente)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot epoca
		(type SYMBOL)
		(allowed-values primavera verano otono invierno siempre)
		(cardinality 1 3)
		(create-accessor read-write))
	(multislot estilo
;+		(comment "estilo del plato. Moderno, sibarita, regional...\ndebe tener clasico/moderno (ambos no!) y luego regional y sibarita opcionalmente.")
		(type SYMBOL)
		(allowed-values clasico moderno regional sibarita indiferente)
		(default indiferente)
		(cardinality 1 ?VARIABLE)
		(create-accessor read-write))
	(single-slot temperatura
;+		(comment "frio o caliente (segun preferencias y epoca del año para decidir)")
		(type SYMBOL)
		(allowed-values frio caliente)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot region
;+		(comment "si se desea el plato de alguna region en concreto (si se deja vacio, todos los platos entran, y solo vale si en estilo hay regional)")
		(type SYMBOL)
		(allowed-values Espana Italia Alemania UK Francia)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot nombre
;+		(comment "nombre del ingrediente / plato")
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot marida_con
;+		(comment "con que bebida se asocia mejor")
		(type INSTANCE)
;+		(allowed-classes Bebida)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot contiene
;+		(comment "que ingredientes contiene")
		(type INSTANCE)
;+		(allowed-classes Ingrediente)
		(cardinality 1 ?VARIABLE)
		(create-accessor read-write)))

(defclass Bebida "datos sobre cada bebida disponible"
	(is-a USER)
	(role concrete)
	(single-slot precio
;+		(comment "cuanto vale (moneda)")
		(type INTEGER)
		(range 0 ?VARIABLE)
;+		(cardinality 1 1)
		(create-accessor read-write)))

(defclass Agua "Agua, 2 tipos"
	(is-a Bebida)
	(role concrete)
	(single-slot tipo_ag
;+		(comment "si es mineral o con gas")
		(type SYMBOL)
		(allowed-values mineral gas)
;+		(cardinality 1 1)
		(create-accessor read-write)))

(defclass Refresco "Refresco, 3 tipos"
	(is-a Bebida)
	(role concrete)
	(single-slot tipo_ref
;+		(comment "el tipo de refresco")
		(type SYMBOL)
		(allowed-values cocacola fanta nestea)
;+		(cardinality 1 1)
		(create-accessor read-write)))

(defclass Vino "Vino, 4 tipos (maridaje distinto)"
	(is-a Bebida)
	(role concrete)
	(single-slot tipo_vin
;+		(comment "de que tipo es")
		(type SYMBOL)
		(allowed-values blanco rosado tinto cava)
;+		(cardinality 1 1)
		(create-accessor read-write)))

(defclass Cerveza "Cerveza, 2 tipos"
	(is-a Bebida)
	(role concrete)
	(single-slot tipo_cer
;+		(comment "de que tipo es")
		(type SYMBOL)
		(allowed-values trigo cebada)
;+		(cardinality 1 1)
		(create-accessor read-write)))

(defclass Ingrediente "datos sobre los ingredientes disponibles"
	(is-a USER)
	(role concrete)
	(multislot pertenece
;+		(comment "a que platos pertenece el ingrediente")
		(type INSTANCE)
;+		(allowed-classes Plato)
		(cardinality 1 ?VARIABLE)
		(create-accessor read-write))
	(single-slot nombre
;+		(comment "nombre del ingrediente / plato")
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write)))

(defclass Evento "datos del evento a celebrar"
	(is-a USER)
	(role concrete)
	(multislot menus
;+		(comment "los 3 menus que se recomiendan")
		(type INSTANCE)
;+		(allowed-classes Menu)
		(cardinality 0 3)
		(create-accessor read-write))
	(single-slot num_comensales
;+		(comment "cuantos comensales asistiran")
		(type INTEGER)
		(range 1 ?VARIABLE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot restriccion
;+		(comment "que restriccion tiene asociada el evento (precio, bebida...)")
		(type INSTANCE)
;+		(allowed-classes Restriccion)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot epoca
		(type SYMBOL)
		(allowed-values primavera verano otono invierno siempre)
		(cardinality 1 3)
		(create-accessor read-write)))

(defclass Restriccion "restricciones/preferencias generales del evento"
	(is-a USER)
	(role concrete)
	(multislot ing_prohibido
;+		(comment "ingredientes prohibidos para los menus del evento")
		(type INSTANCE)
;+		(allowed-classes Ingrediente)
		(create-accessor read-write))
	(single-slot precio_min
;+		(comment "precio minimo del menu del evento")
		(type INTEGER)
		(range 0 ?VARIABLE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot estilo
;+		(comment "estilo del plato. Moderno, sibarita, regional...\ndebe tener clasico/moderno (ambos no!) y luego regional y sibarita opcionalmente.")
		(type SYMBOL)
		(allowed-values clasico moderno regional sibarita indiferente)
		(default indiferente)
		(cardinality 1 ?VARIABLE)
		(create-accessor read-write))
	(single-slot precio_max
;+		(comment "precio maximo del menu del evento")
		(type INTEGER)
		(range 1 ?VARIABLE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot maridaje
;+		(comment "si se quiere una bebida adecuada para los platos, una que combine bien,")
		(type SYMBOL)
		(allowed-values FALSE TRUE)
		(default FALSE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot region
;+		(comment "si se desea el plato de alguna region en concreto (si se deja vacio, todos los platos entran, y solo vale si en estilo hay regional)")
		(type SYMBOL)
		(allowed-values Espana Italia Alemania UK Francia)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot preferencia
		(type INSTANCE)
;+		(allowed-classes Preferencia)
		(cardinality 3 3)
		(create-accessor read-write))
	(single-slot vegetariano
;+		(comment "si es para vegetarianos o no")
		(type SYMBOL)
		(allowed-values FALSE TRUE)
		(default FALSE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot bebida
;+		(comment "que bebidas se desean (en caso de varias se hara \"media\" en el precio)")
		(type INSTANCE)
;+		(allowed-classes Bebida)
		(cardinality 1 ?VARIABLE)
		(create-accessor read-write)))

(defclass Preferencia "preferencias asociadas a una restriccion concreta. Debe haber 3 para cada restriccion. determinan preferencias para los platos."
	(is-a USER)
	(role concrete)
	(single-slot preparacion
;+		(comment "como se ha cocinado")
		(type SYMBOL)
		(allowed-values plancha horno brasa estofado hervido frito rebozado otro indiferente sopa/crema)
		(default indiferente)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot orden
;+		(comment "si el plato es primero, segundo, postre... ambos...")
		(type SYMBOL)
		(allowed-values primero segundo ambos postre)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot tipo
;+		(comment "De que tipo es. Si es entrante, carne, pescado, arroz, pasta, pastel, helado, verdura...")
		(type SYMBOL)
		(allowed-values entrante verdura legumbre pasta arroz carne pescado helado pastel fruta indiferente)
		(default indiferente)
;+		(cardinality 1 1)
		(create-accessor read-write)))

(defclass Menu "datos del menu, incluyendo su precio y su categoria de precio (3 posibles)"
	(is-a USER)
	(role concrete)
	(single-slot coste
;+		(comment "coste del menu entre los 3 posibles. Dependera del precio maximo y minimo marcado por el cliente")
		(type SYMBOL)
		(allowed-values bajo medio alto)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot bebida1
;+		(comment "bebida del primero / bebida para todo el menu en caso de no haber bebida2")
		(type INSTANCE)
;+		(allowed-classes Bebida)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot maridaje
;+		(comment "si se quiere una bebida adecuada para los platos, una que combine bien,")
		(type SYMBOL)
		(allowed-values FALSE TRUE)
		(default FALSE)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot bebida2
;+		(comment "bebida del segundo plato en caso de haber una para cada")
		(type INSTANCE)
;+		(allowed-classes Bebida)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot primero
;+		(comment "primer plato")
		(type INSTANCE)
;+		(allowed-classes Plato)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot segundo
		(type INSTANCE)
;+		(allowed-classes Plato)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot postre
		(type INSTANCE)
;+		(allowed-classes Plato)
;+		(cardinality 1 1)
		(create-accessor read-write)))
