; Sat May 20 22:40:29 CEST 2017
;
;+ (version "3.5")
;+ (build "Build 663")
(definstances instancies
	([ontologia_Class0] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20010]
			[ontologia_Class20011]
			[ontologia_Class20009]
			[ontologia_Class20008])
		(epoca siempre)
		(estilo clasico)
		(nombre "Tomate fresco con mozzarella y oregano")
		(orden primero)
		(precio 9)
		(preparacion otro)
		(temperatura frio)
		(tipo entrante)
		(vegetariano TRUE))

	([ontologia_Class10000] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20080]
			[ontologia_Class10001]
			[ontologia_Class10002]
			[ontologia_Class20030]
			[ontologia_Class20064]
			[ontologia_Class30010]
			[ontologia_Class20028])
		(epoca primavera otono invierno)
		(estilo clasico)
		(marida_con [ontologia_Class20051])
		(nombre "Salteado de col")
		(orden primero)
		(precio 6)
		(preparacion frito)
		(temperatura caliente)
		(tipo verdura)
		(vegetariano TRUE))

	([ontologia_Class10001] of  Ingrediente

		(nombre "col")
		(pertenece
			[ontologia_Class10003]
			[ontologia_Class20005]
			[ontologia_Class30015]
			[ontologia_Class10000]))

	([ontologia_Class10002] of  Ingrediente

		(nombre "col lombarda")
		(pertenece [ontologia_Class10000]))

	([ontologia_Class10003] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class10005]
			[ontologia_Class20026]
			[ontologia_Class10004]
			[ontologia_Class20036]
			[ontologia_Class20037]
			[ontologia_Class20040]
			[ontologia_Class10001])
		(epoca invierno primavera)
		(estilo clasico)
		(marida_con [ontologia_Class20051])
		(nombre "Verduras al grill")
		(orden primero)
		(precio 8)
		(preparacion plancha)
		(temperatura caliente)
		(tipo verdura)
		(vegetariano TRUE))

	([ontologia_Class10004] of  Ingrediente

		(nombre "calabacin")
		(pertenece
			[ontologia_Class30013]
			[ontologia_Class30018]
			[ontologia_Class30019]
			[ontologia_Class10003]))

	([ontologia_Class10005] of  Ingrediente

		(nombre "berenjena")
		(pertenece
			[ontologia_Class30018]
			[ontologia_Class40040]
			[ontologia_Class10003]))

	([ontologia_Class10006] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20037]
			[ontologia_Class20066]
			[ontologia_Class20010]
			[ontologia_Class20031]
			[ontologia_Class20009])
		(epoca invierno primavera)
		(estilo clasico sibarita)
		(marida_con [ontologia_Class20058])
		(nombre "Esparragos gratinados")
		(orden ambos)
		(precio 11)
		(preparacion horno)
		(temperatura caliente)
		(tipo verdura)
		(vegetariano TRUE))

	([ontologia_Class20000] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20039]
			[ontologia_Class20008]
			[ontologia_Class20001])
		(epoca siempre)
		(estilo clasico)
		(marida_con [ontologia_Class20056])
		(nombre "Cordero a la brasa")
		(orden segundo)
		(precio 15)
		(preparacion brasa)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class20001] of  Ingrediente

		(nombre "cordero")
		(pertenece
			[ontologia_Class20006]
			[ontologia_Class20000]))

	([ontologia_Class20002] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class30031]
			[ontologia_Class30010]
			[ontologia_Class20039]
			[ontologia_Class20008])
		(epoca siempre)
		(estilo clasico)
		(marida_con [ontologia_Class20056])
		(nombre "Butifarra a la brasa")
		(orden segundo)
		(precio 13)
		(preparacion brasa)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class20003] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class30031]
			[ontologia_Class20004])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20056])
		(nombre "Butifarra con alubias")
		(orden segundo)
		(precio 16)
		(preparacion plancha)
		(region Espana)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class20004] of  Ingrediente

		(nombre "alubia")
		(pertenece [ontologia_Class20003]))

	([ontologia_Class20005] of  Plato

		(compl media)
		(contiene
			[ontologia_Class30010]
			[ontologia_Class20039]
			[ontologia_Class20035]
			[ontologia_Class10001])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20051])
		(nombre "Codillo asado")
		(orden segundo)
		(precio 23)
		(preparacion brasa)
		(region Alemania)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class20006] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class30010]
			[ontologia_Class20071]
			[ontologia_Class30016]
			[ontologia_Class20001])
		(epoca siempre)
		(estilo clasico)
		(marida_con [ontologia_Class20058])
		(nombre "Surtido de salchichas")
		(orden segundo)
		(precio 21)
		(preparacion brasa)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class20007] of  Ingrediente

		(nombre "leche")
		(pertenece
			[ontologia_Class20027]
			[ontologia_Class30000]
			[ontologia_Class30001]
			[ontologia_Class30008]
			[ontologia_Class30009]
			[ontologia_Class30057]
			[ontologia_Class40046]
			[ontologia_Class40055]))

	([ontologia_Class20008] of  Ingrediente

		(nombre "tomate")
		(pertenece
			[ontologia_Class20013]
			[ontologia_Class20024]
			[ontologia_Class20032]
			[ontologia_Class20060]
			[ontologia_Class20079]
			[ontologia_Class20085]
			[ontologia_Class30007]
			[ontologia_Class30018]
			[ontologia_Class30037]
			[ontologia_Class30041]
			[ontologia_Class30042]
			[ontologia_Class30043]
			[ontologia_Class30046]
			[ontologia_Class30049]
			[ontologia_Class30051]
			[ontologia_Class30053]
			[ontologia_Class30054]
			[ontologia_Class40008]
			[ontologia_Class40011]
			[ontologia_Class40020]
			[ontologia_Class40029]
			[ontologia_Class40034]
			[ontologia_Class40040]
			[ontologia_Class40042]))

	([ontologia_Class20009] of  Ingrediente

		(nombre "queso")
		(pertenece
			[ontologia_Class20044]
			[ontologia_Class20046]
			[ontologia_Class20065]
			[ontologia_Class30001]
			[ontologia_Class30008]
			[ontologia_Class30009]
			[ontologia_Class30033]
			[ontologia_Class30035]
			[ontologia_Class30041]
			[ontologia_Class30042]
			[ontologia_Class30046]
			[ontologia_Class30048]
			[ontologia_Class30056]
			[ontologia_Class30057]))

	([ontologia_Class20010] of  Ingrediente

		(nombre "lacteo")
		(pertenece
			[ontologia_Class20027]
			[ontologia_Class20044]
			[ontologia_Class20046]
			[ontologia_Class20065]
			[ontologia_Class20083]
			[ontologia_Class30000]
			[ontologia_Class30001]
			[ontologia_Class30005]
			[ontologia_Class30008]
			[ontologia_Class30009]
			[ontologia_Class30033]
			[ontologia_Class30035]
			[ontologia_Class30041]
			[ontologia_Class30042]
			[ontologia_Class30043]
			[ontologia_Class30046]
			[ontologia_Class30048]
			[ontologia_Class30056]
			[ontologia_Class30057]
			[ontologia_Class40043]
			[ontologia_Class40046]
			[ontologia_Class40048]
			[ontologia_Class40049]
			[ontologia_Class40050]
			[ontologia_Class40055]
			[ontologia_Class40056]))

	([ontologia_Class20011] of  Ingrediente

		(nombre "oregano")
		(pertenece [ontologia_Class0]))

	([ontologia_Class20012] of  Ingrediente

		(nombre "aceite de oliva")
		(pertenece
			[ontologia_Class20013]
			[ontologia_Class20021]
			[ontologia_Class20024]
			[ontologia_Class20027]
			[ontologia_Class20032]
			[ontologia_Class20038]
			[ontologia_Class20044]
			[ontologia_Class20060]
			[ontologia_Class20061]
			[ontologia_Class20062]
			[ontologia_Class20069]
			[ontologia_Class20076]
			[ontologia_Class20077]
			[ontologia_Class20079]
			[ontologia_Class30007]
			[ontologia_Class30008]
			[ontologia_Class30009]
			[ontologia_Class30011]
			[ontologia_Class30013]
			[ontologia_Class30014]
			[ontologia_Class30015]
			[ontologia_Class30017]
			[ontologia_Class30018]
			[ontologia_Class30019]
			[ontologia_Class30021]
			[ontologia_Class30027]
			[ontologia_Class30029]
			[ontologia_Class30033]
			[ontologia_Class30035]
			[ontologia_Class30037]
			[ontologia_Class30039]
			[ontologia_Class30041]
			[ontologia_Class30042]
			[ontologia_Class30043]
			[ontologia_Class30046]
			[ontologia_Class30047]
			[ontologia_Class30051]
			[ontologia_Class30053]
			[ontologia_Class30054]
			[ontologia_Class30056]
			[ontologia_Class30057]
			[ontologia_Class30058]
			[ontologia_Class40008]
			[ontologia_Class40009]
			[ontologia_Class40010]
			[ontologia_Class40011]
			[ontologia_Class40018]
			[ontologia_Class40020]
			[ontologia_Class40021]
			[ontologia_Class40029]
			[ontologia_Class40030]
			[ontologia_Class40032]
			[ontologia_Class40034]
			[ontologia_Class40035]
			[ontologia_Class40036]
			[ontologia_Class40037]
			[ontologia_Class40038]
			[ontologia_Class40039]
			[ontologia_Class40040]
			[ontologia_Class40042]))

	([ontologia_Class20013] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20014]
			[ontologia_Class20015]
			[ontologia_Class20016]
			[ontologia_Class20008]
			[ontologia_Class20019])
		(epoca invierno primavera)
		(estilo moderno sibarita)
		(nombre "Ensalada de tomate con vinagreta de fresa")
		(orden primero)
		(precio 13)
		(preparacion otro)
		(temperatura frio)
		(tipo entrante)
		(vegetariano TRUE))

	([ontologia_Class20014] of  Ingrediente

		(nombre "fresa")
		(pertenece
			[ontologia_Class30001]
			[ontologia_Class30003]
			[ontologia_Class30005]
			[ontologia_Class40048]
			[ontologia_Class20013]))

	([ontologia_Class20015] of  Ingrediente

		(nombre "fruta")
		(pertenece
			[ontologia_Class20021]
			[ontologia_Class20062]
			[ontologia_Class30001]
			[ontologia_Class30002]
			[ontologia_Class30003]
			[ontologia_Class30004]
			[ontologia_Class30005]
			[ontologia_Class30006]
			[ontologia_Class30049]
			[ontologia_Class40007]
			[ontologia_Class40018]
			[ontologia_Class40043]
			[ontologia_Class40044]
			[ontologia_Class40048]
			[ontologia_Class40049]
			[ontologia_Class40050]
			[ontologia_Class40052]
			[ontologia_Class40054]
			[ontologia_Class40055]))

	([ontologia_Class20016] of  Ingrediente

		(nombre "lechuga")
		(pertenece
			[ontologia_Class20021]
			[ontologia_Class20032]
			[ontologia_Class20044]
			[ontologia_Class20060]
			[ontologia_Class20061]
			[ontologia_Class20062]
			[ontologia_Class40014]
			[ontologia_Class20013]))

	([ontologia_Class20017] of  Ingrediente

		(nombre "naranja")
		(pertenece
			[ontologia_Class20021]
			[ontologia_Class20061]
			[ontologia_Class30001]
			[ontologia_Class30003]
			[ontologia_Class30004]
			[ontologia_Class40015]
			[ontologia_Class40021]))

	([ontologia_Class20019] of  Ingrediente

		(nombre "vinagre")
		(pertenece
			[ontologia_Class20024]
			[ontologia_Class20032]
			[ontologia_Class20044]
			[ontologia_Class20060]
			[ontologia_Class20061]
			[ontologia_Class20062]
			[ontologia_Class20079]
			[ontologia_Class20013]))

	([ontologia_Class20020] of  Ingrediente

		(nombre "mandarina")
		(pertenece [ontologia_Class20061]))

	([ontologia_Class20021] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20022]
			[ontologia_Class20015]
			[ontologia_Class20017]
			[ontologia_Class20023]
			[ontologia_Class20016])
		(epoca primavera otono invierno)
		(estilo moderno sibarita)
		(nombre "Ensalada de canonigos y naranja con pasas")
		(orden primero)
		(precio 8)
		(preparacion otro)
		(temperatura frio)
		(tipo entrante)
		(vegetariano TRUE))

	([ontologia_Class20022] of  Ingrediente

		(nombre "canonigo")
		(pertenece
			[ontologia_Class20061]
			[ontologia_Class20021]))

	([ontologia_Class20023] of  Ingrediente

		(nombre "pasa")
		(pertenece [ontologia_Class20021]))

	([ontologia_Class20024] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20026]
			[ontologia_Class20025]
			[ontologia_Class20008]
			[ontologia_Class20019])
		(epoca siempre)
		(estilo clasico)
		(nombre "Ensalada de remolacha y berros")
		(orden primero)
		(precio 11)
		(preparacion otro)
		(temperatura frio)
		(tipo entrante)
		(vegetariano TRUE))

	([ontologia_Class20025] of  Ingrediente

		(nombre "remolacha")
		(pertenece [ontologia_Class20024]))

	([ontologia_Class20026] of  Ingrediente

		(nombre "berro")
		(pertenece
			[ontologia_Class20062]
			[ontologia_Class20024]
			[ontologia_Class10003]))

	([ontologia_Class20027] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20030]
			[ontologia_Class20010]
			[ontologia_Class20029]
			[ontologia_Class20031]
			[ontologia_Class20066]
			[ontologia_Class20007]
			[ontologia_Class20028])
		(epoca siempre)
		(estilo moderno sibarita)
		(nombre "Croquetas de coliflor")
		(orden primero)
		(precio 8)
		(preparacion rebozado)
		(temperatura caliente)
		(tipo entrante)
		(vegetariano TRUE))

	([ontologia_Class20028] of  Ingrediente

		(nombre "coliflor")
		(pertenece
			[ontologia_Class30008]
			[ontologia_Class20027]
			[ontologia_Class10000]))

	([ontologia_Class20029] of  Ingrediente

		(nombre "pan rallado")
		(pertenece
			[ontologia_Class20069]
			[ontologia_Class20076]
			[ontologia_Class20077]
			[ontologia_Class20027]))

	([ontologia_Class20030] of  Ingrediente

		(nombre "huevo")
		(pertenece
			[ontologia_Class20038]
			[ontologia_Class20060]
			[ontologia_Class20065]
			[ontologia_Class20069]
			[ontologia_Class20076]
			[ontologia_Class20077]
			[ontologia_Class20083]
			[ontologia_Class30000]
			[ontologia_Class30001]
			[ontologia_Class30002]
			[ontologia_Class30013]
			[ontologia_Class30014]
			[ontologia_Class30033]
			[ontologia_Class30058]
			[ontologia_Class40012]
			[ontologia_Class40041]
			[ontologia_Class40050]
			[ontologia_Class40052]
			[ontologia_Class40055]
			[ontologia_Class40056]))

	([ontologia_Class20031] of  Ingrediente

		(nombre "mantequilla")
		(pertenece
			[ontologia_Class20065]
			[ontologia_Class20069]
			[ontologia_Class20076]
			[ontologia_Class20077]
			[ontologia_Class20083]
			[ontologia_Class30008]
			[ontologia_Class30009]
			[ontologia_Class30017]
			[ontologia_Class30042]
			[ontologia_Class30043]
			[ontologia_Class30048]
			[ontologia_Class30056]
			[ontologia_Class40012]
			[ontologia_Class40052]
			[ontologia_Class40054]))

	([ontologia_Class20032] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class20034]
			[ontologia_Class20033]
			[ontologia_Class20037]
			[ontologia_Class20035]
			[ontologia_Class20008]
			[ontologia_Class20016]
			[ontologia_Class20019])
		(epoca siempre)
		(estilo clasico)
		(nombre "Ensalada verde")
		(orden primero)
		(precio 9)
		(preparacion otro)
		(temperatura frio)
		(tipo entrante)
		(vegetariano TRUE))

	([ontologia_Class20033] of  Ingrediente

		(nombre "escarola")
		(pertenece
			[ontologia_Class20061]
			[ontologia_Class20032]))

	([ontologia_Class20034] of  Ingrediente

		(nombre "endibia")
		(pertenece [ontologia_Class20032]))

	([ontologia_Class20035] of  Ingrediente

		(nombre "pimiento")
		(pertenece
			[ontologia_Class20060]
			[ontologia_Class20079]
			[ontologia_Class30027]
			[ontologia_Class40014]
			[ontologia_Class20005]
			[ontologia_Class20032]))

	([ontologia_Class20036] of  Ingrediente

		(nombre "cebolla")
		(pertenece
			[ontologia_Class20060]
			[ontologia_Class20079]
			[ontologia_Class30007]
			[ontologia_Class30014]
			[ontologia_Class30018]
			[ontologia_Class30019]
			[ontologia_Class30027]
			[ontologia_Class30037]
			[ontologia_Class30041]
			[ontologia_Class30042]
			[ontologia_Class30043]
			[ontologia_Class30046]
			[ontologia_Class30048]
			[ontologia_Class30049]
			[ontologia_Class30051]
			[ontologia_Class30053]
			[ontologia_Class30054]
			[ontologia_Class30057]
			[ontologia_Class40008]
			[ontologia_Class40018]
			[ontologia_Class40020]
			[ontologia_Class40021]
			[ontologia_Class40023]
			[ontologia_Class40029]
			[ontologia_Class40032]
			[ontologia_Class40034]
			[ontologia_Class40039]
			[ontologia_Class40040]
			[ontologia_Class40042]))

	([ontologia_Class20037] of  Ingrediente

		(nombre "esparrago")
		(pertenece
			[ontologia_Class20060]
			[ontologia_Class20032]
			[ontologia_Class10006]
			[ontologia_Class10003]))

	([ontologia_Class20038] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20030]
			[ontologia_Class20040]
			[ontologia_Class20042]
			[ontologia_Class20039]
			[ontologia_Class20041])
		(epoca siempre)
		(estilo clasico)
		(nombre "Ensaladilla rusa")
		(orden primero)
		(precio 10)
		(preparacion hervido)
		(temperatura frio)
		(tipo entrante)
		(vegetariano TRUE))

	([ontologia_Class20039] of  Ingrediente

		(nombre "patata")
		(pertenece
			[ontologia_Class30007]
			[ontologia_Class30009]
			[ontologia_Class30013]
			[ontologia_Class30014]
			[ontologia_Class30017]
			[ontologia_Class30019]
			[ontologia_Class30060]
			[ontologia_Class40009]
			[ontologia_Class40010]
			[ontologia_Class40011]
			[ontologia_Class40012]
			[ontologia_Class40014]
			[ontologia_Class40033]
			[ontologia_Class40034]
			[ontologia_Class40037]
			[ontologia_Class40038]
			[ontologia_Class40039]
			[ontologia_Class40041]))

	([ontologia_Class20040] of  Ingrediente

		(nombre "judia verde")
		(pertenece
			[ontologia_Class30019]
			[ontologia_Class30051]
			[ontologia_Class20038]
			[ontologia_Class10003]))

	([ontologia_Class20041] of  Ingrediente

		(nombre "zanahoria")
		(pertenece
			[ontologia_Class30007]
			[ontologia_Class30019]
			[ontologia_Class30060]
			[ontologia_Class40023]
			[ontologia_Class20038]))

	([ontologia_Class20042] of  Ingrediente

		(nombre "mayonesa")
		(pertenece [ontologia_Class20038]))

	([ontologia_Class20043] of  Ingrediente

		(nombre "atun")
		(pertenece
			[ontologia_Class20060]
			[ontologia_Class30046]
			[ontologia_Class40029]))

	([ontologia_Class20044] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20010]
			[ontologia_Class20009]
			[ontologia_Class20045]
			[ontologia_Class20047]
			[ontologia_Class20075]
			[ontologia_Class20016]
			[ontologia_Class20019])
		(epoca siempre)
		(estilo clasico)
		(nombre "Ensalada Cesar")
		(orden primero)
		(precio 11)
		(preparacion otro)
		(temperatura frio)
		(tipo entrante)
		(vegetariano TRUE))

	([ontologia_Class20045] of  Ingrediente

		(nombre "pan tostado")
		(pertenece
			[ontologia_Class20085]
			[ontologia_Class40032]
			[ontologia_Class20044]))

	([ontologia_Class20046] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20009]
			[ontologia_Class20010])
		(epoca siempre)
		(estilo sibarita clasico regional)
		(marida_con [ontologia_Class20052])
		(nombre "Tabla de quesos")
		(orden primero)
		(precio 12)
		(preparacion otro)
		(region Francia)
		(temperatura frio)
		(tipo entrante)
		(vegetariano TRUE))

	([ontologia_Class20047] of  Ingrediente

		(nombre "espinaca")
		(pertenece [ontologia_Class20044]))

	([ontologia_Class20049] of  Agua

		(precio 2)
		(tipo_ag mineral))

	([ontologia_Class20050] of  Agua

		(precio 3)
		(tipo_ag gas))

	([ontologia_Class20051] of  Cerveza

		(precio 5)
		(tipo_cer cebada))

	([ontologia_Class20052] of  Cerveza

		(precio 5)
		(tipo_cer trigo))

	([ontologia_Class20053] of  Refresco

		(precio 3)
		(tipo_ref cocacola))

	([ontologia_Class20054] of  Refresco

		(precio 3)
		(tipo_ref fanta))

	([ontologia_Class20055] of  Refresco

		(precio 4)
		(tipo_ref nestea))

	([ontologia_Class20056] of  Vino

		(precio 12)
		(tipo_vin tinto))

	([ontologia_Class20057] of  Vino

		(precio 13)
		(tipo_vin blanco))

	([ontologia_Class20058] of  Vino

		(precio 12)
		(tipo_vin rosado))

	([ontologia_Class20059] of  Vino

		(precio 16)
		(tipo_vin cava))

	([ontologia_Class20060] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20043]
			[ontologia_Class20036]
			[ontologia_Class20030]
			[ontologia_Class20008]
			[ontologia_Class20037]
			[ontologia_Class20016]
			[ontologia_Class20035]
			[ontologia_Class20019])
		(epoca siempre)
		(estilo clasico)
		(nombre "Ensalada campera")
		(orden primero)
		(precio 11)
		(preparacion otro)
		(temperatura frio)
		(tipo entrante)
		(vegetariano FALSE))

	([ontologia_Class20061] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20020]
			[ontologia_Class20017]
			[ontologia_Class20022]
			[ontologia_Class20033]
			[ontologia_Class20016]
			[ontologia_Class20019])
		(epoca primavera otono invierno)
		(estilo moderno sibarita)
		(nombre "Ensalada de citricos")
		(orden primero)
		(precio 13)
		(preparacion otro)
		(temperatura frio)
		(tipo entrante)
		(vegetariano TRUE))

	([ontologia_Class20062] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20015]
			[ontologia_Class20064]
			[ontologia_Class20063]
			[ontologia_Class30010]
			[ontologia_Class20026]
			[ontologia_Class20016]
			[ontologia_Class20019])
		(epoca siempre)
		(estilo clasico sibarita)
		(nombre "Ensalada de jamon y manzana")
		(orden primero)
		(precio 15)
		(preparacion otro)
		(temperatura frio)
		(tipo entrante)
		(vegetariano FALSE))

	([ontologia_Class20063] of  Ingrediente

		(nombre "manzana")
		(pertenece
			[ontologia_Class30002]
			[ontologia_Class30003]
			[ontologia_Class40052]
			[ontologia_Class20062]))

	([ontologia_Class20064] of  Ingrediente

		(nombre "jamon")
		(pertenece
			[ontologia_Class20076]
			[ontologia_Class20078]
			[ontologia_Class30058]
			[ontologia_Class20062]
			[ontologia_Class10000]))

	([ontologia_Class20065] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20066]
			[ontologia_Class20030]
			[ontologia_Class20010]
			[ontologia_Class20031]
			[ontologia_Class20009])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20057])
		(nombre "Gougeres")
		(orden primero)
		(precio 10)
		(preparacion frito)
		(region Francia)
		(temperatura caliente)
		(tipo entrante)
		(vegetariano TRUE))

	([ontologia_Class20066] of  Ingrediente

		(nombre "harina")
		(pertenece
			[ontologia_Class20069]
			[ontologia_Class20076]
			[ontologia_Class20077]
			[ontologia_Class30002]
			[ontologia_Class30008]
			[ontologia_Class30009]
			[ontologia_Class30017]
			[ontologia_Class30033]
			[ontologia_Class30035]
			[ontologia_Class30037]
			[ontologia_Class30039]
			[ontologia_Class30041]
			[ontologia_Class30042]
			[ontologia_Class30043]
			[ontologia_Class30046]
			[ontologia_Class30047]
			[ontologia_Class30048]
			[ontologia_Class40012]
			[ontologia_Class40021]
			[ontologia_Class40035]
			[ontologia_Class40036]
			[ontologia_Class40041]
			[ontologia_Class40042]
			[ontologia_Class40050]
			[ontologia_Class40052]
			[ontologia_Class40054]
			[ontologia_Class40055]
			[ontologia_Class40056]))

	([ontologia_Class20069] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20070]
			[ontologia_Class20066]
			[ontologia_Class20031]
			[ontologia_Class20030]
			[ontologia_Class20029])
		(epoca siempre)
		(estilo clasico)
		(nombre "Croquetas de bacalao")
		(orden primero)
		(precio 7)
		(preparacion rebozado)
		(temperatura caliente)
		(tipo entrante)
		(vegetariano FALSE))

	([ontologia_Class20070] of  Ingrediente

		(nombre "bacalao")
		(pertenece
			[ontologia_Class20083]
			[ontologia_Class40032]
			[ontologia_Class40040]
			[ontologia_Class40041]))

	([ontologia_Class20071] of  Ingrediente

		(nombre "pollo")
		(pertenece
			[ontologia_Class20077]
			[ontologia_Class30051]
			[ontologia_Class40018]
			[ontologia_Class40020]
			[ontologia_Class20006]))

	([ontologia_Class20072] of  Ingrediente

		(nombre "melon")
		(pertenece
			[ontologia_Class20078]
			[ontologia_Class30003]
			[ontologia_Class40049]))

	([ontologia_Class20073] of  Ingrediente

		(nombre "arroz")
		(pertenece
			[ontologia_Class20083]
			[ontologia_Class30049]
			[ontologia_Class30051]
			[ontologia_Class30053]
			[ontologia_Class30054]
			[ontologia_Class30056]
			[ontologia_Class30057]
			[ontologia_Class30058]
			[ontologia_Class30060]
			[ontologia_Class40009]))

	([ontologia_Class20074] of  Ingrediente

		(nombre "salmon")
		(pertenece
			[ontologia_Class20083]
			[ontologia_Class40030]))

	([ontologia_Class20075] of  Ingrediente

		(nombre "yogur")
		(pertenece
			[ontologia_Class20083]
			[ontologia_Class20044]))

	([ontologia_Class20076] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20066]
			[ontologia_Class20030]
			[ontologia_Class20031]
			[ontologia_Class20064]
			[ontologia_Class20029])
		(epoca siempre)
		(estilo clasico)
		(nombre "Croquetas de jamon")
		(orden primero)
		(precio 8)
		(preparacion rebozado)
		(temperatura caliente)
		(tipo entrante)
		(vegetariano FALSE))

	([ontologia_Class20077] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20066]
			[ontologia_Class20030]
			[ontologia_Class20031]
			[ontologia_Class20029]
			[ontologia_Class20071])
		(epoca siempre)
		(estilo clasico)
		(nombre "Croquetas de pollo")
		(orden primero)
		(precio 7)
		(preparacion rebozado)
		(temperatura caliente)
		(tipo entrante)
		(vegetariano FALSE))

	([ontologia_Class20078] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20072]
			[ontologia_Class30010]
			[ontologia_Class20064])
		(epoca verano)
		(estilo moderno)
		(nombre "Melon con jamon")
		(orden primero)
		(precio 9)
		(preparacion otro)
		(temperatura frio)
		(tipo entrante)
		(vegetariano FALSE))

	([ontologia_Class20079] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20008]
			[ontologia_Class20012]
			[ontologia_Class20080]
			[ontologia_Class20081]
			[ontologia_Class20036]
			[ontologia_Class20035]
			[ontologia_Class20019])
		(epoca verano primavera)
		(estilo clasico regional)
		(nombre "Gazpacho")
		(orden primero)
		(precio 5)
		(preparacion sopa/crema)
		(region Espana)
		(temperatura frio)
		(tipo entrante)
		(vegetariano TRUE))

	([ontologia_Class20080] of  Ingrediente

		(nombre "ajo")
		(pertenece
			[ontologia_Class30011]
			[ontologia_Class30019]
			[ontologia_Class30021]
			[ontologia_Class30029]
			[ontologia_Class40023]
			[ontologia_Class40025]
			[ontologia_Class40032]
			[ontologia_Class40033]))

	([ontologia_Class20081] of  Ingrediente

		(nombre "pepino")
		(pertenece
			[ontologia_Class20085]
			[ontologia_Class20079]))

	([ontologia_Class20082] of  Ingrediente

		(nombre "sandia")
		(pertenece
			[ontologia_Class20085]
			[ontologia_Class30003]))

	([ontologia_Class20083] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20073]
			[ontologia_Class20030]
			[ontologia_Class20031]
			[ontologia_Class20074]
			[ontologia_Class20010]
			[ontologia_Class20070]
			[ontologia_Class20075])
		(epoca siempre)
		(estilo clasico regional)
		(nombre "Kedgeree")
		(orden primero)
		(precio 6)
		(preparacion otro)
		(region UK)
		(temperatura frio)
		(tipo entrante)
		(vegetariano FALSE))

	([ontologia_Class20085] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20082]
			[ontologia_Class20008]
			[ontologia_Class20045]
			[ontologia_Class20081])
		(epoca verano)
		(estilo clasico)
		(nombre "Crema de sandia")
		(orden primero)
		(precio 5)
		(preparacion sopa/crema)
		(temperatura frio)
		(tipo entrante)
		(vegetariano TRUE))

	([ontologia_Class30000] of  Plato

		(compl media)
		(contiene
			[ontologia_Class40047]
			[ontologia_Class20030]
			[ontologia_Class20010]
			[ontologia_Class20007]
			[ontologia_Class30040])
		(epoca siempre)
		(estilo clasico regional)
		(nombre "Mousse de chocolate")
		(orden postre)
		(precio 6)
		(preparacion otro)
		(region Francia)
		(temperatura frio)
		(tipo pastel)
		(vegetariano TRUE))

	([ontologia_Class30001] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20030]
			[ontologia_Class20010]
			[ontologia_Class20007]
			[ontologia_Class20017]
			[ontologia_Class20009]
			[ontologia_Class20015]
			[ontologia_Class20014])
		(epoca siempre)
		(estilo clasico)
		(nombre "Tarta de queso")
		(orden postre)
		(precio 5)
		(preparacion horno)
		(temperatura frio)
		(tipo pastel)
		(vegetariano TRUE))

	([ontologia_Class30002] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20015]
			[ontologia_Class20066]
			[ontologia_Class20030]
			[ontologia_Class20063])
		(epoca siempre)
		(estilo clasico)
		(nombre "Tarta de manzana")
		(orden postre)
		(precio 6)
		(preparacion horno)
		(temperatura frio)
		(tipo pastel)
		(vegetariano TRUE))

	([ontologia_Class30003] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20015]
			[ontologia_Class20072]
			[ontologia_Class20017]
			[ontologia_Class20082]
			[ontologia_Class30050]
			[ontologia_Class20014]
			[ontologia_Class20063])
		(epoca primavera invierno)
		(estilo clasico)
		(nombre "Macedonia de frutas")
		(orden postre)
		(precio 5)
		(preparacion otro)
		(temperatura frio)
		(tipo fruta)
		(vegetariano FALSE))

	([ontologia_Class30004] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20015]
			[ontologia_Class20017])
		(epoca siempre)
		(estilo clasico)
		(nombre "Zumo de naranja")
		(orden postre)
		(precio 3)
		(preparacion otro)
		(temperatura frio)
		(tipo fruta)
		(vegetariano TRUE))

	([ontologia_Class30005] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20015]
			[ontologia_Class20010]
			[ontologia_Class30040]
			[ontologia_Class20014])
		(epoca invierno primavera)
		(estilo clasico)
		(nombre "Fresas con nata")
		(orden postre)
		(precio 5)
		(preparacion otro)
		(temperatura frio)
		(tipo fruta)
		(vegetariano TRUE))

	([ontologia_Class30006] of  Plato

		(compl baja)
		(contiene [ontologia_Class20015])
		(epoca siempre)
		(estilo clasico)
		(nombre "Fruta del tiempo")
		(orden postre)
		(precio 3)
		(preparacion otro)
		(temperatura frio)
		(tipo fruta)
		(vegetariano TRUE))

	([ontologia_Class30007] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class30052]
			[ontologia_Class20039]
			[ontologia_Class20008]
			[ontologia_Class30016]
			[ontologia_Class20041])
		(epoca siempre)
		(estilo clasico)
		(marida_con [ontologia_Class20058])
		(nombre "Conejo estofado")
		(orden segundo)
		(precio 19)
		(preparacion estofado)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class30008] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20066]
			[ontologia_Class20010]
			[ontologia_Class20007]
			[ontologia_Class20031]
			[ontologia_Class20009]
			[ontologia_Class20028])
		(epoca primavera otono invierno)
		(estilo clasico)
		(marida_con [ontologia_Class20057])
		(nombre "Coliflor con bechamel")
		(orden ambos)
		(precio 12)
		(preparacion horno)
		(temperatura caliente)
		(tipo verdura)
		(vegetariano TRUE))

	([ontologia_Class30009] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20066]
			[ontologia_Class20010]
			[ontologia_Class20007]
			[ontologia_Class20031]
			[ontologia_Class20039]
			[ontologia_Class20009]
			[ontologia_Class30010])
		(epoca siempre)
		(estilo clasico)
		(nombre "Patatas rellenas")
		(orden ambos)
		(precio 9)
		(preparacion horno)
		(temperatura caliente)
		(tipo verdura)
		(vegetariano FALSE))

	([ontologia_Class30010] of  Ingrediente

		(nombre "cerdo")
		(pertenece
			[ontologia_Class30021]
			[ontologia_Class30027]
			[ontologia_Class30029]
			[ontologia_Class30033]
			[ontologia_Class30037]
			[ontologia_Class30041]
			[ontologia_Class30047]
			[ontologia_Class30051]
			[ontologia_Class30058]
			[ontologia_Class40009]
			[ontologia_Class40011]
			[ontologia_Class40025]))

	([ontologia_Class30011] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20080]
			[ontologia_Class30012])
		(epoca otono)
		(estilo clasico sibarita)
		(marida_con [ontologia_Class20057])
		(nombre "Setas a la parrilla")
		(orden primero)
		(precio 10)
		(preparacion plancha)
		(temperatura caliente)
		(tipo verdura)
		(vegetariano TRUE))

	([ontologia_Class30012] of  Ingrediente

		(nombre "seta")
		(pertenece
			[ontologia_Class30039]
			[ontologia_Class30057]
			[ontologia_Class40008]
			[ontologia_Class30011]))

	([ontologia_Class30013] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20030]
			[ontologia_Class20039]
			[ontologia_Class10004])
		(epoca siempre)
		(estilo clasico regional)
		(nombre "Tortilla paisana")
		(orden primero)
		(precio 8)
		(preparacion frito)
		(region Espana)
		(temperatura caliente)
		(tipo verdura)
		(vegetariano TRUE))

	([ontologia_Class30014] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class20030]
			[ontologia_Class20039])
		(epoca siempre)
		(estilo clasico regional)
		(nombre "Tortilla de patatas")
		(orden primero)
		(precio 9)
		(preparacion frito)
		(region Espana)
		(temperatura caliente)
		(tipo verdura)
		(vegetariano TRUE))

	([ontologia_Class30015] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class30016]
			[ontologia_Class10001])
		(epoca primavera otono invierno)
		(estilo clasico regional)
		(marida_con [ontologia_Class20052])
		(nombre "Sauerkraut")
		(orden primero)
		(precio 12)
		(preparacion otro)
		(region Alemania)
		(temperatura frio)
		(tipo verdura)
		(vegetariano TRUE))

	([ontologia_Class30016] of  Ingrediente

		(nombre "especia")
		(pertenece
			[ontologia_Class30035]
			[ontologia_Class30060]
			[ontologia_Class40012]
			[ontologia_Class40015]
			[ontologia_Class40023]
			[ontologia_Class40025]
			[ontologia_Class40030]
			[ontologia_Class40032]
			[ontologia_Class40033]
			[ontologia_Class40034]))

	([ontologia_Class30017] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20066]
			[ontologia_Class20031]
			[ontologia_Class20039])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20057])
		(nombre "Polenta con patatas")
		(orden primero)
		(precio 10)
		(preparacion otro)
		(region Italia)
		(temperatura caliente)
		(tipo verdura)
		(vegetariano TRUE))

	([ontologia_Class30018] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class20008]
			[ontologia_Class10005]
			[ontologia_Class10004])
		(epoca siempre)
		(estilo sibarita moderno regional)
		(marida_con [ontologia_Class20057])
		(nombre "Ratatouille")
		(orden segundo)
		(precio 13)
		(preparacion horno)
		(region Francia)
		(temperatura caliente)
		(tipo verdura)
		(vegetariano TRUE))

	([ontologia_Class30019] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20080]
			[ontologia_Class20036]
			[ontologia_Class30020]
			[ontologia_Class20039]
			[ontologia_Class10004]
			[ontologia_Class20040]
			[ontologia_Class20041])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20056])
		(nombre "Potaje de garbanzos")
		(orden ambos)
		(precio 7)
		(preparacion estofado)
		(region Espana)
		(temperatura caliente)
		(tipo legumbre)
		(vegetariano TRUE))

	([ontologia_Class30020] of  Ingrediente

		(nombre "garbanzo")
		(pertenece [ontologia_Class30019]))

	([ontologia_Class30021] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20080]
			[ontologia_Class30010]
			[ontologia_Class30023]
			[ontologia_Class30022]
			[ontologia_Class30025]
			[ontologia_Class30026])
		(epoca otono invierno)
		(estilo clasico regional)
		(marida_con [ontologia_Class20056])
		(nombre "Fabada asturiana")
		(orden ambos)
		(precio 9)
		(preparacion estofado)
		(region Espana)
		(temperatura caliente)
		(tipo legumbre)
		(vegetariano FALSE))

	([ontologia_Class30022] of  Ingrediente

		(nombre "faba")
		(pertenece [ontologia_Class30021]))

	([ontologia_Class30023] of  Ingrediente

		(nombre "chorizo")
		(pertenece
			[ontologia_Class30027]
			[ontologia_Class30021]))

	([ontologia_Class30025] of  Ingrediente

		(nombre "morcilla")
		(pertenece
			[ontologia_Class30047]
			[ontologia_Class40009]
			[ontologia_Class30021]))

	([ontologia_Class30026] of  Ingrediente

		(nombre "panceta")
		(pertenece
			[ontologia_Class30027]
			[ontologia_Class30033]
			[ontologia_Class30021]))

	([ontologia_Class30027] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class30028]
			[ontologia_Class30010]
			[ontologia_Class30023]
			[ontologia_Class30026]
			[ontologia_Class20035])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20056])
		(nombre "Lentejas estofadas")
		(orden ambos)
		(precio 8)
		(preparacion estofado)
		(region Espana)
		(temperatura caliente)
		(tipo legumbre)
		(vegetariano FALSE))

	([ontologia_Class30028] of  Ingrediente

		(nombre "lenteja")
		(pertenece [ontologia_Class30027]))

	([ontologia_Class30029] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20080]
			[ontologia_Class30031]
			[ontologia_Class30032]
			[ontologia_Class30010]
			[ontologia_Class30030])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20056])
		(nombre "Guisantes con butifarra")
		(orden ambos)
		(precio 9)
		(preparacion estofado)
		(region Espana)
		(temperatura caliente)
		(tipo legumbre)
		(vegetariano FALSE))

	([ontologia_Class30030] of  Ingrediente

		(nombre "guisante")
		(pertenece
			[ontologia_Class30058]
			[ontologia_Class30029]))

	([ontologia_Class30031] of  Ingrediente

		(nombre "butifarra blanca")
		(pertenece
			[ontologia_Class20002]
			[ontologia_Class20003]
			[ontologia_Class30029]))

	([ontologia_Class30032] of  Ingrediente

		(nombre "butifarra negra")
		(pertenece [ontologia_Class30029]))

	([ontologia_Class30033] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class30010]
			[ontologia_Class20066]
			[ontologia_Class20030]
			[ontologia_Class20010]
			[ontologia_Class30034]
			[ontologia_Class20009]
			[ontologia_Class30026])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20057])
		(nombre "Espaguetis carbonara")
		(orden primero)
		(precio 15)
		(preparacion otro)
		(region Italia)
		(temperatura caliente)
		(tipo pasta)
		(vegetariano FALSE))

	([ontologia_Class30034] of  Ingrediente

		(nombre "pasta")
		(pertenece
			[ontologia_Class30035]
			[ontologia_Class30037]
			[ontologia_Class30039]
			[ontologia_Class30041]
			[ontologia_Class30042]
			[ontologia_Class30043]
			[ontologia_Class30046]
			[ontologia_Class30047]
			[ontologia_Class30048]
			[ontologia_Class40042]))

	([ontologia_Class30035] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class30016]
			[ontologia_Class20066]
			[ontologia_Class20010]
			[ontologia_Class30034]
			[ontologia_Class30036]
			[ontologia_Class20009])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20058])
		(nombre "Espaguetis pesto")
		(orden primero)
		(precio 16)
		(preparacion otro)
		(region Italia)
		(temperatura caliente)
		(tipo pasta)
		(vegetariano TRUE))

	([ontologia_Class30036] of  Ingrediente

		(nombre "pinon")
		(pertenece
			[ontologia_Class40020]
			[ontologia_Class30035]))

	([ontologia_Class30037] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class30010]
			[ontologia_Class20066]
			[ontologia_Class30034]
			[ontologia_Class20008]
			[ontologia_Class30038])
		(epoca siempre)
		(estilo clasico regional)
		(nombre "Espaguetis bolognesa")
		(orden primero)
		(precio 13)
		(preparacion otro)
		(region Italia)
		(temperatura caliente)
		(tipo pasta)
		(vegetariano FALSE))

	([ontologia_Class30038] of  Ingrediente

		(nombre "ternera")
		(pertenece
			[ontologia_Class30041]
			[ontologia_Class30042]
			[ontologia_Class30047]
			[ontologia_Class40008]
			[ontologia_Class40010]
			[ontologia_Class40012]
			[ontologia_Class40014]
			[ontologia_Class40021]
			[ontologia_Class40023]
			[ontologia_Class40025]))

	([ontologia_Class30039] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20066]
			[ontologia_Class30040]
			[ontologia_Class30034]
			[ontologia_Class30012])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20057])
		(nombre "Espaguetis al funghi")
		(orden primero)
		(precio 17)
		(preparacion otro)
		(region Italia)
		(temperatura caliente)
		(tipo pasta)
		(vegetariano TRUE))

	([ontologia_Class30040] of  Ingrediente

		(nombre "nata")
		(pertenece
			[ontologia_Class40043]
			[ontologia_Class40046]
			[ontologia_Class40048]
			[ontologia_Class40049]
			[ontologia_Class40050]
			[ontologia_Class40056]
			[ontologia_Class30039]
			[ontologia_Class30005]
			[ontologia_Class30000]))

	([ontologia_Class30041] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class30010]
			[ontologia_Class20066]
			[ontologia_Class20010]
			[ontologia_Class30034]
			[ontologia_Class20009]
			[ontologia_Class30038]
			[ontologia_Class20008])
		(epoca siempre)
		(estilo clasico)
		(nombre "Macarrones con carne picada")
		(orden primero)
		(precio 12)
		(preparacion horno)
		(temperatura caliente)
		(tipo pasta)
		(vegetariano FALSE))

	([ontologia_Class30042] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class20066]
			[ontologia_Class20010]
			[ontologia_Class30034]
			[ontologia_Class20009]
			[ontologia_Class30038]
			[ontologia_Class20008]
			[ontologia_Class20031])
		(epoca siempre)
		(estilo clasico)
		(marida_con [ontologia_Class20057])
		(nombre "Canelones gratinados")
		(orden primero)
		(precio 15)
		(preparacion horno)
		(temperatura caliente)
		(tipo pasta)
		(vegetariano FALSE))

	([ontologia_Class30043] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class20066]
			[ontologia_Class20031]
			[ontologia_Class30045]
			[ontologia_Class30034]
			[ontologia_Class30044]
			[ontologia_Class20008]
			[ontologia_Class20010])
		(epoca siempre)
		(estilo clasico sibarita)
		(marida_con [ontologia_Class20059])
		(nombre "Canelones de mar")
		(orden primero)
		(precio 17)
		(preparacion horno)
		(temperatura caliente)
		(tipo pasta)
		(vegetariano FALSE))

	([ontologia_Class30044] of  Ingrediente

		(nombre "pescado")
		(pertenece
			[ontologia_Class30053]
			[ontologia_Class30054]
			[ontologia_Class40033]
			[ontologia_Class40034]
			[ontologia_Class40036]
			[ontologia_Class40038]
			[ontologia_Class40039]
			[ontologia_Class40042]
			[ontologia_Class30043]))

	([ontologia_Class30045] of  Ingrediente

		(nombre "marisco")
		(pertenece
			[ontologia_Class30053]
			[ontologia_Class30054]
			[ontologia_Class40020]
			[ontologia_Class40037]
			[ontologia_Class40042]
			[ontologia_Class30043]))

	([ontologia_Class30046] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20043]
			[ontologia_Class20066]
			[ontologia_Class20010]
			[ontologia_Class30034]
			[ontologia_Class20009]
			[ontologia_Class20036]
			[ontologia_Class20008])
		(epoca siempre)
		(estilo clasico)
		(marida_con [ontologia_Class20057])
		(nombre "Espirales con atun")
		(orden primero)
		(precio 16)
		(preparacion otro)
		(temperatura caliente)
		(tipo pasta)
		(vegetariano FALSE))

	([ontologia_Class30047] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class30010]
			[ontologia_Class20066]
			[ontologia_Class30034]
			[ontologia_Class30038]
			[ontologia_Class30025])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20056])
		(nombre "Fideos a la cazuela")
		(orden ambos)
		(precio 14)
		(preparacion estofado)
		(region Espana)
		(temperatura caliente)
		(tipo pasta)
		(vegetariano FALSE))

	([ontologia_Class30048] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20066]
			[ontologia_Class20010]
			[ontologia_Class20031]
			[ontologia_Class30034]
			[ontologia_Class20009]
			[ontologia_Class20036])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20052])
		(nombre "Spatzle con mantequilla y queso")
		(orden primero)
		(precio 15)
		(preparacion otro)
		(region Alemania)
		(temperatura caliente)
		(tipo pasta)
		(vegetariano FALSE))

	([ontologia_Class30049] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20073]
			[ontologia_Class20036]
			[ontologia_Class20008]
			[ontologia_Class20015]
			[ontologia_Class30050])
		(epoca siempre)
		(estilo clasico regional)
		(nombre "Arroz a la cubana")
		(orden primero)
		(precio 11)
		(preparacion hervido)
		(region Espana)
		(temperatura caliente)
		(tipo arroz)
		(vegetariano TRUE))

	([ontologia_Class30050] of  Ingrediente

		(nombre "platano")
		(pertenece
			[ontologia_Class30049]
			[ontologia_Class30003]))

	([ontologia_Class30051] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20073]
			[ontologia_Class20036]
			[ontologia_Class30010]
			[ontologia_Class30052]
			[ontologia_Class20008]
			[ontologia_Class20040]
			[ontologia_Class20071])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20057])
		(nombre "Paella valenciana")
		(orden primero)
		(precio 14)
		(preparacion hervido)
		(region Espana)
		(temperatura caliente)
		(tipo arroz)
		(vegetariano FALSE))

	([ontologia_Class30052] of  Ingrediente

		(nombre "conejo")
		(pertenece
			[ontologia_Class30007]
			[ontologia_Class30051]))

	([ontologia_Class30053] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20073]
			[ontologia_Class20036]
			[ontologia_Class20008]
			[ontologia_Class30045]
			[ontologia_Class30044])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20057])
		(nombre "Paella")
		(orden primero)
		(precio 19)
		(preparacion hervido)
		(region Espana)
		(temperatura caliente)
		(tipo arroz)
		(vegetariano FALSE))

	([ontologia_Class30054] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20073]
			[ontologia_Class30055]
			[ontologia_Class20036]
			[ontologia_Class20008]
			[ontologia_Class30045]
			[ontologia_Class30044])
		(epoca siempre)
		(estilo clasico sibarita)
		(marida_con [ontologia_Class20059])
		(nombre "Arroz negro")
		(orden primero)
		(precio 21)
		(preparacion hervido)
		(temperatura caliente)
		(tipo arroz)
		(vegetariano FALSE))

	([ontologia_Class30055] of  Ingrediente

		(nombre "calamar")
		(pertenece
			[ontologia_Class40035]
			[ontologia_Class30054]))

	([ontologia_Class30056] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20073]
			[ontologia_Class20010]
			[ontologia_Class20031]
			[ontologia_Class20009])
		(epoca siempre)
		(estilo clasico regional sibarita)
		(marida_con [ontologia_Class20057])
		(nombre "Risotto de queso")
		(orden primero)
		(precio 22)
		(preparacion hervido)
		(region Italia)
		(temperatura caliente)
		(tipo arroz)
		(vegetariano TRUE))

	([ontologia_Class30057] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20073]
			[ontologia_Class20010]
			[ontologia_Class20009]
			[ontologia_Class20036]
			[ontologia_Class20007]
			[ontologia_Class30059]
			[ontologia_Class30012])
		(epoca otono invierno)
		(estilo clasico regional sibarita)
		(marida_con [ontologia_Class20058])
		(nombre "Risotto de setas")
		(orden primero)
		(precio 20)
		(preparacion hervido)
		(region Italia)
		(temperatura caliente)
		(tipo arroz)
		(vegetariano TRUE))

	([ontologia_Class30058] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20073]
			[ontologia_Class30010]
			[ontologia_Class20030]
			[ontologia_Class30030]
			[ontologia_Class20064])
		(epoca siempre)
		(estilo clasico)
		(marida_con [ontologia_Class20057])
		(nombre "Arroz frito")
		(orden ambos)
		(precio 13)
		(preparacion frito)
		(temperatura caliente)
		(tipo arroz)
		(vegetariano FALSE))

	([ontologia_Class30059] of  Ingrediente

		(nombre "puerro")
		(pertenece [ontologia_Class30057]))

	([ontologia_Class30060] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20073]
			[ontologia_Class30061]
			[ontologia_Class30016]
			[ontologia_Class20039]
			[ontologia_Class20041])
		(epoca siempre)
		(estilo clasico sibarita)
		(nombre "Arroz al curry")
		(orden primero)
		(precio 16)
		(preparacion otro)
		(temperatura caliente)
		(tipo arroz)
		(vegetariano TRUE))

	([ontologia_Class30061] of  Ingrediente

		(nombre "curry")
		(pertenece [ontologia_Class30060]))

	([ontologia_Class40007] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20015]
			[ontologia_Class40058]
			[ontologia_Class40024]
			[ontologia_Class50008])
		(epoca otono)
		(estilo clasico)
		(marida_con [ontologia_Class20059])
		(nombre "Uvas flambeadas")
		(orden postre)
		(precio 4)
		(preparacion otro)
		(temperatura frio)
		(tipo fruta)
		(vegetariano TRUE))

	([ontologia_Class40008] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class30038]
			[ontologia_Class20008]
			[ontologia_Class30012])
		(epoca otono verano)
		(estilo clasico sibarita)
		(marida_con [ontologia_Class20056])
		(nombre "Ternera con setas")
		(orden segundo)
		(precio 22)
		(preparacion estofado)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class40009] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20073]
			[ontologia_Class30010]
			[ontologia_Class20039]
			[ontologia_Class30025])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20056])
		(nombre "Morcilla de Burgos")
		(orden segundo)
		(precio 19)
		(preparacion plancha)
		(region Espana)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class40010] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20039]
			[ontologia_Class30038])
		(epoca siempre)
		(estilo clasico)
		(nombre "Filete a la plancha")
		(orden segundo)
		(precio 13)
		(preparacion plancha)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class40011] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class30010]
			[ontologia_Class20039]
			[ontologia_Class20008])
		(epoca siempre)
		(estilo clasico)
		(nombre "Chuletas de cerdo")
		(orden segundo)
		(precio 14)
		(preparacion plancha)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class40012] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class30016]
			[ontologia_Class20039]
			[ontologia_Class30038]
			[ontologia_Class20066]
			[ontologia_Class20030]
			[ontologia_Class20031]
			[ontologia_Class40013])
		(epoca siempre)
		(estilo clasico regional)
		(nombre "Steak and kidney pie")
		(orden segundo)
		(precio 17)
		(preparacion horno)
		(region UK)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class40013] of  Ingrediente

		(nombre "viscera")
		(pertenece
			[ontologia_Class40025]
			[ontologia_Class40012]))

	([ontologia_Class40014] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20039]
			[ontologia_Class30038]
			[ontologia_Class20016]
			[ontologia_Class20035])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20051])
		(nombre "Rosbif")
		(orden segundo)
		(precio 20)
		(preparacion brasa)
		(region UK)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class40015] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class40016]
			[ontologia_Class30016]
			[ontologia_Class20017]
			[ontologia_Class40017])
		(epoca siempre)
		(estilo clasico regional sibarita)
		(marida_con [ontologia_Class20058])
		(nombre "Magret de pato")
		(orden segundo)
		(precio 24)
		(preparacion plancha)
		(region Francia)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class40016] of  Ingrediente

		(nombre "pato")
		(pertenece [ontologia_Class40015]))

	([ontologia_Class40017] of  Ingrediente

		(nombre "cuscus")
		(pertenece [ontologia_Class40015]))

	([ontologia_Class40018] of  Plato

		(compl media)
		(contiene
			[ontologia_Class40019]
			[ontologia_Class20015]
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class20071])
		(epoca siempre)
		(estilo clasico)
		(marida_con [ontologia_Class20057])
		(nombre "Pollo con ciruelas")
		(orden segundo)
		(precio 22)
		(preparacion horno)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class40019] of  Ingrediente

		(nombre "ciruela")
		(pertenece [ontologia_Class40018]))

	([ontologia_Class40020] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class20008]
			[ontologia_Class30045]
			[ontologia_Class30036]
			[ontologia_Class20071])
		(epoca siempre)
		(estilo clasico)
		(marida_con [ontologia_Class20057])
		(nombre "Pollo con cigalas")
		(orden segundo)
		(precio 24)
		(preparacion estofado)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class40021] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class40022]
			[ontologia_Class20036]
			[ontologia_Class20017]
			[ontologia_Class30038]
			[ontologia_Class20066])
		(epoca primavera otono invierno)
		(estilo clasico regional sibarita)
		(marida_con [ontologia_Class20056])
		(nombre "Ossobuco a la milanesa")
		(orden segundo)
		(precio 23)
		(preparacion estofado)
		(region Italia)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class40022] of  Ingrediente

		(nombre "apio")
		(pertenece [ontologia_Class40021]))

	([ontologia_Class40023] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20080]
			[ontologia_Class20036]
			[ontologia_Class30016]
			[ontologia_Class30038]
			[ontologia_Class40024]
			[ontologia_Class20041])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20056])
		(nombre "Boeuf bourguignon")
		(orden segundo)
		(precio 27)
		(preparacion estofado)
		(region Francia)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class40024] of  Ingrediente

		(nombre "vino de cocina")
		(pertenece
			[ontologia_Class40023]
			[ontologia_Class40007]))

	([ontologia_Class40025] of  Plato

		(compl media)
		(contiene
			[ontologia_Class40026]
			[ontologia_Class30010]
			[ontologia_Class40028]
			[ontologia_Class30038]
			[ontologia_Class20080]
			[ontologia_Class30016]
			[ontologia_Class40013])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20051])
		(nombre "Hog's pudding")
		(orden ambos)
		(precio 18)
		(preparacion horno)
		(region UK)
		(temperatura caliente)
		(tipo carne)
		(vegetariano FALSE))

	([ontologia_Class40026] of  Ingrediente

		(nombre "avena")
		(pertenece [ontologia_Class40025]))

	([ontologia_Class40028] of  Ingrediente

		(nombre "pan")
		(pertenece
			[ontologia_Class40033]
			[ontologia_Class40052]
			[ontologia_Class40025]))

	([ontologia_Class40029] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20043]
			[ontologia_Class20036]
			[ontologia_Class20008])
		(epoca siempre)
		(estilo clasico)
		(marida_con [ontologia_Class20057])
		(nombre "Atun con tomate")
		(orden ambos)
		(precio 16)
		(preparacion otro)
		(temperatura caliente)
		(tipo pescado)
		(vegetariano FALSE))

	([ontologia_Class40030] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class30016]
			[ontologia_Class20074])
		(epoca siempre)
		(estilo clasico)
		(marida_con [ontologia_Class20057])
		(nombre "Salmon a la plancha")
		(orden segundo)
		(precio 17)
		(preparacion plancha)
		(temperatura caliente)
		(tipo pescado)
		(vegetariano FALSE))

	([ontologia_Class40032] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20080]
			[ontologia_Class20036]
			[ontologia_Class30016]
			[ontologia_Class20070]
			[ontologia_Class20045])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20057])
		(nombre "Brandada de bacalao")
		(orden primero)
		(precio 19)
		(preparacion hervido)
		(region Espana)
		(temperatura caliente)
		(tipo pescado)
		(vegetariano FALSE))

	([ontologia_Class40033] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20080]
			[ontologia_Class30016]
			[ontologia_Class20039]
			[ontologia_Class40028]
			[ontologia_Class30044])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20057])
		(nombre "Bullabesa")
		(orden primero)
		(precio 18)
		(preparacion hervido)
		(region Francia)
		(temperatura caliente)
		(tipo pescado)
		(vegetariano FALSE))

	([ontologia_Class40034] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class30016]
			[ontologia_Class20039]
			[ontologia_Class20008]
			[ontologia_Class30044])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20057])
		(nombre "Suquet")
		(orden segundo)
		(precio 20)
		(preparacion estofado)
		(region Espana)
		(temperatura caliente)
		(tipo pescado)
		(vegetariano FALSE))

	([ontologia_Class40035] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20066]
			[ontologia_Class30055])
		(epoca siempre)
		(estilo clasico regional)
		(nombre "Rabas")
		(orden primero)
		(precio 12)
		(preparacion frito)
		(region Espana)
		(temperatura caliente)
		(tipo pescado)
		(vegetariano FALSE))

	([ontologia_Class40036] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20066]
			[ontologia_Class30044])
		(epoca siempre)
		(estilo clasico regional)
		(nombre "Pescaito frito")
		(orden segundo)
		(precio 13)
		(preparacion frito)
		(region Espana)
		(temperatura caliente)
		(tipo pescado)
		(vegetariano FALSE))

	([ontologia_Class40037] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20039]
			[ontologia_Class30045])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20057])
		(nombre "Moules frites")
		(orden primero)
		(precio 15)
		(preparacion otro)
		(region Francia)
		(temperatura caliente)
		(tipo pescado)
		(vegetariano FALSE))

	([ontologia_Class40038] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20039]
			[ontologia_Class30044])
		(epoca siempre)
		(estilo clasico)
		(marida_con [ontologia_Class20057])
		(nombre "Merluza a la plancha")
		(orden segundo)
		(precio 12)
		(preparacion plancha)
		(temperatura caliente)
		(tipo pescado)
		(vegetariano FALSE))

	([ontologia_Class40039] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class20039]
			[ontologia_Class30044])
		(epoca siempre)
		(estilo clasico)
		(marida_con [ontologia_Class20057])
		(nombre "Dorada al horno")
		(orden segundo)
		(precio 19)
		(preparacion horno)
		(temperatura caliente)
		(tipo pescado)
		(vegetariano FALSE))

	([ontologia_Class40040] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class20008]
			[ontologia_Class20070]
			[ontologia_Class10005])
		(epoca siempre)
		(estilo clasico)
		(nombre "Bacalao con berenjenas")
		(orden ambos)
		(precio 16)
		(preparacion estofado)
		(temperatura caliente)
		(tipo pescado)
		(vegetariano FALSE))

	([ontologia_Class40041] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20066]
			[ontologia_Class20030]
			[ontologia_Class20039]
			[ontologia_Class20070])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20051])
		(nombre "Fish and chips")
		(orden primero)
		(precio 15)
		(preparacion rebozado)
		(region UK)
		(temperatura caliente)
		(tipo pescado)
		(vegetariano FALSE))

	([ontologia_Class40042] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20012]
			[ontologia_Class20036]
			[ontologia_Class20066]
			[ontologia_Class30034]
			[ontologia_Class20008]
			[ontologia_Class30045]
			[ontologia_Class30044])
		(epoca siempre)
		(estilo clasico regional)
		(marida_con [ontologia_Class20059])
		(nombre "Fideuada de marisco")
		(orden ambos)
		(precio 19)
		(preparacion hervido)
		(region Espana)
		(temperatura caliente)
		(tipo pescado)
		(vegetariano FALSE))

	([ontologia_Class40043] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20015]
			[ontologia_Class20010]
			[ontologia_Class30040])
		(epoca verano)
		(estilo clasico regional)
		(nombre "Gelato italiano")
		(orden postre)
		(precio 6)
		(preparacion otro)
		(region Italia)
		(temperatura frio)
		(tipo helado)
		(vegetariano TRUE))

	([ontologia_Class40044] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class40045]
			[ontologia_Class20015])
		(epoca verano)
		(estilo clasico)
		(nombre "Sorbete de limon")
		(orden postre)
		(precio 5)
		(preparacion otro)
		(temperatura frio)
		(tipo helado)
		(vegetariano TRUE))

	([ontologia_Class40045] of  Ingrediente

		(nombre "limon")
		(pertenece [ontologia_Class40044]))

	([ontologia_Class40046] of  Plato

		(compl media)
		(contiene
			[ontologia_Class40047]
			[ontologia_Class20010]
			[ontologia_Class20007]
			[ontologia_Class30040])
		(epoca verano)
		(estilo clasico)
		(nombre "Helado de chocolate")
		(orden postre)
		(precio 6)
		(preparacion otro)
		(temperatura frio)
		(tipo helado)
		(vegetariano TRUE))

	([ontologia_Class40047] of  Ingrediente

		(nombre "chocolate")
		(pertenece
			[ontologia_Class40050]
			[ontologia_Class40056]
			[ontologia_Class40046]
			[ontologia_Class30000]))

	([ontologia_Class40048] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20015]
			[ontologia_Class20010]
			[ontologia_Class20014]
			[ontologia_Class30040])
		(epoca verano)
		(estilo clasico)
		(nombre "Helado de fresa")
		(orden postre)
		(precio 4)
		(preparacion otro)
		(temperatura frio)
		(tipo helado)
		(vegetariano TRUE))

	([ontologia_Class40049] of  Plato

		(compl baja)
		(contiene
			[ontologia_Class20015]
			[ontologia_Class20010]
			[ontologia_Class20072]
			[ontologia_Class30040])
		(epoca verano)
		(estilo clasico)
		(nombre "Helado de melon")
		(orden postre)
		(precio 4)
		(preparacion otro)
		(temperatura frio)
		(tipo helado)
		(vegetariano TRUE))

	([ontologia_Class40050] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20015]
			[ontologia_Class20066]
			[ontologia_Class20030]
			[ontologia_Class20010]
			[ontologia_Class40051]
			[ontologia_Class40047]
			[ontologia_Class30040])
		(epoca primavera verano)
		(estilo clasico regional)
		(nombre "Pastel Selva Negra")
		(orden postre)
		(precio 7)
		(preparacion horno)
		(region Alemania)
		(temperatura frio)
		(tipo pastel)
		(vegetariano FALSE))

	([ontologia_Class40051] of  Ingrediente

		(nombre "cereza")
		(pertenece
			[ontologia_Class40055]
			[ontologia_Class40050]))

	([ontologia_Class40052] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class20066]
			[ontologia_Class20030]
			[ontologia_Class20031]
			[ontologia_Class40053]
			[ontologia_Class20015]
			[ontologia_Class20063]
			[ontologia_Class40028])
		(epoca invierno)
		(estilo clasico regional)
		(nombre "Pudin de navidad")
		(orden postre)
		(precio 6)
		(preparacion horno)
		(region UK)
		(temperatura frio)
		(tipo pastel)
		(vegetariano FALSE))

	([ontologia_Class40053] of  Ingrediente

		(nombre "fruto seco")
		(pertenece [ontologia_Class40052]))

	([ontologia_Class40054] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20015]
			[ontologia_Class20066]
			[ontologia_Class20031])
		(epoca siempre)
		(estilo clasico regional)
		(nombre "Crumble")
		(orden postre)
		(precio 5)
		(preparacion horno)
		(region UK)
		(temperatura frio)
		(tipo pastel)
		(vegetariano FALSE))

	([ontologia_Class40055] of  Plato

		(compl media)
		(contiene
			[ontologia_Class20015]
			[ontologia_Class20066]
			[ontologia_Class20030]
			[ontologia_Class20010]
			[ontologia_Class20007]
			[ontologia_Class40051])
		(epoca siempre)
		(estilo clasico regional)
		(nombre "Clafoutis")
		(orden postre)
		(precio 8)
		(preparacion horno)
		(region Francia)
		(temperatura frio)
		(tipo pastel)
		(vegetariano TRUE))

	([ontologia_Class40056] of  Plato

		(compl alta)
		(contiene
			[ontologia_Class40057]
			[ontologia_Class20066]
			[ontologia_Class20030]
			[ontologia_Class40058]
			[ontologia_Class20010]
			[ontologia_Class40047]
			[ontologia_Class30040])
		(epoca siempre)
		(estilo clasico regional)
		(nombre "Tiramisu")
		(orden postre)
		(precio 6)
		(preparacion otro)
		(region Italia)
		(temperatura frio)
		(tipo pastel)
		(vegetariano TRUE))

	([ontologia_Class40057] of  Ingrediente

		(nombre "cafe")
		(pertenece [ontologia_Class40056]))

	([ontologia_Class40058] of  Ingrediente

		(nombre "licor")
		(pertenece
			[ontologia_Class40056]
			[ontologia_Class40007]))

	([ontologia_Class50008] of  Ingrediente

		(nombre "uva")
		(pertenece [ontologia_Class40007]))
)
