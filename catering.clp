
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

(deffunction pregunta-text(?pregunta)
  (format t "%s" ?pregunta)
  (bind ?resposta (read))
  ?resposta
)

(deffunction pregunta-num(?pregunta ?min ?max)

)
