(asdf:operate 'asdf:load-op :fron)

(in-package :fron)

(def-ontology framenet-ontology)

(def-class person
    ((gender)
     (name)))

(def-class edible-good ())

(def-class container
    ((volume)))

(def-class apply_heat
    ((cook (a person))
     (food)
     (heating_instrument)
     (container)))

(def-object remy ()
    (a person (name Remy) (gender male)))

(def-object egg ()
	(a edible-good))

(def-object pan ()
    (a container (volume 100ml)))


(progn
  (def-world-model cooking-world
    (instantiate remy)
    (instantiate egg)
    (instantiate pan)
    (print-world-model)))