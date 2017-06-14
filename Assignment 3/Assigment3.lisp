(load "fron")

(def-ontology framenet-ontology)

(def-class person 
    (name))

(def-class edible-good)

(def-class material)

(def-class container 
    ((container-material < material)))

(def-class movement
    ((movement-agent)
     (movement-undergoer)
     (movement-destination)))

(def-class apply-heat
    ((apply-heat-agent)
     (apply-heat-undergoer)))

(def-object sally 
    (a person (name sally)))

(def-object jim
    (a person (name jim)))

(def-object egg1
    (a edible-good))

(def-object egg2
    (a edible-good))

(def-object butter
    (a edible-good))

(def-object roast
    (a edible-good))

(def-object teflon 
    (a material))

(def-object teflon-pan
    (a container (container-material teflon)))

(def-object oven
    (a container))

(def-class cooking-type)
(def-object frying (a cooking-type))
(def-object browning (a cooking-type))

(def-event cooking
    :roles
    ((cooking-cook ?cook)
     (cooking-edible ?edible)
     (cooking-container ?container)
     (cooking-medium ?medium)
     (cooking-type ?cooking-type))
    :evokes
    ((a person ?cook)
     (a edible-good ?edible)
     (a container ?container)
     (a edible-good ?medium)
     (a cooking-type ?cooking-type))
    :states
    ((container-is-empty)
     (medium-is-in-container)
     (medium-is-hot)
     (edible-is-in-container)
     (edible-is-fried))
    :actions
    ((put-medium-in-container
       (:require container-is-empty)
       (:action (a movement 
                   (movement-agent ?cook)
                   (movement-undergoer ?medium)
                   (movement-destination ?container)))
       (:begin medium-is-in-container)
       (:end container-is-empty))
     (heat-medium
       (:require medium-is-in-container)
       (:action (a apply-heat
                   (apply-heat-agent ?cook)
                   (apply-heat-undergoer ?medium)))
       (:begin medium-is-hot))
     (put-edible-in-container
       (:require medium-is-hot)
       (:action (a movement
                   (movement-agent ?cook)
                   (movement-undergoer ?edible)
                   (movement-destination ?container)))
       (:begin edible-is-in-container))
     (heat-edible
       (:require edible-is-in-container)
       (:action (a apply-heat
                   (apply-heat-agent ?cook)
                   (apply-heat-undergoer ?edible)))
       (:begin edible-is-fried))))

(progn
  (def-world-model cooking-world)
  (instantiate sally)
  ; (instantiate jim)
  ; (instantiate teflon)
  ; (instantiate teflon-pan)
  (instantiate egg1)
  ; (instantiate egg2)
  (instantiate butter)
  ; (instantiate roast)
  ; (instantiate oven)
  (instantiate frying)
  ; (instantiate browning)
  (instantiate-event cooking
               (cooking-cook sally)
               (cooking-edible egg1)
               (cooking-medium butter)
               (cooking-type frying))
  ; (instantiate-event cooking
  ;              (cooking-cook sally)
  ;              (cooking-edible egg2)
  ;              (cooking-container teflon-pan)
  ;              (cooking-type frying))
  ; (instantiate-event cooking
  ;              (cooking-cook jim)
  ;              (cooking-edible roast)
  ;              (cooking-container oven)
  ;              (cooking-type browning))
  (html-world-model cooking-world))
