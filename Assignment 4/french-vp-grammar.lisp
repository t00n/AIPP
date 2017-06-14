(asdf:operate 'asdf:load-op :fcg)
(in-package :fcg)

(activate-monitor trace-fcg-light)
(clear-page)

(def-fcg-constructions french-vp-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (verb-form set-of-features :verb-form))
  :fcg-configurations ((:production-order lex vp modality tense morph) ; vp modality tense morph)
                       (:parse-order morph lex vp modality tense) ; vp modality tense )
                        (:parse-goal-tests :no-applicable-cxns :connected-structure) 
                        (:production-goal-tests :no-applicable-cxns :connected-structure)
                        (:cxn-supplier-mode . :ordered-by-label-and-score)
                        (:node-tests :check-duplicate :restrict-search-depth :no-morphs-if-unconnected-structure)))


(defparameter *verb-form-list* '(indicative infinitive conditional subjunctive imperative participle perfect))

;; extends verb-form
(defmethod fcg-expand ((type (eql :verb-form)) &key value source bindings merge?)
  (declare (ignore source bindings merge?))
  (loop for form in *verb-form-list*
        collect (if (member form value) 
                    `(,form +)
                    `(,form -))))


;;; LEXICAL CONSTRUCTIONS ;;;
(def-fcg-cxn atterrir-lex
             ((?word
                (sem-cat (sem-class action)
                         (sem-function event))
                (args (?ev)))
              <-
              (?word
                (HASH meaning ((land ?ev)
                               (person ?ev ?person)
                               (number ?ev ?number)))
                --
                (syn-cat (lex-class verb)
                         (lemma "atterrir")
                         (perfect-aux "avoir")
                         (verb-form ?verb-form)
                         (tense ?tense)
                         (person ?person)
                         (number ?number))))
              :cxn-set lex)


;; perfect auxiliaries
(def-fcg-cxn perfect-aux-lex 
             ((?word
               (sem-cat (sem-class perfect-event)
                        (sem-function event))
              (args (?super-event ?event)))
              <-
              (?word
               (HASH meaning ((perfect ?super-event ?event)))
               --
               (syn-cat (lex-class perfect-aux)
                         (lemma ?lemma)
                         (verb-form ?verb-form)
                         (tense ?tense))))
             :cxn-set lex)

;; modal auxiliaries
  (def-fcg-cxn devoir-aux-lex 
               ((?word
                 (sem-cat (sem-class modality)
                          (sem-function event))
                (args (?super-event ?event)))
                <-
                (?word
                 (HASH meaning ((modality ?super-event ?event obligation)))
                 --
                 (syn-cat (lex-class modal-aux)
                           (lemma "devoir")
                           (verb-form ?verb-form)
                           (perfect-aux "avoir")
                           (tense ?tense)
                           (person ?person)
                           (number ?number))))
               :cxn-set lex)
  
  (def-fcg-cxn pouvoir-aux-lex 
               ((?word
                 (sem-cat (sem-class modality)
                          (sem-function event))
                (args (?super-event ?event)))
                <-
                (?word
                 (HASH meaning ((modality ?super-event ?event ability)))
                 --
                 (syn-cat (lex-class modal-aux)
                           (lemma "pouvoir")
                           (verb-form ?verb-form)
                           (perfect-aux "avoir")
                           (tense ?tense)
                           (person ?person)
                           (number ?number))))
               :cxn-set lex)
  
  (def-fcg-cxn vouloir-aux-lex 
               ((?word
                 (sem-cat (sem-class modality)
                          (sem-function event))
                (args (?super-event ?event)))
                <-
                (?word
                 (HASH meaning ((modality ?super-event ?event will)))
                 --
                 (syn-cat (lex-class modal-aux)
                           (lemma "vouloir")
                           (verb-form ?verb-form)
                           (perfect-aux "avoir")
                           (tense ?tense)
                           (person ?person)
                           (number ?number))))
               :cxn-set lex)

;;; MORPHOLOGICAL CONSTRUCTIONS ;;;
(def-fcg-cxn atterrir-morph 
             (<-
              (?word
               (syn-cat (lex-class verb)
                        (lemma "atterrir")
                        (verb-form (infinitive))
                        (perfect-aux ?perfect-aux)
                        (tense (past -)
                               (present +)
                               (futur -))
                        (person ?person)
                        (number ?number))
               --
               (HASH form ((string ?word "atterrir")))))
             :cxn-set morph)

;; participe passe
(def-fcg-cxn atterri-morph 
             (<-
              (?word
               (syn-cat (lex-class verb)
                        (lemma "atterrir")
                        (verb-form (participle))
                        (perfect-aux ?perfect-aux)
                        (tense (past +)
                               (present -)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "atterri")))))
             :cxn-set morph)

;; imparfait
(def-fcg-cxn atterrissais-morph 
             (<-
              (?word
               (syn-cat (lex-class verb)
                        (lemma "atterrir")
                        (verb-form (indicative))
                        (perfect-aux ?perfect-aux)
                        (tense (past +)
                               (present -)
                               (futur -))
                        (person 1) ; TODO
                        (number sg))
               --
               (HASH form ((string ?word "atterrissais")))))
             :cxn-set morph)

;; present
(def-fcg-cxn atterris-morph 
             (<-
              (?word
               (syn-cat (lex-class verb)
                        (lemma "atterrir")
                        (verb-form (indicative imperative))
                        (perfect-aux ?perfect-aux)
                        (tense (past -)
                               (present +)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "atterris")))))
             :cxn-set morph)

;; futur simple
(def-fcg-cxn atterrirai-morph 
             (<-
              (?word
               (syn-cat (lex-class verb)
                        (lemma "atterrir")
                        (verb-form (indicative))
                        (perfect-aux ?perfect-aux)
                        (tense (past -)
                               (present -)
                               (futur +))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "atterrirai")))))
             :cxn-set morph)

;; conditional
(def-fcg-cxn atterrirais-morph 
             (<-
              (?word
               (syn-cat (lex-class verb)
                        (lemma "atterrir")
                        (verb-form (conditional))
                        (perfect-aux ?perfect-aux)
                        (tense (past -)
                               (present +)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "atterrirais")))))
             :cxn-set morph)

;; subjunctive
(def-fcg-cxn atterrisse-morph 
             (<-
              (?word
               (syn-cat (lex-class verb)
                        (lemma "atterrir")
                        (verb-form (subjunctive))
                        (perfect-aux ?perfect-aux)
                        (tense (past +)
                               (present +)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "atterrisse")))))
             :cxn-set morph)

;; perfect auxiliaries

(def-fcg-cxn avais-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class perfect-aux)
                        (lemma "avoir")
                        (verb-form (indicative perfect))
                        (tense (past +)
                               (present -)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "avais")))))
             :cxn-set morph)

(def-fcg-cxn ai-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class perfect-aux)
                        (lemma "avoir")
                        (verb-form (indicative perfect))
                        (tense (past -)
                               (present +)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "ai")))))
             :cxn-set morph)

(def-fcg-cxn aurai-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class perfect-aux)
                        (lemma "avoir")
                        (verb-form (indicative perfect))
                        (tense (past -)
                               (present -)
                               (futur +))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "aurai")))))
             :cxn-set morph)

(def-fcg-cxn aurais-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class perfect-aux)
                        (lemma "avoir")
                        (verb-form (conditional perfect))
                        (tense (past -)
                               (present +)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "aurais")))))
             :cxn-set morph)

(def-fcg-cxn aie-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class perfect-aux)
                        (lemma "avoir")
                        (verb-form (subjunctive imperative perfect))
                        (tense (past -)
                               (present +)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "aie")))))
             :cxn-set morph)


(def-fcg-cxn eusse-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class perfect-aux)
                        (lemma "avoir")
                        (verb-form (subjunctive perfect))
                        (tense (past +)
                               (present -)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "eusse")))))
             :cxn-set morph)

;; modal auxiliaries
(def-fcg-cxn devais-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "devoir")
                        (verb-form (indicative))
                        (tense (past +)
                               (present -)
                               (futur -))
                        (perfect-aux ?perfect-aux)
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "devais")))))
             :cxn-set morph)

(def-fcg-cxn dois-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "devoir")
                        (verb-form (indicative))
                        (tense (past -)
                               (present +)
                               (futur -))
                        (perfect-aux ?perfect-aux)
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "dois")))))
             :cxn-set morph)

(def-fcg-cxn devrai-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "devoir")
                        (verb-form (indicative))
                        (tense (past -)
                               (present -)
                               (futur +))
                        (perfect-aux ?perfect-aux)
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "devrai")))))
             :cxn-set morph)

(def-fcg-cxn du-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "devoir")
                        (verb-form (participle))
                        (perfect-aux ?perfect-aux)
                        (tense (past +)
                               (present -)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "dû")))))
             :cxn-set morph)

(def-fcg-cxn devrais-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "devoir")
                        (verb-form (conditional))
                        (perfect-aux ?perfect-aux)
                        (tense (past -)
                               (present +)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "devrais")))))
             :cxn-set morph)

(def-fcg-cxn doive-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "devoir")
                        (verb-form (subjunctive))
                        (perfect-aux ?perfect-aux)
                        (tense (past -)
                               (present +)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "doive")))))
             :cxn-set morph)

(def-fcg-cxn dusse-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "devoir")
                        (verb-form (subjunctive))
                        (perfect-aux ?perfect-aux)
                        (tense (past +)
                               (present -)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "dusse")))))
             :cxn-set morph)

(def-fcg-cxn pouvais-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "pouvoir")
                        (verb-form (indicative))
                        (tense (past +)
                               (present -)
                               (futur -))
                        (perfect-aux ?perfect-aux)
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "pouvais")))))
             :cxn-set morph)

(def-fcg-cxn peux-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "pouvoir")
                        (verb-form (indicative))
                        (tense (past -)
                               (present +)
                               (futur -))
                        (perfect-aux ?perfect-aux)
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "peux")))))
             :cxn-set morph)

(def-fcg-cxn pourrai-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "pouvoir")
                        (verb-form (indicative))
                        (tense (past -)
                               (present -)
                               (futur +))
                        (perfect-aux ?perfect-aux)
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "pourrai")))))
             :cxn-set morph)

(def-fcg-cxn pu-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "pouvoir")
                        (verb-form (participle))
                        (perfect-aux ?perfect-aux)
                        (tense (past +)
                               (present -)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "pu")))))
             :cxn-set morph)

(def-fcg-cxn voulais-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "vouloir")
                        (verb-form (indicative))
                        (tense (past +)
                               (present -)
                               (futur -))
                        (perfect-aux ?perfect-aux)
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "voulais")))))
             :cxn-set morph)

(def-fcg-cxn veux-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "vouloir")
                        (verb-form (indicative))
                        (tense (past -)
                               (present +)
                               (futur -))
                        (perfect-aux ?perfect-aux)
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "veux")))))
             :cxn-set morph)

(def-fcg-cxn voudrai-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "vouloir")
                        (verb-form (indicative))
                        (tense (past -)
                               (present -)
                               (futur +))
                        (perfect-aux ?perfect-aux)
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "voudrai")))))
             :cxn-set morph)

(def-fcg-cxn voulu-aux-morph 
             (<-
              (?word
               (syn-cat (lex-class modal-aux)
                        (lemma "vouloir")
                        (verb-form (participle))
                        (perfect-aux ?perfect-aux)
                        (tense (past +)
                               (present -)
                               (futur -))
                        (person 1)
                        (number sg))
               --
               (HASH form ((string ?word "voulu")))))
             :cxn-set morph)

;;; VP CONSTRUCTION ;;;
(def-fcg-cxn vp-cxn 
               ((?vp-unit
                 (sem-cat (sem-function predicating-expression)
                          (origo ?origo))
                 (syn-cat (phrase-type vp)
                          (perfect-aux ?perfect-aux)
                          (tense ?tense)
                          (person ?person)
                          (number ?number)
                          (verb-form ?verb-form)
                          (left-most-subunit ?main-verb-unit))
                 (args (?ev ?origo))
                 (subunits (?main-verb-unit)))
                <-
                (?main-verb-unit
                 (sem-cat (sem-class action))
                 (args (?ev))
                 --
                 (syn-cat (lex-class verb)
                           (perfect-aux ?perfect-aux)
                           (verb-form ?verb-form)
                           (tense ?tense)
                           (person ?person)
                           (number ?number))))
               :cxn-set vp)

;;; AUXILIARY CONSTRUCTIONS ;;;

;; Aspect
(def-fcg-cxn perfect-cxn 
             ((?new-vp-unit
               (sem-cat (sem-function predicating-expression)
                        (perfect (?super-event ?event)))
               (syn-cat (phrase-type vp)
                        (perfect-aux ?perfect-aux)
                        (tense ?aux-tense)
                        (person ?person)
                        (number ?number)
                        (verb-form (indicative ?indicative)
                                   (infinitive ?infinitive)
                                   (conditional ?conditional)
                                   (subjunctive ?subjunctive)
                                   (imperative ?imperative)
                                   (participle -)
                                   (perfect +))
                        (left-most-subunit ?aux-unit))
               (args (?super-event ?origo))
               (subunits (?aux-unit ?vp-unit)))
              (?vp-unit
               (sem-cat
                (part-of-phrase +)))
              <-
              (?aux-unit
               (sem-cat (sem-class perfect-event)
                        (NOT (part-of-phrase +)))
               (args (?super-event ?event))
               --
               (syn-cat (lex-class perfect-aux)
                        (lemma ?perfect-aux)
                        (tense ?aux-tense)
                        (person ?person)
                        (number ?number)
                        (verb-form (indicative ?indicative)
                                   (infinitive ?infinitive)
                                   (conditional ?conditional)
                                   (subjunctive ?subjunctive)
                                   (imperative ?imperative)
                                   (participle -)
                                   (perfect ?perfect))))
              (?vp-unit
               (sem-cat (sem-function predicating-expression)
                        (NOT (part-of-phrase +)))
               (args (?event ?origo))
               --
               (syn-cat (phrase-type vp)
                        (perfect-aux ?perfect-aux)
                        (verb-form (indicative -)
                                   (infinitive -)
                                   (conditional -)
                                   (subjunctive -)
                                   (imperative -)
                                   (participle +)
                                   (perfect -))
                        (tense ?tense)
                        (left-most-subunit ?left-most-subunit-of-vp))
               (HASH form ((meets ?aux-unit ?left-most-subunit-of-vp)))))
             :cxn-set modality
             :feature-types ((verb-form set-of-features)))

;; Modality
(def-fcg-cxn modal-aux-cxn 
             ((?new-vp-unit
               (sem-cat (sem-function predicating-expression))
               (syn-cat (phrase-type vp)
                        (perfect-aux ?perfect-aux)
                        (tense ?tense)
                        (person ?person)
                        (number ?number)
                        (verb-form ?verb-form)
                        (left-most-subunit ?aux-unit))
               (args (?super-event ?origo))
              (subunits (?aux-unit ?vp-unit)))
              (?vp-unit
               (sem-cat (part-of-phrase +)))
              <-
              (?aux-unit
               (sem-cat (sem-class modality)
                        (sem-function event))
               (args (?super-event ?event))
               --
               (syn-cat (lex-class modal-aux)
                        (tense ?tense)
                        (person ?person)
                        (number ?number)
                        (verb-form ?verb-form)))
              (?vp-unit
               (sem-cat (sem-function predicating-expression)
                        (NOT (part-of-phrase +)))
               (args (?event ?origo))
               --
               (syn-cat  (phrase-type vp)
                         (perfect-aux ?perfect-aux)
                         (verb-form (infinitive))
                         (left-most-subunit ?left-most-subunit-of-vp))
               (HASH form ((meets ?aux-unit ?left-most-subunit-of-vp)))))
             :cxn-set modality)
  
(def-fcg-cxn conditional-cxn 
               ((?main-verb-unit
                  (sem-cat (mood-phrase +)))
                <-
                (?main-verb-unit
                 (HASH meaning ((modality ?event hypothesis)))
                 (sem-cat (sem-function predicating-expression)
                          (NOT (mood-phrase +)))
                 (args (?event ?origo))
                 --
                 (syn-cat (phrase-type vp)
                           (perfect-aux ?perfect-aux)
                           (verb-form (indicative ?indicative)
                                      (infinitive ?infinitive)
                                      (conditional +)
                                      (subjunctive ?subjunctive)
                                      (imperative ?imperative)
                                      (participle ?participle)
                                      (perfect ?perfect))
                           (tense ?tense)
                           (person ?person)
                           (number ?number))))
               :cxn-set modality
               :feature-types ((verb-form set-of-features)))

(def-fcg-cxn indicative-cxn 
               ((?main-verb-unit
                  (sem-cat (mood-phrase +)))
                <-
                (?main-verb-unit
                 (HASH meaning ((modality ?event fact)))
                 (sem-cat (sem-function predicating-expression)
                          (NOT (mood-phrase +)))
                 (args (?event ?origo))
                 --
                 (syn-cat (phrase-type vp)
                           (perfect-aux ?perfect-aux)
                           (verb-form (indicative +)
                                      (infinitive ?infinitive)
                                      (conditional ?conditional)
                                      (subjunctive ?subjunctive)
                                      (imperative ?imperative)
                                      (participle ?participle)
                                      (perfect ?perfect))
                           (tense ?tense)
                           (person ?person)
                           (number ?number))))
               :cxn-set modality
               :feature-types ((verb-form set-of-features)))

(def-fcg-cxn subjunctive-cxn 
               ((?main-verb-unit
                  (sem-cat (mood-phrase +)))
                <-
                (?main-verb-unit
                 (HASH meaning ((modality ?event uncertainty)))
                 (sem-cat (sem-function predicating-expression)
                          (NOT (mood-phrase +)))
                 (args (?event ?origo))
                 --
                 (syn-cat (phrase-type vp)
                           (perfect-aux ?perfect-aux)
                           (verb-form (indicative ?indicative)
                                      (infinitive ?infinitive)
                                      (conditional ?conditional)
                                      (subjunctive +)
                                      (imperative ?imperative)
                                      (participle ?participle)
                                      (perfect ?perfect))
                           (tense ?tense)
                           (person ?person)
                           (number ?number))))
               :cxn-set modality
               :feature-types ((verb-form set-of-features)))


(def-fcg-cxn imperative-cxn 
               ((?main-verb-unit
                  (sem-cat (mood-phrase +)))
                <-
                (?main-verb-unit
                 (HASH meaning ((modality ?event will)))
                 (sem-cat (sem-function predicating-expression)
                          (NOT (mood-phrase +)))
                 (args (?event ?origo))
                 --
                 (syn-cat (phrase-type vp)
                           (perfect-aux ?perfect-aux)
                           (verb-form (indicative ?indicative)
                                      (infinitive ?infinitive)
                                      (conditional ?conditional)
                                      (subjunctive ?subjunctive)
                                      (imperative +)
                                      (participle ?participle)
                                      (perfect ?perfect))
                           (tense ?tense)
                           (person ?person)
                           (number ?number))))
               :cxn-set modality
               :feature-types ((verb-form set-of-features)))

;;; TENSE CONSTRUCTIONS ;;;

(def-fcg-cxn past-cxn 
             ((?vp
                (sem-cat (tense-phrase +)))
              <-
              (?vp
               (sem-cat (sem-function predicating-expression)
                        (NOT (tense-phrase +))
                        (NOT (part-of-phrase +)))
               (args (?ev ?origo))
               (HASH meaning ((deictic-time-point ?origo present)
                              (before ?ev ?origo)))
               --
               (syn-cat (phrase-type vp)
                        (verb-form ?verb-form)
                        (tense (past +)
                               (present ?present)
                               (futur ?futur)))))
             :cxn-set tense)

(def-fcg-cxn present-cxn 
             ((?vp
                (sem-cat (tense-phrase +)))
              <-
              (?vp
               (sem-cat (sem-function predicating-expression)
                        (NOT (tense-phrase +))
                        (NOT (part-of-phrase +)))
               (args (?ev ?origo))
               (HASH meaning ((deictic-time-point ?origo present) 
                              (overlaps ?ev ?origo)))
               --
               (syn-cat (phrase-type vp)
                        (verb-form ?verb-form)
                        (tense (past ?past)
                               (present +)
                               (futur ?futur)))))
             :cxn-set tense)

(def-fcg-cxn futur-cxn 
             ((?vp
                (sem-cat (tense-phrase +)))
              <-
              (?vp
               (sem-cat (sem-function predicating-expression)
                        (NOT (tense-phrase +))
                        (NOT (part-of-phrase +)))
               (args (?ev ?origo))
               (HASH meaning ((deictic-time-point ?origo present) 
                              (after ?ev ?origo)))
               --
               (syn-cat (phrase-type vp)
                        (verb-form ?verb-form)
                        (tense (past ?past)
                               (present ?present)
                               (futur +)))))
             :cxn-set tense)

;;; COMPREHEND
;; plus-que-parfait (indicative past perfect)
(comprehend-and-formulate '("avais" "atterri"))

;; passe compose (indicative present perfect)
(comprehend-and-formulate '("ai" "atterri"))

;; imparfait (indicative past imperfect)
(comprehend-and-formulate '("atterrissais"))

;; present (indicative present) et imperatif present (imperative present)
(comprehend-and-formulate '("atterris"))

;; futur anterieur (indicative future perfect)
(comprehend-and-formulate '("aurai" "atterri"))

;; futur simple (indicative future imperfect)
(comprehend-and-formulate '("atterrirai"))

;; conditionnel present (conditional present)
(comprehend-and-formulate '("atterrirais"))

;; conditionnel passe (conditional present perfect)
(comprehend-and-formulate '("aurais" "atterri"))

;; subjonctif present and imparfait (subjunctive present and past)
(comprehend-and-formulate '("atterrisse"))

;; subjonctif passe (subjunctive present perfect) et imperatif passe (imperative past)
(comprehend-and-formulate '("aie" "atterri"))

;; subjonctif plus que parfait (subjunctive past perfect)
(comprehend-and-formulate '("eusse" "atterri"))

;; obligation (with auxiliary verb "devoir")
;; in order : past, present, futur, conditional present, 
;; subjunctive present, subjunctive past, subjunctive present perfect, subjunctive past perfect
(comprehend-and-formulate '("devais" "atterrir"))
(comprehend-and-formulate '("dois" "atterrir"))
(comprehend-and-formulate '("devrai" "atterrir"))
(comprehend-and-formulate '("devrais" "atterrir"))
(comprehend-and-formulate '("doive" "atterrir"))
(comprehend-and-formulate '("dusse" "atterrir"))
(comprehend-and-formulate '("ai" "dû" "atterrir"))
(comprehend-and-formulate '("eusse" "dû" "atterrir"))

;; ability (with auxiliary verb "pouvoir")
;; in order : past, present, future, subjunctive present perfect
(comprehend-and-formulate '("pouvais" "atterrir"))
(comprehend-and-formulate '("peux" "atterrir"))
(comprehend-and-formulate '("pourrai" "atterrir"))
(comprehend-and-formulate '("ai" "pu" "atterrir"))

;; will (with auxiliary verb "vouloir")
;; in order : past, present, future, subjunctive present perfect
(comprehend-and-formulate '("voulais" "atterrir"))
(comprehend-and-formulate '("veux" "atterrir"))
(comprehend-and-formulate '("voudrai" "atterrir"))
(comprehend-and-formulate '("ai" "voulu" "atterrir"))