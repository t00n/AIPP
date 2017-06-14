(asdf:operate 'asdf:load-op :irl)

(in-package :irl)

(activate-monitor trace-irl-in-web-browser)

;; #### Objects ####
;; taken from irl-tutorial and added the color attribute
(defclass object (entity)
  ((shape :type symbol :initarg :shape :accessor shape)
   (size :type float :initarg :size :accessor size)
   (color :type symbol :initarg :color :accessor color)))

;; taken from irl-tutorial
(defclass object-set (entity)
  ((objects :type list :initarg :objects :accessor objects)))

;; taken from irl-tutorial
(defmethod equal-entity ((set1 object-set) (set2 object-set))
  (permutation-of? (objects set1) (objects set2) :test #'equal-entity))

;; taken from irl-tutorial and added the color in CSS
(defmethod make-html-for-entity-details ((object object) &key)
  `(((div :class "entity-detail" :style "text-align:center")
     ((span :style ,(format nil "font-size:~,2fpx;color:~a;" (+ 20 (* 30 (size object))) (color object)))
      ,(ecase (shape object) (rectangle "&#x25ad;") (triangle "&#x25b3;") (circle "&#x25ef;"))))
    ((div :class "entity-detail") ,(format nil "size: ~,2f" (size object)))))

;; taken from irl-tutorial
(defmethod make-html-for-entity-details ((set object-set) &key)
  `(((div :class "entity-detail") 
     ,@(loop for object in (objects set)
             collect (make-html object :expand-initially t)))))

;; Shape object : taken from irl tutorial
;; Create a shape-category object
(defclass shape-category (entity)
  ((shape :type symbol :initarg :shape :reader shape)))

;; Drawing the category
(defmethod make-html-for-entity-details ((category shape-category) &key)
  `(((div :class "entity-detail" :style "text-align:center")
     ((span :style "font-size:50px;")
      ,(ecase (shape category) (rectangle "&#x25ad;")
         (triangle "&#x25b3;") (circle "&#x25ef;"))))))


;; #### Colors ####
;; create color category
(defclass color-category (entity)
  ((color :type symbol :initarg :color :reader color)))

(defmethod make-html-for-entity-details ((category color-category) &key)
  `(((div :class "entity-detail" :style "text-align:center")
     ((span :style, (format nil "font-size:20px;color:~a;" (color category)))
      ,(color category)))))

;; #### Ontology ####
;; taken from irl-tutorial and added colors to the context objects
(defparameter *context* 
  (make-instance 
   'object-set
   :id 'my-context
   :objects (list (make-instance 'object :size 0.8 :shape 'circle :color 'red)
                  (make-instance 'object :size 0.6 :shape 'rectangle :color 'green)
                  (make-instance 'object :size 0.4 :shape 'circle :color 'blue)
                  (make-instance 'object :size 0.2 :shape 'triangle :color 'red)
                  (make-instance 'object :size 0.9 :shape 'rectangle :color 'green))))

;; define colors
(defparameter *colors*
  (list
   (make-instance 'color-category :id 'red :color 'red)
   (make-instance 'color-category :id 'green :color 'green)
   (make-instance 'color-category :id 'blue :color 'blue)
   (make-instance 'color-category :id 'yellow :color 'yellow)))

;; taken from irl tutorial
;; A list of all the categories
(defparameter *shapes*
  (list
   (make-instance 'shape-category :id 'rectangle :shape 'rectangle)
   (make-instance 'shape-category :id 'circle :shape 'circle)
   (make-instance 'shape-category :id 'triangle :shape 'triangle)))

;; create ontology with context, colors and shapes
(progn
  (defparameter *ontology* (make-blackboard))
  (set-data *ontology* 'colors *colors*)
  (set-data *ontology* 'shapes *shapes*)
  (set-data *ontology* 'context *context*))


;; taken from irl-tutorial
(defprimitive get-context ((context object-set))
  ((context =>)
   (equal-entity (get-data ontology 'context) context))
  ((=> context)
   (bind (context 1.0 (get-data ontology 'context)))))


;; get color-category from ontology
(defprimitive get-color-category ((color-category color-category))
  ((color-category =>)
   (equal-entity (get-data ontology 'color-category) color-category))
  ((=> color-category)
   (bind (color-category 1.0 (get-data ontology 'color-category)))))

;; #### Primitives ####
;; filters all objects of a given color-category in object-set
(defun filter-by-color (object-set color-category)
  (let ((filtered-objects (loop for object in (objects object-set)
                                if (eq (color object) (color color-category))
                                collect object)))
    (when filtered-objects
      (make-instance 'object-set :objects filtered-objects))))

;; this primitive can filter a source-set by color or retrieve a color from a source-set and a filtered-set
(defprimitive filter-by-color ((filtered-set object-set) (source-set object-set)
                               (color-category color-category))
              
  ((source-set color-category => filtered-set)
   (let ((computed-set (filter-by-color source-set color-category)))
     (when computed-set
       (bind (filtered-set 1.0 computed-set)))))
  
  ((source-set filtered-set => color-category)
   (let ((computed-category
          (find-if #'(lambda (color) (mapcar #'(lambda (x y) (equal-entity x y))
                                             (objects filtered-set)
                                             (objects (filter-by-color source-set color))))
                   (get-data ontology 'colors))))
     (when computed-category
       (bind (color-category 1.0 computed-category))))))

;; Shape : filter-by-shape taken from irl-tutorial
(defun filter-by-shape (object-set shape-category)
  (let ((filtered-objects (loop for object in (objects object-set)
                                if (eq (shape object) (shape shape-category))
                                collect object)))
    (when filtered-objects
      (make-instance 'object-set :objects filtered-objects))))

(defprimitive filter-by-shape ((filtered-set object-set) (source-set object-set)
                               (shape-category shape-category))
  ;; first case: if given source-set and shape, compute filtered-set
  ((source-set shape-category => filtered-set)
   (let ((computed-set (filter-by-shape source-set shape-category)))
     (when computed-set
       (bind (filtered-set 1.0 computed-set)))))
  
  ;; second case: if given source-set and filtered-set, compute shape-category
  ((source-set filtered-set => shape-category)
   (let ((computed-category
          (find-if #'(lambda (shape) (mapcar #'(lambda (x y) (equal-entity x y))
                                             (objects filtered-set)
                                             (objects (filter-by-shape source-set shape))))
                   (get-data ontology 'shapes))))
     (when computed-category
       (bind (shape-category 1.0 computed-category)))))

  ;; previous case: if given source-set, compute pairs of filtered-set and shape
  ((source-set => filtered-set shape-category)
   (loop for shape in *shapes*
         for computed-set = (filter-by-shape source-set shape)
         if computed-set
         do (bind (shape-category 1.0 shape)
                  (filtered-set 1.0 computed-set))))

  ;; final case: if given source-set, filtered-set and shape, check for consitency
  ((source-set filtered-set shape-category =>)
   (mapcar #'(lambda (x y) (equal-entity x y))
           (objects filtered-set)
           (objects (filter-by-shape source-set shape-category)))))

;; #### Chunk ####
(defparameter *chunk-color-1*
  (make-instance 
   'chunk
   :id 'filter-by-color+source-set+color
   :irl-program `((filter-by-color ?target-set ?source-set ?color)
                  (get-color-category ?color)
                  (get-context ?source-set))
   :open-vars '((?color . color-category)
                (?source-set . object-set))
   :target-var '(?target-set . object-set)))


;; #### Web page ####
;; here we display stuff on the web page

;; show colors
(progn
  (clear-page)
  (loop for color in *colors*
        do (add-element (make-html color :expand-initially t))))

;; show context
(add-element (make-html *context* :expand-initially t))

;; tests source-set color => filtered-set
(evaluate-irl-program 
 `((filter-by-color ?filtered-set ?source-set ?color)
   (bind color-category ?color ,(first *colors*))
   (bind object-set ?source-set ,*context*))
 *ontology*)

;; tests source-set filtered-set => color
(evaluate-irl-program
 `((filter-by-color ?filtered-set ?source-set ?color)
   (bind object-set ?source-set ,*context*)
   (bind object-set ?filtered-set 
         ,(make-instance 'object-set :objects (list (first (objects *context*))))))
 *ontology*)

;; tests with non existant color (yellow)
(evaluate-irl-program 
 `((filter-by-color ?filtered-set ?source-set ?color)
   (bind color-category ?color ,(fourth *colors*))
   (bind object-set ?source-set ,*context*))
 *ontology*)

;; tests filter by color and shape
(evaluate-irl-program
 `((filter-by-color ?set-2 ?source-set ?color)
   (filter-by-shape ?filtered-set ?set-2 ?shape)
   (bind object-set ?source-set ,*context*)
   (bind color-category ?color ,(first *colors*))
   (bind shape-category ?shape ,(second *shapes*)))
 *ontology*)

;; retrieve color and shape from red circle
(evaluate-irl-program
 `((filter-by-color ?set-2 ?source-set ?color)
   (filter-by-shape ?filtered-set ?set-2 ?shape)
   (bind object-set ?source-set ,*context*)
   (bind object-set ?set-2
         ,(make-instance 'object-set :objects (list (first (objects *context*))))))
 *ontology*)

;; tests with non existant combination of color and shape (red rectangles)
(evaluate-irl-program
 `((filter-by-color ?set-2 ?source-set ?color)
   (filter-by-shape ?filtered-set ?set-2 ?shape)
   (bind object-set ?source-set ,*context*)
   (bind color-category ?color ,(first *colors*))
   (bind shape-category ?shape ,(first *shapes*)))
 *ontology*)

;; shows use of chunk-color-1
(progn
  (add-element (make-html *chunk-color-1* :expand-initially t))
  (defparameter *ont2* (make-blackboard))
  (set-data *ont2* 'color-category (first *colors*))
  (set-data *ont2* 'context *context*)
  (add-element (make-html *ont2* :expand-initially t))
  (evaluate-chunk *chunk-color-1* *ont2*))