(in-package #:html-protocol)

(defparameter *void-elements*
  '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta"
    "param" "source" "track" "wbr"))

(defparameter *opaque-text-elements*
  '("script" "style"))

(defclass html-node () ())

(defclass html-document (html-node)
  ((children :initarg :children :accessor html-document-children :initform nil)))

(defun html-document-p (object)
  (typep object 'html-document))

(defclass html-element (html-node)
  ((name :initarg :name :accessor html-element-name :initform "")
   (attributes :initarg :attributes :accessor html-element-attributes :initform nil)
   (children :initarg :children :accessor html-element-children :initform nil)))

(defun html-element-p (object)
  (typep object 'html-element))

(defclass html-text (html-node)
  ((data :initarg :data :accessor html-text-data :initform "")))

(defun html-text-p (object)
  (typep object 'html-text))

(defclass html-comment (html-node)
  ((data :initarg :data :accessor html-comment-data :initform "")))

(defun html-comment-p (object)
  (typep object 'html-comment))

(defclass html-doctype (html-node)
  ((name :initarg :name :accessor html-doctype-name :initform "html")))

(defun html-doctype-p (object)
  (typep object 'html-doctype))

(defun make-html-document (&key children)
  (make-instance 'html-document :children children))

;;; Name case. HTML names are case-insensitive and stored lowercase. SVG
;;; (and MathML's one) camelCase names are not: an HTML parser restores their
;;; case in foreign content (WHATWG HTML 13.2.6.5, "adjust SVG attributes",
;;; the SVG tag-name table, "adjust MathML attributes"), but XML readers
;;; (XHTML, an <svg> copied out to a .svg file) do not. So html-protocol
;;; keeps the spec's camelCase spelling for exactly the names in those tables,
;;; whatever case they arrive in, and lowercases every other name. None of
;;; the table names is an HTML element or attribute, so this needs no
;;; namespace context.
;;;
;;; CANONICAL-ELEMENT-NAME name     -> the SVG camelCase tag name, or NAME downcased.
;;; CANONICAL-ATTRIBUTE-NAME name   -> the SVG/MathML camelCase attribute name,
;;;                                    or NAME downcased.
;;; MAKE-HTML-ELEMENT, (SETF ELEMENT-ATTR) for a new attribute, and the plump
;;; backend's parse go through them.

(defparameter *svg-camel-case-elements*
  '("altGlyph" "altGlyphDef" "altGlyphItem" "animateColor" "animateMotion"
    "animateTransform" "clipPath" "feBlend" "feColorMatrix" "feComponentTransfer"
    "feComposite" "feConvolveMatrix" "feDiffuseLighting" "feDisplacementMap"
    "feDistantLight" "feDropShadow" "feFlood" "feFuncA" "feFuncB" "feFuncG"
    "feFuncR" "feGaussianBlur" "feImage" "feMerge" "feMergeNode" "feMorphology"
    "feOffset" "fePointLight" "feSpecularLighting" "feSpotLight" "feTile"
    "feTurbulence" "foreignObject" "glyphRef" "linearGradient" "radialGradient"
    "textPath")
  "SVG element names with uppercase letters (WHATWG HTML SVG tag-name table).")

(defparameter *svg-camel-case-attributes*
  '("attributeName" "attributeType" "baseFrequency" "baseProfile" "calcMode"
    "clipPathUnits" "diffuseConstant" "edgeMode" "filterUnits" "glyphRef"
    "gradientTransform" "gradientUnits" "kernelMatrix" "kernelUnitLength"
    "keyPoints" "keySplines" "keyTimes" "lengthAdjust" "limitingConeAngle"
    "markerHeight" "markerUnits" "markerWidth" "maskContentUnits" "maskUnits"
    "numOctaves" "pathLength" "patternContentUnits" "patternTransform"
    "patternUnits" "pointsAtX" "pointsAtY" "pointsAtZ" "preserveAlpha"
    "preserveAspectRatio" "primitiveUnits" "refX" "refY" "repeatCount"
    "repeatDur" "requiredExtensions" "requiredFeatures" "specularConstant"
    "specularExponent" "spreadMethod" "startOffset" "stdDeviation" "stitchTiles"
    "surfaceScale" "systemLanguage" "tableValues" "targetX" "targetY"
    "textLength" "viewBox" "viewTarget" "xChannelSelector" "yChannelSelector"
    "zoomAndPan"
    ;; MathML ("adjust MathML attributes")
    "definitionURL")
  "Attribute names with uppercase letters (WHATWG HTML adjust SVG / MathML attributes).")

(defun %canonical-name (name table)
  (let ((name (string name)))
    (or (find name table :test #'string-equal)
        (string-downcase name))))

(defun canonical-element-name (name)
  "NAME (a string designator) as html-protocol stores it: the SVG camelCase
spelling for a name in *SVG-CAMEL-CASE-ELEMENTS*, else lowercased."
  (%canonical-name name *svg-camel-case-elements*))

(defun canonical-attribute-name (name)
  "NAME (a string designator) as html-protocol stores it: the camelCase
spelling for a name in *SVG-CAMEL-CASE-ATTRIBUTES*, else lowercased."
  (%canonical-name name *svg-camel-case-attributes*))

(defun make-html-element (name &key attributes children)
  "An element named (CANONICAL-ELEMENT-NAME NAME). ATTRIBUTES are kept as given."
  (make-instance 'html-element
                 :name (canonical-element-name name)
                 :attributes attributes
                 :children children))

(defun void-element-p (name)
  (and (stringp name)
       (find name *void-elements* :test #'string-equal)))

(defun html-element-void-p (element)
  (void-element-p (html-element-name element)))

(defun opaque-text-element-p (name-or-element)
  (let ((name (if (html-element-p name-or-element)
                  (html-element-name name-or-element)
                  name-or-element)))
    (and (stringp name)
         (find name *opaque-text-elements* :test #'string-equal))))

(defun document-root (document)
  "First html-element child of DOCUMENT (usually html)."
  (find-if #'html-element-p (html-document-children document)))

(defun element-attr (element name)
  (cdr (assoc name (html-element-attributes element) :test #'string-equal)))

(defun (setf element-attr) (value element name)
  (let* ((key (canonical-attribute-name name))
         (cell (assoc key (html-element-attributes element) :test #'string-equal)))
    ;; Non-destructive: the alist may be shared with the caller (or a literal).
    ;; New attributes go at the end, so serialization keeps insertion order.
    (setf (html-element-attributes element)
          (if cell
              (mapcar (lambda (pair)
                        (if (eq pair cell) (cons (car pair) value) pair))
                      (html-element-attributes element))
              (append (html-element-attributes element)
                      (list (cons key value)))))
    value))

(defun element-text (node)
  "Concatenated descendant text. script/style stay opaque (their text children only)."
  (labels ((walk (n acc)
             (cond
               ((html-text-p n)
                (cons (html-text-data n) acc))
               ((html-element-p n)
                (if (opaque-text-element-p n)
                    (append (reverse (mapcar #'html-text-data
                                             (remove-if-not #'html-text-p
                                                            (html-element-children n))))
                            acc)
                    (reduce #'walk (html-element-children n)
                            :initial-value acc :from-end t)))
               ((html-document-p n)
                (reduce #'walk (html-document-children n)
                        :initial-value acc :from-end t))
               (t acc))))
    (apply #'concatenate 'string (nreverse (walk node nil)))))
