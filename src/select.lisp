(in-package #:html-protocol)

;;; Tiny CSS subset: tag, #id, .class, descendant, child (`>`). No combinators beyond that.
;;; CSS/JS engines are out of scope — selectors match the CLOS tree only.

(defstruct simple-sel
  (tag nil)
  (id nil)
  (classes nil))

(defun %skip-ws (s i)
  (loop while (and (< i (length s)) (member (char s i) '(#\Space #\Tab #\Newline #\Return)))
        do (incf i))
  i)

(defun %read-ident (s i)
  (let ((start i)
        (n (length s)))
    (loop while (and (< i n)
                     (let ((c (char s i)))
                       (or (alphanumericp c) (find c "-_"))))
          do (incf i))
    (when (= i start)
      (error 'html-error :message (format nil "expected identifier at ~A in ~S" start s)))
    (values (string-downcase (subseq s start i)) i)))

(defun %parse-simple (s i)
  (let ((n (length s))
        (sel (make-simple-sel)))
    (cond
      ((and (< i n) (char= (char s i) #\*))
       (incf i))
      ((and (< i n) (not (find (char s i) '(#\# #\. #\> #\Space #\Tab))))
       (multiple-value-bind (tag j) (%read-ident s i)
         (setf (simple-sel-tag sel) tag
               i j))))
    (loop while (< i n)
          for c = (char s i)
          do (case c
               (#\#
                (multiple-value-bind (id j) (%read-ident s (1+ i))
                  (setf (simple-sel-id sel) id
                        i j)))
               (#\.
                (multiple-value-bind (cls j) (%read-ident s (1+ i))
                  (push cls (simple-sel-classes sel))
                  (setf i j)))
               (t (return))))
    (setf (simple-sel-classes sel) (nreverse (simple-sel-classes sel)))
    (values sel i)))

(defun parse-selector (string)
  "→ list of (simple-sel . combinator) where combinator is :descendant or :child."
  (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) string))
        (parts nil)
        (comb :descendant)
        (i 0))
    (when (zerop (length s))
      (error 'html-error :message "empty selector"))
    (loop while (< i (length s))
          do (setf i (%skip-ws s i))
             (when (>= i (length s)) (return))
             (when (char= (char s i) #\>)
               (setf comb :child)
               (incf i)
               (setf i (%skip-ws s i)))
             (multiple-value-bind (sel j) (%parse-simple s i)
               (push (cons sel comb) parts)
               (setf comb :descendant
                     i j)))
    (nreverse parts)))

(defun %class-list (element)
  (let ((raw (element-attr element "class")))
    (when (and raw (plusp (length raw)))
      (loop for start = 0 then (1+ end)
            for end = (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline)))
                                   raw :start start)
            for tok = (subseq raw start (or end (length raw)))
            unless (zerop (length tok))
              collect (string-downcase tok)
            while end))))

(defun %match-simple (element sel)
  (and (html-element-p element)
       (or (null (simple-sel-tag sel))
           (string-equal (simple-sel-tag sel) (html-element-name element)))
       (or (null (simple-sel-id sel))
           (string-equal (simple-sel-id sel) (or (element-attr element "id") "")))
       (every (lambda (cls)
                (find cls (%class-list element) :test #'string-equal))
              (simple-sel-classes sel))))

(defun %element-children (node)
  (cond
    ((html-document-p node) (remove-if-not #'html-element-p (html-document-children node)))
    ((html-element-p node) (remove-if-not #'html-element-p (html-element-children node)))
    (t nil)))

(defun %walk-descendants (node fn)
  (dolist (child (%element-children node))
    (funcall fn child)
    (%walk-descendants child fn)))

(defun select (node selector)
  "Return html-elements under NODE matching SELECTOR (string)."
  (let ((parts (parse-selector selector))
        (acc nil))
    (labels ((walk-sel (current remaining)
               (when remaining
                 (destructuring-bind (sel . comb) (car remaining)
                   (let ((rest (cdr remaining))
                         (cands (if (eq comb :child)
                                    (%element-children current)
                                    (let ((out nil))
                                      (%walk-descendants current (lambda (e) (push e out)))
                                      (nreverse out)))))
                     (dolist (el cands)
                       (when (%match-simple el sel)
                         (if rest
                             (walk-sel el rest)
                             (push el acc)))))))))
      (walk-sel node parts)
      (delete-duplicates (nreverse acc) :test #'eq))))
