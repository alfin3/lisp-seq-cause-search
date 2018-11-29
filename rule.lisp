;;;; rule.lisp
;;;;
;;;; functions and structures necessary to create and modify rules
;;;;
;;;; SBCL 1.3.1

(defstruct rule
  ; current rule hash table: position -> list of literals
  current-ht
  ; list of current to initial path-nodes
  path-lst 
  ; true satisfied instances (keys mapping to inst structs in data-ht)
  t-lst
  ; false satisfied instances (keys mapping to inst structs in data-ht)
  f-lst
  ; array of literal frequencies across true satisfied instances
  t-freq-arr
  ; array of literal frequences across false satisfied instances
  f-freq-arr)

(defstruct path-node 
  ; rule hash table: position -> list of literals
  rule-ht)

;;;; rule creation and modification functions

(defun create-start-rule (params)
  "creates the start rule satisfying all true instances"
  (let ((max-domain-ht (params-t-ht params)))
    (create-rule max-domain-ht () params)))

(defun create-rule (rule-ht prev-rule-path params)
  "creates a rule structure from a rule hash table"
  (multiple-value-bind (t-lst f-lst t-freq-arr f-freq-arr)
                       (update-rule-info rule-ht params)
                       (make-rule
                        :current-ht rule-ht
                        :path-lst (cons (make-path-node :rule-ht rule-ht)
					prev-rule-path)
                        :t-lst t-lst
                        :f-lst f-lst
                        :t-freq-arr t-freq-arr
                        :f-freq-arr f-freq-arr)))

(defun add-random-lit-rule (rule params)
  "adds a random literal to a random position and returns the resulting rule"
  (let* ((pos-lst (params-pos-lst params))
	 (max-domain-ht (params-t-ht params))
         (rule-ht (rule-current-ht rule))
         (pos (random-pos-add pos-lst rule-ht max-domain-ht)))
    (if (null pos) rule ; saturated
      (let ((lit (random-lit-add pos rule-ht max-domain-ht)))
        (if (null lit) rule
          (change-lit-rule rule pos lit #'cons params))))))

(defun change-lit-rule (rule pos lit cons-remove-fn params)
 "changes a literal at a position and returns the resulting rule
  cons-remove-fn: to delete use #'remove, to add use #'cons"
  (let* ((pos-lst (params-pos-lst params))
         (new-rule-ht (copy-ht (rule-current-ht rule)
			       pos-lst
			       :test #'equal))
         (cur-pos-domain (gethash pos new-rule-ht))
         (new-pos-domain (funcall cons-remove-fn lit cur-pos-domain))
         (prev-rule-path (copy-list (rule-path-lst rule))))
    (progn 
      (setf (gethash pos new-rule-ht) new-pos-domain)
      (create-rule new-rule-ht
		   prev-rule-path
		   params))))

(defun reduce-to-essential (rule params)
  "removes pos-lit pairs from a rule that do not satisfy a true instance"
  (let* ((rule-ht (rule-current-ht rule))
         (pos-lst (params-pos-lst params))
         (t-freq-arr (rule-t-freq-arr rule))
         (rule-pairs 
          (foldl (lambda (pos tail)
                   (nconc (make-pos-pairs pos rule-ht) tail))
                 ()
		 pos-lst))
         (relevant-pairs
          (filterl (lambda (pair)
		     (let* ((pos (get-pos pair))
			    (lit (get-lit pair))
			    (pos-ix (pos->ix pos params))
			    (lit-ix (lit->ix lit params)))
		       (not (= (aref t-freq-arr pos-ix lit-ix) 0))))
		   rule-pairs))
	 (new-ht (empty-ht pos-lst)))
    (progn
      (dolist (pair relevant-pairs)
        (let* ((pos (get-pos pair))
               (lit (get-lit pair))
               (prev-domain (gethash pos new-ht)))
          (setf (gethash pos new-ht) (cons lit prev-domain))))
      (create-rule new-ht (rule-path-lst rule) params))))

(defun get-pos (pair) (first pair))
(defun get-lit (pair) (second pair))

(defun pos->ix (pos params)
  (let ((pos-lst (params-pos-lst params)))
    (position pos pos-lst)))
(defun ix->pos (ix params)
  (let ((pos-lst (params-pos-lst params)))
    (nth ix pos-lst)))
(defun lit->ix (lit params)
  (let ((lit-lst (params-lit-lst params)))
    (position lit lit-lst)))
(defun ix->lit (ix params)
  (let ((lit-lst (params-lit-lst params)))
    (nth ix lit-lst)))

;;;; auxiliary rule creation functions

(defun update-rule-info (rule-ht params)
  "in one passage through data instances returns
   1) the list of true satisfied instances (keys from data-ht)
   2) the list of false satisfied instances (keys form data-ht)
   3) the freq array of counts of every literal in true satisfied instances
   4) the freq array of counts of every literal in false satisfied instances"
  (let* ((data-ht (params-data-ht params))
	 (pos-lst (params-pos-lst params))
	 (key-lst (params-key-lst params))
         (lit-lst (params-lit-lst params))
         (freq-t-arr (make-array (list (length pos-lst)
				       (length lit-lst))
				 :initial-element 0))
         (freq-f-arr (make-array (list (length pos-lst)
				       (length lit-lst))
				 :initial-element 0))
         (t-lst ())
         (f-lst ()))
    (dolist (key key-lst)
      (when (sat-inst-p key rule-ht params)  
        (let* ((sat-inst (gethash key data-ht))
	       (seq-lst (inst-seq-lst sat-inst))
	       (val (inst-value sat-inst)))
          (cond
            ((f-p val)
             (incf-array freq-f-arr
			 rule-ht 
                         seq-lst
			 params)
             (setf f-lst (cons key f-lst)))
            ((t-p val)
             (incf-array freq-t-arr
			 rule-ht 
                         seq-lst
			 params)
             (setf t-lst (cons key t-lst)))))))
    (values t-lst f-lst freq-t-arr freq-f-arr)))

(defun sat-inst-p (key rule-ht params)
  "checks if a rule-ht satisfies a data instance (key)"
  (let* ((data-ht (params-data-ht params))
	 (pos-lst (params-pos-lst params))
	 (seq-lst (inst-seq-lst (gethash key data-ht)))
	 (rule-pos-lst (non-empty-pos-lst pos-lst rule-ht)))
    (foldl (lambda (head tail)
	     (and head tail))
	   t
           (mapcar (lambda (pos) 
                     (member (nth (pos->ix pos params) seq-lst)
                             (gethash pos rule-ht)))
		   rule-pos-lst))))

(defun incf-array (freq-arr rule-ht seq-lst params)
  "updates the frequency array"
  (let* ((pos-lst (params-pos-lst params))
	 (non-empty-pos-lst (non-empty-pos-lst pos-lst rule-ht)))
    (dolist (pos non-empty-pos-lst)
      (let* ((pos-ix (pos->ix pos params))
	     (lit (nth pos-ix seq-lst))
	     (lit-ix (lit->ix lit params)))
	(when (member lit (gethash pos rule-ht))
	  (incf (aref freq-arr pos-ix lit-ix)))))))

(defun non-empty-pos-lst (pos-lst rule-ht)
  "returns the list of positions in a rule-ht that have at least one literal"
  (filterl (lambda (pos)
	     (not (null (gethash pos rule-ht))))
	   pos-lst))

;;;; auxiliary rule modification functions

(defun random-pos-del (pos-lst rule-ht)
  "returns a random position with a non-empty domain, 
   if all domains are empty returns NIL"
  (let ((non-empty-pos-lst (non-empty-pos-lst pos-lst rule-ht)))
    (if (null non-empty-pos-lst) NIL
	(random-member non-empty-pos-lst))))

(defun random-pos-add (pos-lst rule-ht max-domain-ht)
  "returns a random position with a non-saturated domain,
   if all domains are saturated returns NIL"
  (let ((non-satur-pos-lst (non-satur-pos-lst pos-lst
					      rule-ht
					      max-domain-ht)))
    (if (null non-satur-pos-lst) NIL
	(random-member non-satur-pos-lst))))

(defun random-lit-del (pos rule-ht)
  "returns a random literal to be deleted at a given position,
   checks if the position is non-empty"
  (let ((cur-domain (gethash pos rule-ht)))
    (if (null cur-domain) NIL
      (random-member cur-domain))))
 
(defun random-lit-add (pos rule-ht max-domain-ht)
  "returns a random literal to be added at a given position, 
   checks if the position is saturated"
  (let* ((cur-domain (gethash pos rule-ht))
         (max-domain (gethash pos max-domain-ht))
         (choose-from-max-domain 
          (filterl (lambda (lit)
		     (not (member lit cur-domain)))
		   max-domain)))
    (if (null choose-from-max-domain) NIL
	(random-member choose-from-max-domain))))

(defun random-member (lst)
  (nth (random (length lst)) lst))

(defun non-satur-pos-lst (pos-lst rule-ht max-domain-ht)
  "returns the list of positions in a rule-ht that are not saturated"
   (filterl 
    (lambda (pos) 
      (let ((cur-domain (gethash pos rule-ht))
            (max-domain (gethash pos max-domain-ht)))
        (and (not (equal-lists-p cur-domain max-domain))
             (foldl (lambda (head tail)
		      (and head tail))
		    t
		    (mapcar (lambda (lit)
			      (member lit max-domain))
			    cur-domain)))))
    pos-lst))

;;;; rule printing functions

(defun print-rule (rule params)
  "prints a rule"
  (let ((es-rule (reduce-to-essential rule params))
	(pos-lst (params-pos-lst params))
        (t-domain-ht (params-t-ht params))
        (f-domain-ht (params-f-ht params)))
    (progn
      (dolist (pos pos-lst)
        (format t "max-f-pos: ~A  domain: ~A ~%"
		pos (gethash pos f-domain-ht))
        (format t "max-t-pos : ~A  domain: ~A ~%"
		pos (gethash pos t-domain-ht))
        (format t "rule pos  : ~A  domain: ~A ~%"
		pos (gethash pos (rule-current-ht es-rule))))
      (let ((t-num (length (rule-t-lst es-rule)))
	    (f-num (length (rule-f-lst es-rule))))
	(format t "total satisfied instances: ~D ~%" (+ t-num f-num))
	(format t "true satisfied instances: ~D ~%" t-num)
	(format t "false satisfied instances: ~D ~%" f-num)
	(format t "true to false ratio: ~D ~%"(coerce (tf-ratio es-rule)
						      'float))))))

;; EOF  
