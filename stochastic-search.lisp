;;;; stochastic-search.lisp
;;;;
;;;; functions for hill-climbing with random walk (inspired by WalkSAT of
;;;; Selman, Kautz, and Cohen) and importing data and printing results
;;;;
;;;; SBCL 1.3.1

(defparameter +params+ "parameter bundle is set in import-data")
(defparameter +pos-lst+ (list 0 1 2 3 4 5 6 7 8 9
			      10 11 12 13 14 15 16 17 18 19
			      20))
(defparameter +lit-lst+ (list 'A 'R 'N 'D 'C 'E 'Q 'G 'H 'I
			      'L 'K 'M 'F 'P 'S 'T 'W 'Y 'V))
(defparameter +first-seq-length+ 5)
(defparameter +second-seq-length+ 16)
(defparameter +value-length+ 1)
(defparameter +seq-length+ (length +pos-lst+))
(defparameter +cut-threshold+ 0.0)
(defparameter +max-size-best-rule-lst+ 10)
(defparameter +tf-goal-ratio+ 10.0)
(defparameter +max-stay-without-change+ 100)
(defparameter +total-iterations+ 100)
(defparameter *best-rule-lst* ())

(defstruct params
  ; hash table: key -> data instance (inst struct)
  data-ht 
  ; hash table: position -> list of literals in true data instances
  t-ht
  ; hash table: position -> list of literals in false data instances 
  f-ht
  ; list of sequence positions
  pos-lst
  ; list of data instance keys in data-ht
  key-lst
  ; list of literals occuring across all data instances
  lit-lst)

(defstruct inst
  ; first sequence as a list
  first-seq-lst
  ; second sequence as a list
  second-seq-lst
  ; append second to first sequences
  seq-lst
  ; float value of the data instance
  value)

;;;; running the search algorithm

(defun starter (pathname)
  "starter function for running a rule search"
  (progn
    (import-data (make-pathname :name pathname))
    (format t "data imported ~%")
    (let* ((start-rule (create-start-rule +params+))
           (best-rule-lst (stochastic-search start-rule
					     #'walk-step
					     #'walk-goal-p
					     #'walk-save-p)))
      (print-result best-rule-lst pathname)))) 

;;;; data importing and printing 

(defun import-data (path)
  "imports data and returns NIL if data import is complete"
  (multiple-value-bind (data-ht key-lst) (populate-ht path)
    (setf +params+ (make-params :data-ht data-ht
				:t-ht (tf-domain-ht #'t-p
						    data-ht
						    key-lst
						    +pos-lst+)
				:f-ht (tf-domain-ht #'f-p
						    data-ht
						    key-lst
						    +pos-lst+)
				:pos-lst +pos-lst+
				:key-lst key-lst
				:lit-lst +lit-lst+))
    NIL))
 
(defun populate-ht (path)
  "creates and populates a data hash table from a data file"
  (with-open-file (strm path :direction :input)
    (let ((ht (make-hash-table :test #'equal))
	  (key (read-line strm))
	  (key-lst ()))
      (populate-ht-helper strm ht (cons key key-lst)))))

(defun populate-ht-helper (strm ht key-lst)
  "adds the first key of key-lst and its data instance to a hash table"
  (if (equal (car key-lst) "ENDFILE") (values ht (cdr key-lst))
      (progn
	(setf (gethash (car key-lst) ht)
	      (let ((first-seq-lst (get-lst strm +first-seq-length+ ()))
		    (second-seq-lst (get-lst strm +second-seq-length+ ()))
		    (value (car (get-lst strm +value-length+ ()))))
		(make-inst :first-seq-lst first-seq-lst
			   :second-seq-lst second-seq-lst
			   :seq-lst (append first-seq-lst
					    second-seq-lst)
			   :value value)))
	(read-line strm)
	(read-line strm)
	(populate-ht-helper strm ht (cons (read-line strm) key-lst)))))
    
(defun get-lst (strm num lst)
  "reads from strm and returns a list"
  (if (= num 0) (reverse lst)
      (get-lst strm (- num 1) (cons (read strm) lst))))

(defun tf-domain-ht (tf-p data-ht key-lst pos-lst)
  "returns a domain ht that maps a position to a list containing all literals 
   that occur in true (#'t-p) OR false (#'f-p) instances"
  (let ((domain-ht (empty-ht pos-lst)))
    (dolist (key key-lst) 
      (let ((seq-lst (inst-seq-lst (gethash key data-ht)))
            (val (inst-value (gethash key data-ht))))
	(when (funcall tf-p val)
	  (dolist (pos-lit-pair (mapcar #'list pos-lst seq-lst))
	    (let* ((pos (first pos-lit-pair))
		   (lit (second pos-lit-pair))
		   (pos-domain (gethash pos domain-ht)))
	      (when (not (member lit pos-domain))
		(setf (gethash pos domain-ht)
		      (cons lit pos-domain))))))))
    domain-ht))

(defun empty-ht (pos-lst)
  "initialize an empty domain ht"
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pos pos-lst)
      (setf (gethash pos ht) ()))
    ht))

(defun t-p (val)
   (> val +cut-threshold+))
(defun f-p (val)
  (or (< val +cut-threshold+)
      (= val +cut-threshold+)))

(defun print-result (rule-lst pathname)
  "print a list of rules"
  (let ((pos-lst (params-pos-lst +params+)))
    (format t "~A ~%" pathname)
    (print-lst pos-lst)
    (dolist (rule rule-lst)
      (let* ((es-rule (reduce-to-essential rule +params+))
	     (es-rule-ht (rule-current-ht es-rule)))
	(dolist (pos pos-lst)
	  (print-pos-domain pos es-rule-ht))
	(format t "~D ~%"
		(coerce (tf-ratio es-rule) 'float))
	(format t "~D ~%~%~%"
		(+ (length (rule-t-lst es-rule))
		   (length (rule-f-lst es-rule))))))))
                 
(defun print-pos-domain (pos rule-ht)
  "prints a position and corresponding literals in rule-ht"
  (format t "~A ~%" pos)
  (print-lst (gethash pos rule-ht)))        
(defun print-lst (sym-lst)
  "prints out the list of symbols without parentheses"
  (dolist (sym sym-lst (format t "~%"))
    (format t "~A " sym)))

;;;;  stochastic search algorithm

(defun stochastic-search (start-rule search-step-fn search-goal-p search-save-p)
  "main function for running a local search"
  (let ((iter-counter 0)
        (after-best-counter 0)
        (cur-rule start-rule))
    (setf *best-rule-lst* (list start-rule))
    (print-rule cur-rule +params+)
    (loop
       (progn
	 (when (= after-best-counter +max-stay-without-change+)
	   (progn
	     (setf cur-rule (create-start-rule +params+))
	     (setf after-best-counter 0)))
	 (when (funcall search-save-p cur-rule *best-rule-lst*)
	   (progn
	     (setf *best-rule-lst* (save-to-best-lst cur-rule *best-rule-lst*))
	     (setf after-best-counter 0)
	     (print-rule cur-rule +params+)))
	 (when (funcall search-goal-p cur-rule iter-counter)
	   (return))
	 (setf cur-rule (funcall search-step-fn cur-rule))
	 (incf iter-counter)
	 (incf after-best-counter)))
    *best-rule-lst*))

(defun walk-save-p (rule best-rule-lst)
  "checks if a rule is good enough to be saved in a sorted best list"
  (> (tf-ratio (reduce-to-essential rule +params+))
     (tf-ratio (reduce-to-essential (car best-rule-lst) +params+))))

(defun save-to-best-lst (rule best-rule-lst)
  "adds a new rule to the sorted best list and returns the updated list"
  (if (= (length best-rule-lst) +max-size-best-rule-lst+)
      (cons rule (nbutlast best-rule-lst))
      (cons rule best-rule-lst)))

(defun walk-goal-p (rule iter-counter)
  "tests for termination of a walk"
  (cond
    ((= iter-counter +total-iterations+) t)
    ((null (second (rule-path-lst rule))) NIL) ; start rule
    ((and (= (length (rule-f-lst rule)) 0)
	  (= (length (rule-t-lst rule)) 0))
     NIL)
    ((= (length (rule-f-lst rule)) 0) t)
    (t (> (/ (length (rule-t-lst rule))
	     (length (rule-f-lst rule)))
	  +tf-goal-ratio+))))

(defun walk-step (rule)
  "runs a single step of a walk"
  (let* ((best-pairs (best-pairs rule))
         (best-of-best-pairs (best-tf-ratio-pair rule best-pairs)))
    (cond
      ((null best-of-best-pairs) rule) ;end if no best pair was made
      ((> (es-tf-ratio best-of-best-pairs rule) 1)
       (change-lit-rule rule
			(get-pos best-of-best-pairs)
			(get-lit best-of-best-pairs)
			#'remove
			+params+))
      (t (cond
	   ((> (random 2) 0)
	    (change-lit-rule rule
			     (get-pos best-of-best-pairs)
			     (get-lit best-of-best-pairs)
			     #'remove
			     +params+))
	   ((> (random 2) 0)
	    (let ((picked-pair (random-member best-pairs)))
	      (change-lit-rule rule
			       (get-pos picked-pair)
			       (get-lit picked-pair)
			       #'remove
			       +params+)))
	   (t (add-random-lit-rule rule +params+)))))))
       
(defun es-tf-ratio (pair rule)
  "returns the essential true to false ratio after deleting a POS-LIT pair"
  (let ((new-rule (change-lit-rule rule
				   (get-pos pair)
				   (get-lit pair)
				   #'remove
				   +params+)))
    (tf-ratio (reduce-to-essential new-rule +params+))))

(defun best-pairs (rule)
  "get unsorted list of best deletable POS-LIT pairs from each dimension:
   1) randomly chosen satisfied false instance
   2) randomly chosen position
   3) randomly chosen literal"
  (let* ((rule-ht (rule-current-ht rule))
         (f-lst (rule-f-lst rule))
         (rand-f-key (if (null f-lst)
			 NIL
			 (random-member f-lst)))
         (rand-f-pos (random-pos-del +pos-lst+ rule-ht))
         (rand-f-lit (if (null rand-f-pos)
			 NIL
			 (random-lit-del rand-f-pos rule-ht)))
         (best-lst ())
         (fn-lst (list #'make-key-pairs #'make-pos-pairs #'make-lit-pairs))
         (dim-lst (list rand-f-key rand-f-pos rand-f-lit)))
    (best-pairs-helper rule fn-lst dim-lst best-lst)))

(defun best-pairs-helper (rule fn-lst dim-lst best-lst)
  "returns a unsorted list of best pairs"
  (cond
    ((null fn-lst) best-lst)
    ((null (car dim-lst))
     (best-pairs-helper rule
			(cdr fn-lst)
			(cdr dim-lst)
			best-lst))
    (t (let* ((rule-ht (rule-current-ht rule))
	      (all-pairs (funcall (car fn-lst) (car dim-lst) rule-ht))
	      (best-pair (best-tf-ratio-pair rule all-pairs)))
	 (if (null best-pair)
	     (best-pairs-helper rule
				(cdr fn-lst)
				(cdr dim-lst)
				best-lst)
	     (best-pairs-helper rule
				(cdr fn-lst)
				(cdr dim-lst)
				(cons best-pair best-lst)))))))

(defun make-key-pairs (key rule-ht)
  (let ((seq (inst-seq-lst (gethash key (params-data-ht +params+)))))
    (mapcar (lambda (pos lit) (list pos lit)) +pos-lst+ seq)))
(defun make-pos-pairs (pos rule-ht)
  (let ((lit-lst (gethash pos rule-ht)))
    (mapcar (lambda (lit) (list pos lit)) lit-lst)))
(defun make-lit-pairs (lit rule-ht)
  (foldl (lambda (pos tail)
	   (if (member lit (gethash pos rule-ht))
	       (cons (list pos lit) tail)
	       tail))
	 () +pos-lst+))

(defun best-tf-ratio-pair (rule pair-lst)
 "returns the best POS-LIT pair whose deletion will generate the rule with a  
  best tf essential ratio, checks for generalization requirements beforehand"
 (let ((min-req-pair-lst (min-req-filter rule pair-lst +params+)))
   (if (null min-req-pair-lst) NIL
       (let* ((rule-lst (mapcar (lambda (pair)
				  (change-lit-rule rule
						   (get-pos pair)
						   (get-lit pair)
						   #'remove
						   +params+))
				min-req-pair-lst))
	      (rule-pair-lst (mapcar (lambda (rule pair)
				       (list (reduce-to-essential rule +params+)
					     pair))
				     rule-lst min-req-pair-lst)))
	 (cond
	   ((= (length min-req-pair-lst) 1) (car min-req-pair-lst))
	   (t (let* ((sorted-rule-pair-lst
		      (sort rule-pair-lst
			    (lambda (rule-pair1 rule-pair2)
			      (> (tf-ratio (get-rule rule-pair1))
				 (tf-ratio (get-rule rule-pair2))))))
		     (first-rule-pair (car sorted-rule-pair-lst)))
		(get-pair
		 (random-member
		  (remove-if (lambda (rule-pair)
			       (not (= (tf-ratio (get-rule rule-pair))
				       (tf-ratio (get-rule first-rule-pair)))))
			     sorted-rule-pair-lst))))))))))

(defun tf-ratio (rule)
  "calculates the ratio of true to false satisfied instances"
  (if (= (length (rule-f-lst rule)) 0)
      (length (rule-t-lst rule))
      (/ (length (rule-t-lst rule))
	 (length (rule-f-lst rule)))))
  
(defun get-rule (rule-pair)
  (first rule-pair))
(defun get-pair (rule-pair)
  (second rule-pair))

;; EOF
