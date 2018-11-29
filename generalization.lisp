;;;; generalization.lisp
;;;;
;;;; functions specifying and testing for rule generalization requirements
;;;;
;;;; SBCL 1.3.1

(defparameter +min-num-first-seq+ 10)
(defparameter +min-num-second-seq+ 5) 

(defun min-req-filter (rule pair-lst params)
  "returns pairs in pair-lst that fullfill generalization requirements"
  (filterl (lambda (pair)
	     (min-req-p rule pair params))
	   pair-lst))

(defun min-req-p (rule pair params)
  "checks if after deleting a POS-LIT pair, the minimal requirements with 
   respect to satisfied (sub)sequences in the resulting rule are fullfilled"
  (let* ((lit (get-lit pair))
         (pos (get-pos pair))
         (cons-remove-fn #'remove)
         (first-seq-fn #'inst-first-seq-lst)
         (second-seq-fn #'inst-second-seq-lst)
         (pos-lst (params-pos-lst params))
         (next-rule (change-lit-rule rule
				     pos
				     lit
				     cons-remove-fn
				     params)))
    (and
     (= (length (non-empty-pos-lst pos-lst
				   (rule-current-ht rule)))
	(length (non-empty-pos-lst pos-lst
				   (rule-current-ht next-rule))))
     (min-num-seq-p next-rule
		    +min-num-first-seq+
		    first-seq-fn)
     (min-num-seq-p next-rule
		    +min-num-second-seq+
		    second-seq-fn))))

(defun min-num-seq-p (rule min-num seq-fn)
  "checks if a rule satisfies min-num distinct (sub)sequences across 
   true satisfied instances"
  (let ((num-distinct (length (distinct-seq rule seq-fn))))
    (or (> num-distinct min-num)
	(= num-distinct min-num))))

(defun distinct-seq (rule seq-fn)
  "returns distinct (sub)sequences across true satisfied instances"
  (let ((accum ())
        (lst (rule-t-lst rule))
        (data-ht (params-data-ht +params+)))
    (distinct-seq-helper accum lst seq-fn data-ht)))

(defun distinct-seq-helper (accum lst seq-fn data-ht)
  "accumulates distinct (sub)sequences across true satisfied instances"
  (if (null lst) accum
    (let* ((cur-key (car lst))
           (cur-seq (funcall seq-fn (gethash cur-key data-ht)))
           (next-lst
	    (remove-if (lambda (key)
			 (equal-lists-p cur-seq
					(funcall seq-fn (gethash key data-ht))))
		       lst)))
      (distinct-seq-helper (cons cur-seq accum) next-lst seq-fn data-ht))))

;; EOF
