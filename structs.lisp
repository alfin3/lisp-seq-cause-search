;;;; structs.lisp
;;;;
;;;; structs for handling data instances and representing logic rules
;;;;
;;;; SBCL 1.3.1

;;;; data handling

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

;;;; rule representation

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

;; EOF
