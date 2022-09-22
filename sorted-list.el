;;; sorted-list.el --- A sorted elisp list with log(n) insert, delete, search using an AVL tree. -*- lexical-binding: t -*-

;; Author: Boris Glavic <lordpretzel@gmail.com>
;; Maintainer: Boris Glavic <lordpretzel@gmail.com>
;; Version: 0.1
;; Package-Requires: ()
;; Homepage: https://github.com/lordpretzel/sorted-list
;; Keywords:


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Gener1al Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Datastructure that wraps a sorted Lisp list by complementing it with an AVL
;; tree which indexes the cons cells of the list. This allows O(log n) lookup,
;; insertion and deletion for a regular elisp list.

;;; Code:

;; ********************************************************************************
;; IMPORTS
(require 'avl-tree)
(require 'cl-lib)

;; ********************************************************************************
;; DATA STRUCTURES
(cl-defstruct (sorted-list-
               (:constructor nil)
               (:constructor sorted-list--create
                             (&optional list
                                        cmpfn
                                        &aux
                                        (lst (sort list cmpfn))
                                        (tree (sorted-list--avl-index-list lst cmpfn)))))
  "A list backed up with an avl tree for fast search."
  (lst nil :type list :documentation "The user-facing list.")
  (tree nil :type avl-tree- :documentation "The avl-tree indexing cons cells of the list.")
  (cmpfn 'string< :type function :documentation "The comparison function for sorting."))

;; ********************************************************************************
;; FUNCTIONS
(defun sorted-list--create-tree-cmp-fn (cmpfn)
  "Create a comparison function for avl-tree backed lists from CMPFN for elements."
  `(lambda (l r)
     (let ((lv (car l))
          (rv (car r)))
      (funcall ',cmpfn lv rv))))

(defun sorted-list--avl-next-smaller (tree data)
  "Return the largest element in avl TREE which is smaller than DATA if it exists.

Matching uses the comparison function previously specified in
`avl-tree-create' when TREE was created. If there is no such
element in the tree, nil is returned."
  (let* ((node (avl-tree--root tree))
         (path nil)
	     (compare-function (avl-tree--cmpfun tree)))
    (catch 'found
      ;; go to leaf for not DATA if it exists
      (while node
	    (cond
	     ((funcall compare-function data (avl-tree--node-data node))
          (push (list node 'left) path)
	      (setq node (avl-tree--node-left node)))
	     ((funcall compare-function (avl-tree--node-data node) data)
          (push (list node 'right) path)
	      (setq node (avl-tree--node-right node)))
	     (t
          (if (avl-tree--node-left node)
              ;; if have a left child, then take left branch, then right branches
              (let ((n (avl-tree--node-left node)))
                (while (avl-tree--node-right n)
                  (setq n (avl-tree--node-right n)))
                (throw 'found (avl-tree--node-data n)))
            (setf node nil)))))
      ;; either DATA does not exist in tree or it is a leaf node, find next smaller in parent
      (cl-loop
       for (p dir) in path
       do
       (when (equal dir 'right)
         (throw 'found (avl-tree--node-data p))))
      (throw 'found nil))))

(defun sorted-list--avl-index-list (l cmpfn)
  "Create an AVL tree that indexes the cells of list L sorted according to CMPFN."
  (setq l (sort l cmpfn))
  (let ((tree (avl-tree-create (sorted-list--create-tree-cmp-fn cmpfn)))
        (el l))
    (while el
      (avl-tree-enter tree el)
      (setq el (cdr el)))
    tree))

;;;###autoload
(defun sorted-list-member-p (sl data)
  "Does DATA exist in sorted list SL."
  (let* ((tr (sorted-list--tree sl))
         (el (avl-tree-member tr (cons data nil))))
    (if el
        (car el)
      nil)))

;;;###autoload
(defalias 'sorted-list-create #'sorted-list--create
  "Create a sorted list. If LST is provided, then sort list to create the sorted list.
CMPFN is a function which takes two arguments, A and B,
and returns non-nil if A is less than B, and nil otherwise.")

;;;###autoload
(defalias 'sorted-list-list #'sorted-list--lst
  "Get the elisp list version of a sorted list.")

(defun sorted-list--equals (smallerfn a b)
  "Given a smaller-then function SMALLERFN, determine wether A = B."
  (not (or (funcall smallerfn a b)
           (funcall smallerfn b a))))

;;;###autoload
(defun sorted-list-insert (sl tuple)
  "Insert TUPLE into sorted list SL."
  (let* ((tr (sorted-list--tree sl))
         (tcell (cons tuple nil))
         (el (avl-tree-member tr tcell))
         (cmp (sorted-list--cmpfn sl)))
    ;; if we already have element, append after last copy of element
    (if el
        ;; append after last item equal to tuple: insert a (a a a b b) -> (a a a a b b)
        (let ((cs el))
          (while (and (cdr cs) (sorted-list--equals cmp (cadr cs) tuple))
            (setf cs (cdr cs)))
          ;; insert after cs
          (setcdr cs (cons
                      tuple
                      (cdr cs))))
      ;; find next smaller element to insert after
      (let ((prior-cs (sorted-list--avl-next-smaller tr tcell)))
        (if prior-cs
            ;; next smaller element exists
            (let ((newcell (cons
                            tuple
                            (cdr prior-cs))))
              (setcdr prior-cs newcell)
              (avl-tree-enter tr newcell))
            ;; no smaller element, create new head
        (let* ((oldhead (sorted-list--lst sl))
               (newhead (cons tuple oldhead)))
          (setf (sorted-list--lst sl) newhead)
          (avl-tree-enter tr newhead)))))))

;;;###autoload
(defun sorted-list-delete (sl tuple)
  "Delete first instance of TUPLE in sorted list SL."
  (let* ((tr (sorted-list--tree sl))
         (tcell (cons tuple nil))
         (el (avl-tree-member tr tcell))
         (cmp (sorted-list--cmpfn sl)))
    (avl-tree-delete tr (cons tuple nil))
    (if (and (cdr el) (sorted-list--equals cmp (cadr el) tuple))
        ;; if not last duplicate of TUPLE, then add back to avl-tree
        (avl-tree-enter tr (cdr el)))
      ;; delete el
      (let ((prior-cs (sorted-list--avl-next-smaller tr tcell)))
        (if prior-cs
            (setcdr prior-cs (cddr prior-cs))
          ;; el is first element: delete list head
          (setf (sorted-list--lst sl) (cdr (sorted-list--lst sl)))))))

(provide 'sorted-list)
;;; sorted-list.el ends here
