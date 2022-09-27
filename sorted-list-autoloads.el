;;; sorted-list-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck_sorted-list" "flycheck_sorted-list.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flycheck_sorted-list.el

(autoload 'sorted-list-member-p "flycheck_sorted-list" "\
Does DATA exist in sorted list SL.

\(fn SL DATA)" nil nil)

(register-definition-prefixes "flycheck_sorted-list" '("sorted-list-"))

;;;***

;;;### (autoloads nil "sorted-list" "sorted-list.el" (0 0 0 0))
;;; Generated autoloads from sorted-list.el

(autoload 'sorted-list-member-p "sorted-list" "\
Does DATA exist in sorted list SL.

\(fn SL DATA)" nil nil)

(register-definition-prefixes "sorted-list" '("sorted-list-"))

;;;***


;;; Generated autoloads from sorted-list.el

(autoload 'sorted-list-member-p "sorted-list" "\
Does DATA exist in sorted list SL.

(fn SL DATA)")
(defalias 'sorted-list-create #'sorted-list--create "\
Create a sorted list. If LST is provided, then sort list to create the sorted list.
CMPFN is a function which takes two arguments, A and B,
and returns non-nil if A is less than B, and nil otherwise.")
(defalias 'sorted-list-list #'sorted-list--lst "\
Get the elisp list version of a sorted list.")
(autoload 'sorted-list-insert "sorted-list" "\
Insert TUPLE into sorted list SL.

(fn SL TUPLE)")
(autoload 'sorted-list-delete "sorted-list" "\
Delete first instance of TUPLE in sorted list SL.

(fn SL TUPLE)")
(register-definition-prefixes "sorted-list" '("sorted-list--"))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; sorted-list-autoloads.el ends here
