[![License: GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
<!-- [![GitHub release](https://img.shields.io/github/release/lordpretzel/sorted-list.svg?maxAge=86400)](https://github.com/lordpretzel/sorted-list/releases) -->
<!-- [![MELPA Stable](http://stable.melpa.org/packages/sorted-list-badge.svg)](http://stable.melpa.org/#/sorted-list) -->
<!-- [![MELPA](http://melpa.org/packages/sorted-list-badge.svg)](http://melpa.org/#/sorted-list) -->
[![Build Status](https://secure.travis-ci.org/lordpretzel/sorted-list.png)](http://travis-ci.org/lordpretzel/sorted-list)


# sorted-list

Small elisp library for efficiently maintaining a sorted `list`. Internally, this uses an AVL-tree to store the cells of the list, but the sorted list is also available as a regular lisp list (that is the point). Note that because of the high constant factor of AVL-trees, it only makes sense to use this data structure if there are many lookups / updates and the list is large enough.

## Example Usage

~~~elisp
;; create a sorted list from a regular list, providing a comparison function implementing the sort order.
(setq mysortlist (sorted-list-create '(4 3 5 10 7) '<))

(sorted-list-member-p mysortlist 5) ;; runs in O(log n)
(pp (sorted-list-list mysortlist)) ;; get the underlying sorted lisp list

(sorted-list-insert mysortlist 1) ;; runs in O(log n)
(pp (sorted-list-list mysortlist)) ;; get the underlying sorted lisp list

(sorted-list-delete mysortlist 5) ;; runs in O(log n)
(pp (sorted-list-list mysortlist)) ;; get the underlying sorted lisp list

;; benchmark searching the last element in a list (1 ... 300000) using sorted list vs. regular list
(setq mysortlist (sorted-list-create (number-sequence 1 300000) '<))
(setq myunsortlist (number-sequence 1 300000))
(benchmark 10
           '(dotimes (i 1000) (sorted-list-member-p mysortlist 299999)))
;; Elapsed time: 0.065488s
(benchmark 10
           '(dotimes (i 1000) (member 299999 myunsortlist)))
;; Elapsed time: 15.424318s
~~~

## Installation

<!-- ### MELPA -->

<!-- Symbol’s value as variable is void: $1 is available from MELPA (both -->
<!-- [stable](http://stable.melpa.org/#/sorted-list) and -->
<!-- [unstable](http://melpa.org/#/sorted-list)).  Assuming your -->
<!-- ((melpa . https://melpa.org/packages/) (gnu . http://elpa.gnu.org/packages/) (org . http://orgmode.org/elpa/)) lists MELPA, just type -->

<!-- ~~~sh -->
<!-- M-x package-install RET sorted-list RET -->
<!-- ~~~ -->

<!-- to install it. -->

### Quelpa

Using [use-package](https://github.com/jwiegley/use-package) with [quelpa](https://github.com/quelpa/quelpa).

~~~elisp
(use-package
:quelpa ((sorted-list
:fetcher github
:repo "lordpretzel/sorted-list")
:upgrade t)
)
~~~

### straight

Using [use-package](https://github.com/jwiegley/use-package) with [straight.el](https://github.com/raxod502/straight.el)

~~~elisp
(use-package sorted-list
:straight (sorted-list :type git :host github :repo "lordpretzel/sorted-list")
~~~

### Source

Alternatively, install from source. First, clone the source code:

~~~sh
cd MY-PATH
git clone https://github.com/lordpretzel/sorted-list.git
~~~

Now, from Emacs execute:

~~~
M-x package-install-file RET MY-PATH/sorted-list
~~~

Alternatively to the second step, add this to your Symbol’s value as variable is void: \.emacs file:

~~~elisp
(add-to-list 'load-path "MY-PATH/sorted-list")
(require 'sorted-list)
~~~
