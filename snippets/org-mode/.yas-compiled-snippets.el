;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '((",sl" "#+BEGIN_SRC latex\n$1\n#+END_SRC\n$0" "latex source block" nil nil nil "/home/coba/.emacs.d/snippets/org-mode/latex source block" nil nil)
                       (",r" "#+label: \n#+caption: \n#+BEGIN_SRC R :eval :session :results output :exports both\n$1\n#+END_SRC\n$0" "R source block" nil nil nil "/home/coba/.emacs.d/snippets/org-mode/elisp source block" nil nil)
                       (",dg" "#+BEGIN_SRC dot :file $1.pdf :cmdline -Kdot -Tpdf\n  digraph {\n        rankdir=LR;\n        $2\n  }\n#+END_SRC\n\n#+label: fig:$1\n#+ATTR_LATEX: :width 7cm :placement [H]\n#+caption:\n#+RESULTS:\n[[file:$1.pdf]]\n\n$0" "Digraph" nil nil nil "/home/coba/.emacs.d/snippets/org-mode/digraph" nil nil)
                       (",r" "#+BEGIN_SRC R :eval :session :results output :tangle yes :exports both\n$1\n#+END_SRC\n$0" "R source block" nil nil nil "/home/coba/.emacs.d/snippets/org-mode/R source block" nil nil)))


;;; Do not edit! File generated at Sat Nov 14 16:12:34 2020
