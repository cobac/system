;;; Compiled snippets and support files for `bibtex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'bibtex-mode
                     '((",misc" "@misc{$2,\n  DATE_ADDED =   {`(current-time-string)`},\n  author =       {$3},\n  doi =          {},\n  title =        {$4},\n  url =          {$1},\n  year =         $5,\n}\n\n$0\n\n" "Misc" nil nil nil "/home/coba/.emacs.d/snippets/bibtex-mode/Misc" nil nil)
                       (",article" "@article{$1,\n  author =       {$2},\n  title =        {$3},\n  journal =      {$4},\n  volume =       $5,\n  number =       $6,\n  pages =        {$7},\n  year =         $8,\n  doi =          {$9},\n  url = {$10},\n  DATE_ADDED =   {`(current-time-string)`},\n}\n$0" "Article" nil nil nil "/home/coba/.emacs.d/snippets/bibtex-mode/Article" nil nil)))


;;; Do not edit! File generated at Sat Nov 14 16:12:34 2020
