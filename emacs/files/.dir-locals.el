((emacs-lisp-mode
  (eval add-hook 'before-save-hook #'prettier-elisp-buffer nil t)))
