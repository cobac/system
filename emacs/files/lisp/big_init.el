;; mu4e
(use-package mu4e
  :straight t
  :general (coba-leader-def "m" 'mu4e)
  :config (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  (setq mail-user-agent 'mu4e-user-agent
        mu4e-change-filenames-when-moving t
        mu4e-get-mail-command "/usr/bin/mbsync -a"
        mu4e-update-interval 120
        ;;mu4e-html2text-command "html2text --unicode-snob"
        ;;shr-color-visible-luminance-min 80
        message-kill-buffer-on-exit t
        mu4e-index-update-error-warning nil
        mu4e-view-show-images t
        mu4e-attachment-dir "/home/coba/Downloads"
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask)
  (add-to-list
   'mu4e-bookmarks
   '(:name "Coba Inbox"
           :query "maildir:/coba/Inbox"
           :key ?j))
  (add-to-list
   'mu4e-bookmarks
   '(:name "Cosas Inbox"
           :query "maildir:/cosas/Inbox"
           :key ?k))
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "coba"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p
               "/coba" (mu4e-message-field msg :maildir))))
          :vars
          '((user-full-name . "Coba")
            (user-mail-address . "coba@cobac.eu")
            (mu4e-drafts-folder . "/coba/Drafts")
            (mu4e-sent-folder . "/coba/Sent")
            (mu4e-refile-folder . "/coba/Archives")
            (mu4e-trash-folder . "/coba/Trash")
            (smtpmail-smtp-user . "coba@cobac.eu")
            (smtpmail-smtp-server . "mail.your-server.de")
            (smtpmail-stream-type . ssl)
            (smtpmail-smtp-service . 465)))
         (make-mu4e-context
          :name "j-cosas"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p
               "/cosas" (mu4e-message-field msg :maildir))))
          :vars
          '((user-full-name . "David Coba")
            (user-mail-address . "cosas@cobac.eu")
            (mu4e-drafts-folder . "/cosas/Drafts")
            (mu4e-sent-folder . "/cosas/Sent")
            (mu4e-refile-folder . "/cosas/Archives")
            (mu4e-trash-folder . "/cosas/Trash")
            (smtpmail-smtp-user . "cosas@cobac.eu")
            (smtpmail-smtp-server . "mail.your-server.de")
            (smtpmail-stream-type . ssl)
            (smtpmail-smtp-service . 465)))))
  (setq mu4e-split-view 'horizontal
        mu4e-view-show-addresses t
        mu4e-view-show-images t)
  (add-to-list
   'mu4e-view-actions '("browser" . mu4e-action-view-in-browser)
   t)
  (coba-local-leader-def
    'mu4e-view-mode-map "t"
    '(lambda ()
       (interactive)
       (org-capture "nil" "f")))
  (add-to-list 'auto-mode-alist '("\\.eml\\'" . org-mode))
  (add-to-list
   'mu4e-marks
   '(archive
     :char "A"
     :prompt "Archive"
     :dyn-target (lambda (target msg)
                   (mu4e-get-refile-folder msg))
     :show-target (lambda (target) "archive")
     :action
     (lambda (docid msg target)
       (mu4e--server-move
        docid (mu4e--mark-check-target target) "+S-u-N"))))
  (mu4e~headers-defun-mark-for archive)
  (general-def
    'mu4e-headers-mode-map
    :states
    '(motion visual normal emacs)
    "A"
    'mu4e-headers-mark-for-archive)
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)
  (setq gnus-icalendar-org-capture-file "~/Sync/Org/todo.org"
        gnus-icalendar-org-capture-headline '("Other"))
  (gnus-icalendar-org-setup))

(use-package mu4e-alert
  :straight t
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-mode-line-display))

;; TODO: Fix mu4e-thread-folding
;; Error with nano-color-background not defined
;;(use-package mu4e-thread-folding
;;  :straight (:type git :host github :repo "rougier/mu4e-thread-folding")
;;  :config
;;  (require 'mu4e-thread-folding)
;;  (add-to-list 'mu4e-header-info-custom
;;               '(:empty . (:name "Empty"
;;                                 :shortname ""
;;                                 :function (lambda (msg) "  "))))
;;  (setq mu4e-headers-fields '((:empty           .     2)
;;                              (:human-date    .     12)
;;                              (:flags           .     6)
;;                              (:mailing-list  .     10)
;;                              (:from          .     22)
;;                              (:subject         .    nil)))
;;  (define-key mu4e-headers-mode-map (kbd "<tab>")       'mu4e-headers-toggle-at-point)
;;  )

;; TODO: check inline thing
;; (use-package
;;   org-msg
;;   :straight t
;;   :config
;;   (setq
;;    org-msg-default-alternatives
;;    '((new . (text html))
;;      (reply-to-text . (text))
;;      (reply-to-html . (html text)))
;;    org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
;;    org-msg-startup "inlineimages"
;;    org-msg-convert-citation t)
;;   (setq-default org-html-with-latex 'dvipng)
;;   (org-msg-mode)
;;   (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler))

(mu4e)

;; Latex
(defvar org-tex-apa-template
  (coba-file-content-as-string
   "~/.emacs.d/ox-templates/apa-article.tex"))

(defvar org-tex-apa-es-template
  (coba-file-content-as-string
   "~/.emacs.d/ox-templates/apa-article-es.tex"))

(defvar org-tex-report-template
  (coba-file-content-as-string "~/.emacs.d/ox-templates/report.tex"))

(defvar org-tex-graphix-template
  (coba-file-content-as-string "~/.emacs.d/ox-templates/graphix.tex"))

(defvar org-tex-footmisc-template
  (coba-file-content-as-string
   "~/.emacs.d/ox-templates/footmisc.tex"))

(defvar org-tex-math-template
  (coba-file-content-as-string "~/.emacs.d/ox-templates/math.tex"))

(defvar org-tex-listings-template
  (coba-file-content-as-string
   "~/.emacs.d/ox-templates/listings.tex"))

(defvar custom-tex-template nil
  "Custom latex preamble for org-export.")

(setq custom-tex-template
      (mapconcat 'identity
                 (list
                  org-tex-apa-template
                  org-tex-math-template
                  org-tex-graphix-template)
                 "\n"))

(defun coba-define-org-tex-template ()
  "Define `org-latex-classes' concatenating snippets."
  (interactive)
  (setq org-tex-apa-template
        (coba-file-content-as-string
         "~/.emacs.d/ox-templates/apa-article.tex"))
  (setq org-tex-apa-es-template
        (coba-file-content-as-string
         "~/.emacs.d/ox-templates/apa-article-es.tex"))
  (setq org-tex-report-template
        (coba-file-content-as-string
         "~/.emacs.d/ox-templates/report.tex"))
  (setq org-tex-graphix-template
        (coba-file-content-as-string
         "~/.emacs.d/ox-templates/graphix.tex"))
  (setq org-tex-footmisc-template
        (coba-file-content-as-string
         "~/.emacs.d/ox-templates/footmisc.tex"))
  (setq org-tex-math-template
        (coba-file-content-as-string
         "~/.emacs.d/ox-templates/math.tex"))
  (setq org-tex-listings-template
        (coba-file-content-as-string
         "~/.emacs.d/ox-templates/listings.tex"))
  (setq org-latex-classes
        `(("custom"
           ,(format "%s
    [NO-DEFAULT-PACKAGES]
    [NO-PACKAGES]
    [EXTRA]"
                    custom-tex-template)
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("custom-apa"
           ,(format
             "%s
    [NO-DEFAULT-PACKAGES]
    [NO-PACKAGES]
    [EXTRA]"
             custom-tex-template)
           ("\\section*{%s}" . "\\section*{%s}")
           ("\\subsection*{%s}" . "\\subsection*{%s}")
           ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph*{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph*{%s}" . "\\subparagraph*{%s}"))
          ("article"
           "\\documentclass[11pt]{article}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
[EXTRA]"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

(coba-define-org-tex-template)

;; TODO: Check what's up with codeblocks

(defun coba-org-latex-remove-title (str)
  "Remove empty \\title{} from STR.
    From https://stackoverflow.com/questions/57967064/disable-title-in-org-latex-export ."
  (replace-regexp-in-string "^\\\\title{}$" "" str))

(defun coba-org-latex-remove-author (str)
  "Remove default \\author{Coba} from STR.
    From https://stackoverflow.com/questions/57967064/disable-title-in-org-latex-export ."
  (replace-regexp-in-string "^\\\\author{Coba}$" "" str))

(defun coba-org-latex-remove-date (str)
  "Remove default \\date{\\today} from STR.
    From https://stackoverflow.com/questions/57967064/disable-title-in-org-latex-export ."
  (replace-regexp-in-string "^\\\\date{\\\\today}$" "" str))

(advice-add
 'org-latex-template
 :filter-return 'coba-org-latex-remove-title)

(advice-add
 'org-latex-template
 :filter-return 'coba-org-latex-remove-author)

(advice-add
 'org-latex-template
 :filter-return 'coba-org-latex-remove-date)

(setq org-startup-with-inline-images t
      org-latex-compiler "xelatex"
      org-latex-bib-compiler "bibtex"
                                        ;org-export-in-background t
      org-latex-default-class "custom"
      org-latex-hyperref-template nil
      org-latex-prefer-user-labels t
      org-export-with-toc nil
      org-latex-src-block-backend 'engraved
      org-latex-listings-options
      '(("basicstyle" "\\ttfamily\\color{code-fg}")
        ("stringstyle" "\\ttfamily\\color{code-string}")
        ("commentstyle" "\\color{code-comment}")
        ("keywordstyle" "\\color{code-keyword}")
        ("upquote" "true")
        ("backgroundcolor" "\\color{code-bg}")
        ("frame" "single")
        ("framesep" "5pt")
        ("rulecolor" "\\color{code-comment}")
        ("framerule" "1pt"))
      org-latex-pdf-process
      '("ln -s ~/Sync/Brain/bib.bib bib.bib"
        "latexmk -pdflatex='xelatex -interaction nonstopmode' -shell-escape -pdf -bibtex -f %f"))

(add-to-list 'org-latex-default-packages-alist '("" "txfonts" t))

(add-to-list 'org-latex-default-packages-alist '("" "graphviz" t))

;; Latex
(use-package tex
  :straight auctex
  :config (TeX-global-PDF-mode t)
  (setq TeX-source-correlate-mode t
        TeX-source-correlate-start-server t
        TeX-parse-self t
        TeX-auto-save t
        TeX-view-program-selection '((output-pdf "Zathura"))
        LaTeX-command "xelatex --synctex=1"
        pdf-latex-command "XeLaTeX")
  (add-hook
   'LaTeX-mode-hook
   (lambda ()
     (add-to-list
      'TeX-command-list
      '("XeLaTeX"
        "%`xelatex --synctex=1%(mode)%' %t"
        TeX-run-TeX
        nil
        t))
     (setq TeX-command-extra-options "-file-line-error -shell-escape"
           TeX-command-default "XeLaTeX"
           TeX-auto-untabify t
           TeX-engine 'xetex
           TeX-show-compilation nil)
     (TeX-global-PDF-mode t)
     (setq TeX-save-query nil)))
  (coba-local-leader-def
    :keymaps
    'LaTeX-mode-map
    "s"
    'LaTeX-section
    "e"
    'LaTeX-environment
    "m"
    'TeX-insert-macro
    "p"
    'preview-buffer
    "c"
    'coba-texcount)
  (general-def
    :states '(normal visual insert motion emacs)
    :keymaps
    'LaTeX-mode-map
    "C-f"
    'TeX-font
    "C-c C-c"
    'TeX-command-run-all
    "C-c C-a"
    'TeX-command-master)
  (defun coba-texcount ()
    "Print number of output words from the current buffer. It invoques texcount."
    (interactive)
    (save-buffer)
    (shell-command (concat "texcount " (buffer-name))))
  (defun coba-bibtex-check ()
    "Check the validity of a bibtex file."
    (interactive)
    (save-buffer)
    (shell-command (concat "biber -tool -V " (buffer-name))))
  (coba-local-leader-def
    :keymaps 'bibtex-mode-map "c" 'coba-bibtex-check))

;; ESS
(use-package ess
  :straight t
  ;; :hook
  ;; (ess-r-mode . lsp)
  ;; (inferior-ess-r-mode . lsp)
  :config
  (require 'ess-site)
  (setq ess-ask-for-ess-directory nil
        comint-move-point-for-output 'others
        comint-scroll-to-bottom-on-input 'this
        ess-eval-visibly 'nowait
        ess-style 'RStudio)
  (evil-set-initial-state 'ess-r-help-mode 'motion)
  (general-def 'ess-r-help-mode-map "q" 'quit-window)
  (general-def
    :keymaps '(comint-mode-map)
    :states
    '(motion normal insert)
    "C-S-k"
    'comint-previous-input
    "C-S-j"
    'comint-next-input
    "C-h"
    'comint-previous-matching-input-from-input
    "C-ñ"
    'ess-insert-assign)
  (general-def
    :states '(insert)
    :keymaps
    'ess-mode-map
    "C-ñ"
    'ess-insert-assign)
  (coba-local-leader-def :keymaps 'ess-mode-map "b" 'ess-eval-buffer))

(evil-set-initial-state 'ess-julia-help-mode 'motion)

(general-def 'ess-julia-help-mode-map "q" 'quit-window)

(use-package poly-R
  :straight t)

(use-package poly-markdown
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown+r-mode)))

;;  Stan
(use-package stan-mode
  :straight t
  :after company
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  :config (setq stan-indentation-offset 2))

(use-package company-stan
  :straight t
  :after stan-mode
  :hook (stan-mode . company-stan-setup))

(use-package eldoc-stan
  :straight t
  :after stan-mode
  :hook (stan-mode . eldoc-stan-setup))

(use-package flycheck-stan
  :straight t
  :after stan-mode
  :hook
  ((stan-mode . flycheck-stan-stanc2-setup)
   (stan-mode . flycheck-stan-stanc3-setup))
  :config
  (setq flycheck-stanc-executable nil
        flycheck-stanc3-executable nil))

(use-package stan-snippets
  :straight t
  :after stan-mode)

;; Julia
(use-package lsp-julia
  :straight (:type git
                   :host github
                   :repo "non-Jedi/lsp-julia")
  :after lsp-mode
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.6")
  (setq lsp-julia-timeout 99999999))

(use-package julia-mode
  :straight t
  :config (add-hook 'julia-mode-hook #'lsp))

(use-package julia-vterm
  :straight t
  :hook (julia-mode . julia-vterm-mode)
  :config
  (general-def
    :states '(normal visual insert motion emacs)
    :keymaps
    'julia-mode-map
    "C-c C-c"
    'julia-vterm-send-region-or-current-line
    "C-c C-l"
    'julia-vterm-send-current-line
    "C-t"
    'coba-julia-vterm-define-threads)
  (defun coba-julia-vterm-define-threads ()
    (interactive)
    (let ((val (read-from-minibuffer "Number of threads: ")))
      (setq julia-vterm-repl-program
            (concat "/usr/bin/julia -t " val))))
  (setq display-buffer-reuse-frames t))

(use-package ob-julia-vterm
  :straight t
  :config (add-to-list 'org-babel-load-languages '(julia-vterm . t))
  (org-babel-do-load-languages
   'org-babel-load-languages org-babel-load-languages)
  (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm))

;;(use-package ob-julia
;;  :straight (:type git :host github :repo "nico202/ob-julia" :files (:defaults))
;;  )

;; TODO: Julia-formatter
;; (use-package julia-formatter
;;   :straight (:type git :repo "https://codeberg.org/FelipeLema/julia-formatter.el"
;;                    :files ("*.jl" "*.el" "*.toml"))
;;   :config
;;   (julia-formatter-setup-aggressive-hooks)
;;   )

;; Haskell
(use-package haskell-mode
  :straight t
  :general
  (general-def
    '(haskell-mode-map haskell-interactive-mode-map) "C-ñ"
    '(lambda ()
       (interactive)
       (insert "-> "))
    "C-Ñ"
    '(lambda ()
       (interactive)
       (insert "<- ")))
  (general-def
    'haskell-mode-map "C-c C-c"
    '(lambda ()
       (interactive)
       (save-buffer)
       (haskell-process-load-file)))
  (coba-local-leader-def
    'haskell-mode-map "r" 'hlint-refactor-refactor-at-point "h"
    'hoogle)
  (general-def
    'haskell-interactive-mode-map
    :states
    '(insert motion)
    "C-k"
    'haskell-interactive-mode-history-previous
    "C-j"
    'haskell-interactive-mode-history-next)
  :config (evil-set-initial-state 'haskell-error-mode 'motion))

;;(use-package ormolu
;;:straight t
;;:hook (haskell-mode . ormolu-format-on-save-mode)
;;:general
;;(general-def 'haskell-mode-map
;;:states '(normal)
;;"gf" 'ormolu-format-buffer)
;;:config
;;(setq ormolu-no-cabal t)
;;)

(use-package flycheck-haskell
  :straight t
  :config (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)
  (setq-default flycheck-disabled-checkers
                '(haskell-ghc haskell-stack-ghc)))

(use-package hlint-refactor
  :straight t)

;; nov
(use-package nov
  :straight t
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (defun coba-nov-font-setup ()
    (face-remap-add-relative
     'variable-pitch
     :family "ibm plex serif text"
     :height 1.0))
  (add-hook 'nov-mode-hook 'coba-nov-font-setup))

;; Rust
(use-package rustic
  :straight t
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'before-save-hook 'lsp-format-buffer nil t)
  (defun coba-rustic-autosave-mode-hook ()
    "Enable auto-saving in rustic-mode buffers."
    (when buffer-file-name
      (setq-local compilation-ask-about-save nil)))
  (add-hook 'rustic-mode-hook 'coba-rustic-autosave-mode-hook)
  (evil-set-initial-state 'rustic-popup-mode 'emacs)
  (general-def
    'rustic-mode-map
    "C-c C-c s"
    'lsp-rust-analyzer-status
    "C-c C-c C-S-r"
    'rustic-cargo-comint-run
    "C-c C-c C-S-d"
    'coba-rustic-cargo-doc-std
    "C-c C-c C-S-b"
    'rustic-cargo-build-doc)
  (general-def
    'rustic-mode-map "C-ñ"
    '(lambda ()
       (interactive)
       (insert "-> ")))
  (defun coba-rustic-cargo-doc-std ()
    "Open the documentation for the standard library in a browser."
    (interactive)
    (shell-command "rustup docs --std"))
  (defun coba-cargo-new (args)
    "Call cargo new with args"
    (interactive "scargo new: ")
    (message (format "cargo new %s" args))
    (shell-command (format "cargo new %s" args)))
  (add-hook
   'rustic-mode-hook
   (lambda ()
     (push '(?< . ("< " . " >")) evil-surround-pairs-alist))))

(use-package ledger-mode
  :straight t
  :config
  (defun coba-hledger-call (command)
    (interactive)
    (shell-command
     (string-join (list
                   "hledger"
                   "-f /home/coba/Sync/oros/main.ledger"
                   command
                   "--monthly --sort --tree"
                   "-b=\"12 months ago\"")
                  " "))
    (windmove-right)
    (delete-other-windows)
    (text-scale-decrease 2))
  (coba-leader-def
    "eb"
    '(lambda ()
       (interactive)
       (coba-hledger-call "bal"))
    "ei"
    '(lambda ()
       (interactive)
       (coba-hledger-call "is"))))

(use-package gleam-ts-mode
  :straight t
  :mode (rx ".gleam" eos)
  :config
  (add-hook
   'gleam-ts-mode-hook
   (lambda ()
     (add-hook 'before-save-hook 'gleam-ts-format nil 'make-it-local)))
  (general-def
    '(gleam-ts-mode-map) "C-ñ"
    '(lambda ()
       (interactive)
       (insert "-> ")))
  (add-to-list 'eglot-server-programs
               '(gleam-ts-mode . ("gleam" "lsp")))
  (add-hook 'gleam-ts-mode-hook 'eglot-ensure))

;; (use-package exercism :straight t)

(use-package vue-mode
  :straight t
  :config
  (add-hook 'mmm-mode-hook
            (lambda ()
              (set-face-background 'mmm-default-submode-face nil))))
