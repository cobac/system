                                        ; Straight.el config

(setq straight-recipes-gnu-elpa-use-mirror t)

;; Bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'ihibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)

                                        ; Built-in setup

;; Remove scroll, tool and menu bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Remove startup screen
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
;; Show matching paranthesis
(show-paren-mode 1)
(electric-pair-mode)

;; Always indent with spaces
(setq-default indent-tabs-mode nil)

;; Font
(setq default-frame-alist 
      (add-to-list 'default-frame-alist '(font . "ibm plex mono-14")))


(setq-default line-spacing 0.15)
;; Not-crazy scroll
(setq scroll-step 1
      scroll-conservatively 10000)

                                        ;; Keep init.el clean
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)


(setq delete-old-versions -1 )		;; delete excess backup versions silently
(setq version-control t )		;; use version control
(setq vc-make-backup-files t )		;; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ;; which directory to put backups file
(setq vc-follow-symlinks t )	        ;; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t)	        ;; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	;; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	;; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	;; sentence SHOULD end with only a point.
(setq default-fill-column 120)		;; toggle wrapping text at the 80th character
(setq initial-scratch-message nil) ; print a default message in the empty scratch buffer opened at startup
(setq delete-by-moving-to-trash t)
(setq auth-sources '("~/.authinfo.gpg"))
(put #'downcase-region 'disabled nil)
(put #'upcase-region 'disabled nil)
(put #'narrow-to-region 'disabled nil)

                                        ; Packages
;; Load upstream org
(straight-use-package
 '(org :host github :repo "emacs-straight/org-mode" :local-repo "org"))

;; Visual
(use-package doom-themes
  :straight t
  :config
  (doom-themes-org-config)
  (load-theme 'doom-gruvbox t)
  )

(use-package memoize
  :straight t)

(use-package all-the-icons
  :straight t
  :after memoize)

(use-package doom-modeline
  :straight t
  :after all-the-icons
  :hook (after-init . doom-modeline-mode)
  :config 
  (fset 'battery-update #'ignore)
  (setq doom-modeline-modal-icon nil
        doom-modeline-enable-word-count t)
  )

;; General - which-key

(use-package which-key
  :straight t
  :config
  (which-key-mode 1)
  )

(use-package general
  :after which-key
  :straight t
  :config
  (setq general-override-states '(insert emacs hybrid normal visual motion operator replace))

  (general-create-definer coba-leader-def
    :states '(normal visual insert motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-create-definer coba-local-leader-def
    :states '(normal visual motion)
    :prefix ",")

  (coba-leader-def
    "f" '(:ignore t :which-key "Files")
    "fi" '(lambda() (interactive)(find-file "~/.emacs.d/init.el")))
  )

;; Evil

(use-package evil
  :straight t
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil) ;for evil-collection

  :general
  (general-def
    :states '(normal motion)
    "ñ" 'counsel-yank-pop
    )

  :config
  (use-package evil-anzu
    :straight t)
  (use-package evil-collection
    :straight t
    :config
    ;;(evil-collection-init 'mu4e)
    (evil-collection-init 'dired)
    ;;(evil-collection-init 'calendar)
    ;;(evil-collection-init 'magit)
    )
  (use-package evil-snipe
    :straight t
    :config
    (evil-snipe-mode 1)
    (evil-snipe-override-mode 1)
    (setq
     evil-snipe-smart-case t
     evil-snipe-show-prompt nil
     evil-snipe-auto-scroll nil
     evil-snipe-scope 'whole-buffer
     evil-snipe-repeat-scope 'whole-buffer)
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
    )
  (use-package evil-surround
    :straight t
    :after evil
    :config (global-evil-surround-mode))
  )


;; Ivy - Counsel - Prescient

(use-package ivy
  :straight t
  :after general
  :config
  (ivy-mode 1)
  (setq ivy-count-format ""
        ivy-wrap t
        ivy-height 25)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))

  (general-def 'ivy-minibuffer-map
    "<escape>" 'minibuffer-keyboard-quit
    "C-j" 'ivy-next-line
    "C-k" 'ivy-previous-line
    "C-l" 'ivy-dispatching-done
    )
  (use-package counsel
    :straight t
    :config
    (general-def 'ivy-switch-buffer-map
      "C-k" 'ivy-previous-line
      "C-d" 'ivy-switch-buffer-kill)
    (coba-leader-def
      "ff" 'counsel-find-file
      "SPC" 'counsel-switch-buffer
      "x" 'counsel-M-x
      "T" 'counsel-load-theme
      )
    (general-def 'counsel-find-file-map
      "C-h" 'counsel-up-directory
      "C-l" 'ivy-alt-done
      )
    )

  (use-package prescient
    :straight t
    :config
    (use-package ivy-prescient
      :straight t
      :after counsel
      :config
      (prescient-persist-mode)
      (ivy-prescient-mode)
      )
    )
  )

;; Hydra
(use-package hydra
  :straight t)

(defhydra coba-hydra-switch-windows ()
  "Manage window movement with evil-window funcions." 
  ("h" evil-window-left) 
  ("j" evil-window-down)
  ("k" evil-window-up)
  ("l" evil-window-right)
  )
(coba-leader-def
  "w" 'coba-hydra-switch-windows/body
  "dd" 'evil-delete-buffer
  "s" '(:ignore t :which-key "Split")
  "sd" 'delete-other-windows
  "sh" 'split-window-below
  "ss" 'split-window-horizontally
  "sr" 'evil-window-rotate-downwards
  "l" 'evil-window-next
  "bs" 'save-some-buffers
  )

;; Org

(use-package org-super-agenda
  :straight (:host github :repo "alphapapa/org-super-agenda")
  :after org
  :general
  (general-def 'org-super-agenda-header-map
    "j" 'org-agenda-next-item
    "k" 'org-agenda-previous-item
    )
  :config
  (setq org-super-agenda-groups
        '(
          (:discard(:and(:category "Annuals" :scheduled past)))
          (:discard(:file-path "archive.org"))
          (:name "Today's deadlines"
                 :deadline today
                 :order 1)
          (:name "Overdue"
                 :deadline past
                 :order 0)
          (:name "Habits"
                 :habit t
                 :order 30)
          (:name "Today's Annuals"
                 :and(
                      :category "Annuals"
                      :scheduled today)
                 :order 3)
          (:name "Annuals"
                 :and(
                      :category "Annuals"
                      :scheduled future)
                 :order 80)
          (:name "Schedule"
                 :time-grid t
                 :order 2)
          (:name "Scheduled"
                 :scheduled today
                 :order 4)
          (:name "University Deadlines"
                 :and(
                      :deadline future
                      :tag "uni")
                 :order 10)
          (:name "Other Deadlines"
                 :deadline future
                 :order 11)
          (:name "Refile"
                 :scheduled past
                 :order 99)
          ))
  )

(org-super-agenda-mode)
(use-package transient
  :straight t)
(use-package org-ql
  :straight (:host github :repo "alphapapa/org-ql"))

(setq org-agenda-files '("~/Nextcloud/Org")
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-log-done (quote time)
      org-log-readline (quote time)
      org-log-reschedule (quote time)
      org-refile-allow-creating-parent-nodes 'confirm
      org-archive-location "~/Nextcloud/Org/archive.org::* From ??"
      org-deadline-warning-days 14
      org-refile-use-outline-path t
      org-use-property-inheritance t
      calendar-date-style 'european
      org-agenda-start-on-weekday nil
      org-outline-path-complete-in-steps nil
      calendar-week-start-day 1
      org-default-notes-file "~/Nextcloud/Org/refile.org"
      org-capture-templates '(
                              ("t" "Todo" entry (file "~/Nextcloud/Org/refile.org")
                               "* TODO %?"
                               :empty-lines 1)
                              ("l" "Link" entry (file "~/Nextcloud/Org/refile.org")
                               "* TODO [%?[]]\n:PROPERTIES:\n:CREATED: %U\n:END:"
                               :empty-lines 1)
                              ("c" "Check Computer" entry (file+olp "~/Nextcloud/Org/todo.org" "Computer" "Check")
                               "* TODO [%?[]]\n:PROPERTIES:\n:CREATED: %U\n:END:"
                               :empty-lines 1)
                              ("p" "Check Psychology" entry (file+olp "~/Nextcloud/Org/todo.org" "Psychology" "Check")
                               "* TODO [%?[]]\n:PROPERTIES:\n:CREATED: %U\n:END:"
                               :empty-lines 1)
                              )
      org-refile-targets (quote(
                                ("~/Nextcloud/Org/todo.org" :maxlevel . 10)
                                ("~/Nextcloud/Org/annuals.org" :maxlevel . 10)
                                ))
      org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED"))
      org-agenda-custom-commands '(("w" "Weekly view" (
                                                       (org-ql-block '(and (todo "WAITING")))
                                                       (agenda))))
      )
(general-define-key
 :keymaps 'org-capture-mode-map
 [remap evil-save-and-close]          'org-capture-finalize
 [remap evil-save-modified-and-close] 'org-capture-finalize
 [remap evil-quit]                    'org-capture-kill)

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%e-%m-%Y, %a>" . "<%e-%m-%Y, %a %H:%M>"))

(coba-leader-def
  "c"  'org-capture
  "a"  '(:ignore t :which-key "Org-Agenda")
  "aa" '(lambda() (interactive)(coba-org-agenda-weekly))
  "o"  '(lambda() (interactive)(find-file "~/Nextcloud/Org/todo.org"))
  "n"  '(lambda() (interactive)(find-file "~/Nextcloud/Org/notas.org"))
  "r"  '(lambda() (interactive)(find-file "~/Nextcloud/Org/refile.org"))
  "ad" '(lambda ()(interactive)(org-ql-search "~/Nextcloud/Org/todo.org"
                                 '(done) :sort '(date priority todo)))
  "ap" '(lambda()(interactive)
          (org-ql-search (org-agenda-files)
            '(and (tags "track") (or (todo "TODO") (todo "WAITING")))
            :title "Projects"
            :sort '(date priority todo)
            :super-groups '((:auto-property "Project"))))
  )

(defun coba-org-agenda-weekly ()
(org-agenda nil "w")
(delete-other-windows)
)

(defun coba-org-create-project (project)
  (interactive "sProject name: ")
  (org-toggle-tag "track")
  (org-set-property "Project" project)
  )

(use-package evil-org
  :straight t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  )

(general-def
  :states '(normal motion)
  :keymaps 'org-mode-map
  "t" 'org-todo
  "ga" 'org-archive-subtree-default
  "gr" 'org-refile
  "gR" 'org-refile-goto-last-stored
  "C-j" 'org-metadown
  "C-k" 'org-metaup
  "C-h" 'org-metaleft
  "C-l" 'org-metaright
  "C-J" 'org-shiftmetadown
  "C-K" 'org-shiftmetaup
  "C-H" 'org-shiftmetaleft
  "C-L" 'org-shiftmetaright
  )

(general-def
  :states '(normal motion)
  :keymaps 'org-agenda-mode-map
  "ga" 'org-agenda-archive
  )

(coba-local-leader-def
  :keymaps 'org-agenda-mode-map
  "s" 'org-agenda-schedule
  "d" 'org-agenda-deadline        
  )
(general-def
  :states '(normal motion)
  :keymaps 'org-ql-view-map
  "ga" 'org-agenda-archive
  )

(coba-local-leader-def
  :keymaps 'org-mode-map
  "s" 'org-schedule
  "d" 'org-deadline        
  "t" 'counsel-org-tag
  "p" 'coba-org-create-project
  )

(use-package org-superstar
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  )

;; Babel

                                        ;(org-babel-do-load-languages
                                        ; 'org-babel-load-languages
                                        ; '((R . t)
                                        ;   ))
                                        ;(setq
                                        ; org-confirm-babel-evaluate nil
                                        ; )
                                        ;(setf
                                        ; org-babel-default-header-args:R '((:output . "results"))
                                        ; )  
;; Company

(use-package company
  :straight t
  :config
  (global-company-mode)

  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        )

  (general-def 'company-active-map
    "C-j" 'company-select-next
    "C-k" 'company-select-previous
    "C-s" 'company-search-candidates
    )
  (use-package company-quickhelp
    :straight t
    :config
    (company-quickhelp-mode))
  (use-package company-prescient
    :straight t
    :config
    (company-prescient-mode)
    )
  )

;; Indent 
(use-package aggressive-indent
  :straight t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  )

;; Reference management

(use-package ivy-bibtex
  :straight t
  :config
  (setq
   bibtex-completion-bibliography "~/Documentos/Psicología/bib/bib.bib"
   bibtex-completion-library-path "~/Documentos/Psicología/bib/pdf"
   bibtex-completion-notes-path   "~/Documentos/Psicología/bib/bibnotes.org"
   bibtex-completion-pdf-open-function (lambda (fpath)
                                         (call-process "zathura" nil 0 nil fpath))
   )
  :general
  (coba-leader-def
    "pp" 'ivy-bibtex
    "pd" 'doi-add-bibtex-entry
    "pf" '(lambda() (interactive)(find-file "~/Documentos/Psicología/bib/bib.bib"))
    "pn" '(lambda() (interactive)(find-file "~/Documentos/Psicología/bib/bibnotes.org"))
    )
  )

(use-package org-ref
  :straight t
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq reftex-default-bibliography "~/Documentos/Psicología/bib/bib.bib"
        org-ref-bibliography-notes "~/Documentos/Psicología/bib/bibnotes.org"
        org-ref-pdf-directory "~/Documentos/Psicología/bib/pdf"
        org-ref-open-pdf-function (lambda (fpath)
                                    (call-process "zathura" nil 0 nil fpath))
        )
  (use-package doi-utils
    :config
    (setq bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator ""
          bibtex-autokey-year-title-separator ""
          bibtex-autokey-titleword-separator ""
          bibtex-autokey-titlewords 2
          bibtex-autokey-titlewords-stretch 1
          bibtex-autokey-titleword-length 5)
    )
  (use-package org-ref-isbn)
  (use-package org-ref-sci-id)
  (use-package org-ref-url-utils)
  (use-package org-ref-latex)
  (use-package org-ref-pdf)
  )

;; TODO : pdf

;; TODO: Magit - Magit-todos

(use-package magit
  :straight t
  :after evil
  :config
  (coba-leader-def
    "g" '(lambda() (interactive)(coba-magit-status))
    )
  )

(defun coba-magit-status ()
  (magit-status)
  (delete-other-windows)
  )

(use-package evil-magit
  :straight t
  :after magit)

(use-package forge
  :straight t
  :after magit)

;; Yasnippets

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode))

;; TODO: Pamparam repetition

;; Spellchecking

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package auto-dictionary
  :straight t
  :config
  (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))
  (coba-leader-def
    "zz" 'adict-change-dictionary)
  )
;; TODO: Email

;; TODO: IRC
                                        ; Languages
(use-package systemd
  :straight t)

;; Lisp
(coba-local-leader-def
  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map )
  "r" 'eval-region
  "b" 'eval-buffer)

;; ESS

(use-package ess
  :straight t
  :config
  (setq
   ess-ask-for-ess-directory nil
   comint-move-point-for-output 'others
   comint-scroll-to-bottom-on-input 'this
   ess-eval-visibly 'nowait
   ess-style 'RStudio
   )

  (general-def
    :keymaps '(comint-mode-map)
    "C-k" 'comint-previous-input
    "C-j" 'comint-next-input
    "C-h" 'comint-previous-matching-input-from-input
    "C-," 'ess-insert-assign)

  (general-def
    :states '(insert)
    :keymaps 'ess-mode-map
    "C-," 'ess-insert-assign
    )
  (coba-local-leader-def
    :keymaps 'ess-mode-map
    "b" 'ess-eval-buffer
    )
  )

;; Markdown

(use-package markdown-mode
  :straight t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
                                        ;("\\.Rmd\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   )
  )

;; Polymode

(use-package polymode
  :straight t)

(use-package poly-R
  :straight t)

(use-package poly-markdown
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown+r-mode)))


;; Latex
(use-package tex
  :straight auctex
  )
