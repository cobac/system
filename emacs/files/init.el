(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Remove scroll, tool and menu bars

(scroll-bar-mode -1)

(tool-bar-mode -1)

(menu-bar-mode -1)

(recentf-mode 1)

(setq recentf-max-saved-items 100)

(run-at-time nil (* 2 60) 'recentf-save-list)

;; Remove startup screen
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Show matching paranthesis
(show-paren-mode 1)

(electric-pair-mode)

;; Always indent with spaces
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Remove trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

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

(setq delete-old-versions -1)

;; delete excess backup versions silently
(setq version-control t)

;; use version control
(setq vc-make-backup-files t)

;; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

;; don't ask for confirmation when opening symlinked file
(setq vc-follow-symlinks t)

;; which directory to put backups file
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save-list/" t)))

;;transform backups file name
(setq inhibit-startup-screen t)

;; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore)

;; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)

;; use utf-8 by default
(setq coding-system-for-write 'utf-8)

;; sentence SHOULD end with only a point.
(setq sentence-end-double-space nil)

;; print a default message in the empty scratch buffer opened at startup
(setq initial-scratch-message nil)

(setq delete-by-moving-to-trash t)

(setq auth-sources '("~/.authinfo.gpg"))

(put #'downcase-region 'disabled nil)

(put #'upcase-region 'disabled nil)

(put #'narrow-to-region 'disabled nil)

(when (eq system-type 'darwin)
  (load "~/.emacs.d/emulate-mac-keyboard-mode.el")
  (emulate-mac-spanish-keyboard-mode)
  (setq mac-command-modifier 'control))

(defun coba-sync-init-with-system ()
  "Sync `init.el`, `ox-templates` and `snippets` from `~/.emacs.d` into `~/Documentos/system/emacs/files`."
  (interactive)
  (shell-command
   "cp $HOME/Documentos/system/emacs/files/init.el $HOME/.emacs.d/")
  (shell-command
   "cp $HOME/Documentos/system/emacs/files/big_init.el $HOME/.emacs.d/")
  (shell-command
   "cp $HOME/Documentos/system/emacs/files/early-init.el $HOME/.emacs.d/")
  (shell-command
   "cp -r $HOME/Documentos/system/emacs/files/ox-templates $HOME/.emacs.d/")
  (shell-command
   "cp -r $HOME/.emacs.d/snippets $HOME/Documentos/system/emacs/files/"))

                                        ; Packages

;; Get correct path
(use-package exec-path-from-shell
  :straight t
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;; Visual
(use-package doom-themes
  :straight t
  :config
  (doom-themes-org-config)
  (setq doom-gruvbox-dark-variant "hard")
  (load-theme 'doom-gruvbox t))

(use-package memoize
  :straight t)

(use-package nerd-icons
  :straight t
  :after memoize)

(use-package mood-line
  :straight t
  :hook (after-init . mood-line-mode))

;; General - which-key
(use-package which-key
  :straight t
  :config (which-key-mode 1))

(use-package general
  :after which-key
  :straight t
  :config
  (setq general-override-states
        '(insert emacs hybrid normal visual motion operator replace))
  (general-create-definer
    coba-leader-def
    :states '(normal visual insert motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (general-create-definer
    coba-local-leader-def
    :states '(normal visual motion)
    :prefix ",")
  (general-def
    "s-+" 'text-scale-increase "s--" 'text-scale-decrease "s-0"
    '(lambda ()
       (interactive)
       (text-scale-adjust 0))))

;; Evil
(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil ;; for evil-collection
        evil-respect-visual-line-mode t)
  :general
  (general-def
    :states
    '(normal motion)
    "ñ"
    'consult-yank-pop
    "gt"
    'undo-tree-visualize
    "gA"
    'align-regexp
    "gc"
    'comment-dwim
    "gd"
    'xref-find-definitions
    "gD"
    'eldoc
    "gi"
    'consult-imenu
    "C-j"
    '(lambda ()
       (interactive)
       (evil-scroll-down 0)
       (recenter nil))
    "C-k"
    '(lambda ()
       (interactive)
       (evil-scroll-up 0)
       (recenter nil)))
  (general-def :states '(visual) "C-=" 'count-words-region)
  :config
  (setq evil-want-minibuffer t
        evil-want-C-d-scroll nil)
  (use-package undo-tree
    :straight t
    :config
    (setq undo-tree-visualizer-diff t
          undo-tree-auto-save-history nil))
  (custom-set-variables '(evil-undo-system 'undo-tree))
  (evil-mode)
  (global-undo-tree-mode)
  (use-package evil-anzu
    :straight t)
  (use-package evil-collection
    :straight (:type git
                     :host github
                     :repo "emacs-evil/evil-collection") ;:branch "retain-selection")
    :config
    (evil-collection-init
     '(dired
       docker
       ediff
       elfeed
       image
       info
       magit
       magit-todos
       mu4e
       nov
       org-present
       profiler
       smerge-mode
       tar-mode
       vterm
       xref)))
  (use-package evil-snipe
    :straight t
    :config (evil-snipe-mode 1)
    (evil-snipe-override-mode 1)
    (setq evil-snipe-smart-case t
          evil-snipe-show-prompt nil
          evil-snipe-auto-scroll nil
          evil-snipe-scope 'whole-buffer
          evil-snipe-repeat-scope 'whole-buffer)
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))
  (use-package evil-surround
    :straight t
    :after evil
    :config (global-evil-surround-mode)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(use-package evil-mc
  :straight t
  :after evil)

(use-package better-jumper
  :straight t
  :after evil
  :config (better-jumper-mode +1)
  (general-def
    :states
    'motion
    "C-o"
    'better-jumper-jump-backward
    "C-i"
    'better-jumper-jump-forward))

;; Vertico, Consult and company

(use-package vertico
  :straight t
  :after evil-collection
  :init (vertico-mode)
  :config
  (general-def
    :keymaps 'vertico-map
    :states
    '(motion insert)
    "C-k"
    'vertico-previous
    "C-j"
    'vertico-next
    "C-S-k"
    'vertico-previous-group
    "C-S-j"
    'vertico-next-group
    "C-l"
    'vertico-insert)
  (general-def
    :keymaps 'vertico-map
    :states
    '(normal)
    "<escape>"
    'minibuffer-keyboard-quit
    "gg"
    'vertico-first
    "G"
    'vertico-last))

(use-package vertico-posframe
  :straight t
  :config (vertico-posframe-mode))

(use-package consult
  :straight t)

(use-package marginalia
  :straight t
  :config (marginalia-mode))

(use-package embark
  :straight t
  :after consult
  :bind (("C-." . embark-act)
         ("C-)" . embark-dwim))
  :config
  ;; C-d: kill-buffer during switch-to-buffer
  ;; https://www.reddit.com/r/emacs/comments/16g08me/killbuffer_from_the_minibuffer_after_mx/
  ;; no confirmation
  (setf (alist-get 'kill-buffer embark-pre-action-hooks)
        ())
  ;; ;; don't embark--restart
  (setf (alist-get 'kill-buffer embark-post-action-hooks)
        ())
  ;; ;; don't quit
  (setq embark-quit-after-action '((kill-buffer . nil) (t . t)))
  (general-def
    :keymaps 'minibuffer-local-map
    :states '(insert normal)
    "C-d" (general-simulate-key "C-. k C-j"))
  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets)
                                                       :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t
         (lambda (binding)
           (not (string-suffix-p "-argument" (cdr binding))))))))
  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package embark-consult
  :straight t
  :config)

(use-package orderless
  :straight t
  :custom (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(coba-leader-def
  "f"
  '(:ignore t
            :which-key "Files")
  "fi"
  '(lambda ()
     (interactive)
     (find-file "~/Documentos/system/emacs/files/init.el"))
  "fs"
  '(lambda ()
     (interactive)
     (ido-find-file-in-dir "~/Documentos/system/"))
  "fp"
  '(lambda ()
     (interactive)
     (ido-find-file-in-dir "~/Documentos/"))
  "fo"
  '(lambda ()
     (interactive)
     (find-file "~/Sync/oros/main.ledger"))
  "ff"
  'find-file
  "fr"
  'recentf
  "FF"
  'project-find-file
  ;; "fz" 'counsel-fzf
  "SPC"
  'switch-to-buffer
  "x"
  'execute-extended-command
  "/"
  'consult-ripgrep
  "T"
  'consult-theme
  "!"
  'async-shell-command
  "¡"
  'shell-command
  ":" 'eval-expression)

(use-package prescient
  :straight t
  :config (prescient-persist-mode))

(use-package vertico-prescient
  :straight t
  :config (vertico-prescient-mode))

(use-package wgrep
  :straight t
  :hook (wgrep-setup . evil-normal-state))

(use-package transient-posframe
  :straight t
  :config (transient-posframe-mode t))

;; Project
(defun coba-project-root-override (dir)
  "Find DIR's project root by searching for a '.project.el' file.

If this file exists, it marks the project root. For convenient compatibility
with Projectile, '.projectile' is also considered a project root marker.

https://blog.jmthornton.net/p/emacs-project-override"
  (let ((root
         (or (locate-dominating-file dir ".project.el")
             (locate-dominating-file dir ".projectile")))
        (backend
         (ignore-errors
           (vc-responsible-backend dir))))
    (when root
      (if (version<= emacs-version "28")
          (cons 'vc root)
        (list 'vc backend root)))))

(use-package project
  :straight t
  :config
  (add-hook 'project-find-functions #'coba-project-root-override))

;; Hydra
(use-package hydra
  :straight t)

(defhydra coba-hydra-windows ()
  "Manage window movement with evil-window funcions."
  ("h" evil-window-left)
  ("j" evil-window-down)
  ("k" evil-window-up)
  ("l" evil-window-right)
  ("C-h" shrink-window-horizontally)
  ("C-j" shrink-window)
  ("C-k" enlarge-window)
  ("C-l" enlarge-window-horizontally)
  ("s" split-window-horizontally)
  ("d" delete-window))

(coba-leader-def
  "w"
  'coba-hydra-windows/body
  "dd"
  'evil-delete-buffer
  "s"
  '(:ignore t
            :which-key "Split")
  "sd"
  'delete-other-windows
  "sh"
  'split-window-below
  "ss"
  'split-window-horizontally
  "sr"
  'evil-window-rotate-downwards
  "l"
  'evil-window-next
  "bs"
  'save-some-buffers)

;; Openwith

(use-package openwith
  :straight (:type git
                   :host github
                   :repo "cobac/openwith")
  :config
  (setq openwith-associations
        (list
         (list
          (openwith-make-extension-regexp
           '("mpg"
             "mpeg"
             "mp3"
             "mp4"
             "avi"
             "wmv"
             "wav"
             "mov"
             "flv"
             "ogm"
             "ogg"
             "mkv"))
          "vlc" '(file))
         (list
          (openwith-make-extension-regexp
           '("doc" "docx" "xls" "ppt" "pptx" "odt" "ods" "odg" "odp"))
          "libreoffice" '(file))
         (list
          (openwith-make-extension-regexp
           '("pdf" "ps" "ps.gz" "dvi"))
          "zathura" '(file))
         (list
          (openwith-make-extension-regexp
           '("png" "jpg" "jpeg" "webp"))
          "feh" '(file))))
  (openwith-mode 1))

;; Numbers stuff TODO: check visual selection
;; I tried :(
(use-package evil-numbers
  :straight (:type git
                   :host github
                   :repo "janpath/evil-numbers") ;:branch "retain-selection")
  :after evil
  :config
  (general-def
    :states
    '(normal motion visual)
    "C-a"
    'evil-numbers/inc-at-pt
    "C-x"
    'evil-numbers/dec-at-pt))

;; Org

(use-package org
  :straight t
  :config
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (setq org-agenda-files
        '("~/Sync/Org/todo.org"
          "~/Sync/Org/refile.org"
          "~/Sync/Org/annuals.org")
        org-agenda-span 7
        org-agenda-start-on-weekday 1
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-log-done (quote time)
        org-log-readline (quote time)
        org-log-reschedule (quote time)
        org-refile-allow-creating-parent-nodes 'confirm
        org-archive-location "~/Sync/Org/archive.org::* From ??"
        org-deadline-warning-days 14
        org-refile-use-outline-path t
        org-extend-today-until 4
        org-use-property-inheritance t
        calendar-date-style 'european
        org-outline-path-complete-in-steps nil
        calendar-week-start-day 1
        org-default-notes-file "~/Sync/Org/refile.org"
        org-capture-templates
        '(("t"
           "Todo"
           entry
           (file "~/Sync/Org/refile.org")
           "* TODO %?"
           :empty-lines 1)
          ("l"
           "Link"
           entry
           (file "~/Sync/Org/refile.org")
           "* TODO [%?[][]]\n:PROPERTIES:\n:CREATED: %U\n:END:"
           :empty-lines 1)
          ("f"
           "File link"
           entry
           (file "~/Sync/Org/refile.org")
           "* TODO %A\n:PROPERTIES:\n:CREATED: %U\n:END:"
           :empty-lines 1)
          ("c"
           "Check Computer"
           entry
           (file+olp "~/Sync/Org/todo.org" "Computer" "Check")
           "* TODO [%?[][]]\n:PROPERTIES:\n:CREATED: %U\n:END:"
           :empty-lines 1)
          ("p"
           "Check Psychology"
           entry
           (file+olp "~/Sync/Org/todo.org" "Psychology" "Check")
           "* TODO [%?[][]]\n:PROPERTIES:\n:CREATED: %U\n:END:"
           :empty-lines 1)
          ("m"
           "Movies"
           entry
           (file+olp "~/Sync/Org/todo.org" "Leisure" "Movies")
           "* TODO %?"
           :empty-lines 1)
          ("n"
           "Movies waiting"
           entry
           (file+olp "~/Sync/Org/todo.org" "Leisure" "Movies" "Waiting")
           "* TODO %?"
           :empty-lines 1)
          ("s"
           "Series"
           entry
           (file+olp "~/Sync/Org/todo.org" "Leisure" "Series")
           "* TODO %?"
           :empty-lines 1)
          ("w"
           "Series waiting"
           entry
           (file+olp "~/Sync/Org/todo.org" "Leisure" "Series" "Waiting")
           "* TODO %?"
           :empty-lines 1)
          ("b"
           "Books"
           entry
           (file+olp "~/Sync/Org/todo.org" "Leisure" "Books")
           "* TODO %?"
           :empty-lines 1)
          ("o"
           "Otro"
           entry
           (file+olp "~/Sync/Org/todo.org" "Other")
           "* TODO %?"
           :empty-lines 1))
        org-refile-targets
        (quote (("~/Sync/Org/todo.org" :maxlevel . 10)
                ("~/Sync/Org/annuals.org" :maxlevel . 10)))
        org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED"))
        org-agenda-custom-commands
        '(("w"
           "Weekly view"
           ((org-ql-block '(and (todo "WAITING"))) (agenda)))))
  (general-define-key
   :keymaps
   'org-capture-mode-map
   [remap evil-save-and-close]
   'org-capture-finalize
   [remap evil-save-modified-and-close]
   'org-capture-finalize
   [remap evil-quit]
   'org-capture-kill)
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats
        '("<%Y-%m-%e, %a>" . "<%Y-%m-%e, %a %H:%M>"))
  (coba-leader-def
    "c" 'org-capture
    "a" '(:ignore t
                  :which-key "Org-Agenda") "aa"
    '(lambda ()
       (interactive)
       (coba-org-agenda-weekly))
    "o"
    '(lambda ()
       (interactive)
       (find-file "~/Sync/Org/todo.org"))
    "R"
    '(lambda ()
       (interactive)
       (find-file "~/Sync/Org/refile.org"))
    "ad"
    '(lambda ()
       (interactive)
       (org-ql-search
         "~/Sync/Org/todo.org"
         '(done)
         :sort '(date priority todo)))
    "ap"
    '(lambda ()
       (interactive)
       (org-ql-search
         (org-agenda-files)
         '(and (tags "track")
               (or (todo "TODO")
                   (todo "WAITING")))
         :title "Projects"
         :sort '(date priority todo)
         :super-groups '((:auto-property "Project")))
       (delete-other-windows)))
  (defun coba-org-agenda-weekly ()
    "Helper to open Agenda in weekly view by default."
    (org-agenda nil "w")
    (delete-other-windows))
  (general-def
    :states '(normal motion)
    :keymaps
    'org-mode-map
    "C-t"
    'org-todo
    "C-S-T"
    'coba-org-todo-yesterday-twice
    "ga"
    'org-archive-subtree-default
    "gr"
    'org-refile
    "gR"
    'org-refile-goto-last-stored
    "gG"
    'consult-outline
    "C-P"
    'org-latex-preview)
  (general-def
    :states '(normal motion)
    :keymaps
    'org-agenda-mode-map
    "ga"
    'org-agenda-archive
    "C-t"
    'org-agenda-todo
    "C-S-T"
    'coba-org-agenda-todo-yesterday-twice)
  (coba-local-leader-def
    :keymaps
    'org-agenda-mode-map
    "s"
    'org-agenda-schedule
    "d"
    'org-agenda-deadline)
  (defun coba-org-todo-yesterday-twice ()
    "Call `org-todo-yesterday` twice."
    (interactive)
    (org-todo-yesterday)
    (org-todo-yesterday))
  (defun coba-org-agenda-todo-yesterday-twice ()
    "Call `org-todo-yesterday` twice."
    (interactive)
    (org-agenda-todo-yesterday)
    (org-agenda-todo-yesterday))
  (general-def
    :states '(normal motion)
    :keymaps
    'org-ql-view-map
    "ga"
    'org-agenda-archive)
  (coba-local-leader-def
    :keymaps 'org-mode-map
    :states
    '(normal motion)
    "s" 'org-schedule
    "d" 'org-deadline
    "t" 'org-set-tag-command)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook (lambda ()
                             (electric-indent-local-mode -1)))
  (org-babel-do-load-languages
   'org-babel-load-languages '((R . t)
                               (dot . t)))
  (setq org-confirm-babel-evaluate nil)
  (setf org-babel-default-header-args:R '((:output . "results")))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.85)))

(use-package org-present
  :after org
  :straight t)

(use-package evil-org
  :straight t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda ()
                                  (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-super-agenda
  :straight (:host github
                   :repo "alphapapa/org-super-agenda")
  :after org
  :general
  (general-def
    'org-super-agenda-header-map
    "j"
    'org-agenda-next-item
    "k"
    'org-agenda-previous-item)
  :config
  (setq org-super-agenda-groups
        '(
          ;;(:discard(:and(:category "Annuals" :scheduled past)))
          (:name "Today's deadlines" :deadline today :order 1)
          (:name "Overdue" :deadline past :order 0)
          (:name "Habits" :habit t :order 30)
          (:name
           "Today's Annuals"
           :and (:category "Annuals" :scheduled today)
           :order 3)
          (:name
           "Annuals"
           :and (:category "Annuals" :scheduled future)
           :order 80)
          (:name "Schedule" :time-grid t :order 2)
          (:name "Scheduled" :scheduled today :order 4)
          (:name
           "University Deadlines"
           :and (:deadline future :tag "uni")
           :order 10)
          (:name "Other Deadlines" :deadline future :order 11)
          (:name "Refile" :scheduled past :order 99)))
  (org-super-agenda-mode))

(use-package transient
  :straight t)

(use-package org-ql
  :straight (:host github
                   :repo "alphapapa/org-ql")
  :after org)

(use-package org-modern
  :straight t
  :config
  (with-eval-after-load 'org
    (global-org-modern-mode))
  (setq org-modern-keyword nil
        org-modern-todo nil
        org-modern-tag nil
        org-modern-star 'replace
        org-modern-block-name nil))

(use-package org-download
  :straight t
  :after general
  :config
  (coba-local-leader-def 'org-mode-map "p" 'org-download-clipboard)
  (setq-default org-download-image-dir "~/Sync/Brain/pictures"
                org-download-heading-lvl nil))

(use-package org-roam
  :after org
  :straight (:host
             github
             :repo "org-roam/org-roam"
             :branch "main"
             :files ("*.el" "out" "extensions/*.el"))
  :general
  (coba-leader-def
    "r" 'org-roam-node-find "fd" 'org-roam-dailies-goto-today)
  (general-def :keymaps 'org-mode-map "C-i" 'org-roam-node-insert)
  (coba-local-leader-def
    :keymaps
    'org-mode-map
    ","
    'org-roam-buffer-toggle
    "i"
    'org-id-get-create)
  (general-unbind
    :keymaps 'org-roam-mode-map
    :states
    '(normal visual motion)
    "SPC")
  (general-def
    :keymaps 'org-roam-mode-map
    :states
    '(normal visual motion)
    "q"
    'evil-delete-buffer)
  :init (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory (file-truename "~/Sync/Brain")
        org-roam-db-location "~/Sync/Brain/roam.db"
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        org-roam-dailies-directory "diario"
        org-roam-dailies-capture-templates
        '(("d"
           "default"
           plain
           "%?"
           :target (file+head "%<%Y-%m>.org" "* %<%d %A>\n\n")))
        org-roam-capture-templates
        '(("r" "default" plain "- tags :: %?"
           :target
           (file+head
            "${slug}.org"
            ":PROPERTIES:\n:ROAM_ALIASES:\n:ROAM_REFS:\n:END:\n#+STARTUP: latexpreview\n#+filetags:\n#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("w" "motherduck" plain "%?"
           :target
           (file+head
            "motherduck/${slug}.org"
            ":PROPERTIES:\n:ROAM_ALIASES:\n:ROAM_REFS:\n:END:\n#+STARTUP: latexpreview\n#+filetags: motherduck\n#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("p" "bib" plain "- tags :: %?"
           :target
           (file+head
            "${citekey}.org"
            ":PROPERTIES:\n:ROAM_ALIASES: \"${author-abbrev}(${year}): ${title}\"\n:END:\n#+STARTUP: latexpreview\n#+filetags:\n#+title: ${citekey}\n")
           :immediate-finish t
           :unnarrowed t)))
  (evil-set-initial-state 'org-roam-mode 'motion)
  ;; Show hierarchy of nodes from https://github.com/org-roam/org-roam/issues/1565
  ;; (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
  ;;   "Return the file TITLE for the node."
  ;;   (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
  ;; (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
  ;;   "Return the hierarchy for the node."
  ;;         (olp (org-roam-node-olp node))
  ;;         (level (org-roam-node-level node))
  ;;         (filetitle (org-roam-node-filetitle node)))
  ;;     (concat
  ;;      title
  ;;      (if (> level 0)
  ;;          (concat ", from " filetitle))
  ;;      (if (> level 1)
  ;;          (concat " -> " (string-join olp " -> "))))))
  ;;(setq org-roam-node-display-template "${hierarchy:*} ${tags:20}")
  (org-roam-db-autosync-enable))

(use-package org-roam-ui
  :after org-roam
  :straight (:host
             github
             :repo "org-roam/org-roam-ui"
             :branch "main"
             :files ("*.el" "out"))
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow nil
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

;; Company

(use-package company
  :straight t
  :config (global-company-mode)
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t)
  (setq default-company-backends
        '(company-semantic
          company-gtags
          company-files
          company-keywords
          company-capf
          company-yasnippet
          company-abbrev
          company-dabbrev
          company-dabbrev-code)
        company-backends (list default-company-backends))
  (general-def
    'company-active-map
    "C-j"
    'company-select-next
    "C-k"
    'company-select-previous
    "C-s"
    'company-search-candidates))

(use-package company-quickhelp
  :straight t
  :after company
  :config (company-quickhelp-mode))

(use-package company-prescient
  :straight t
  :after company
  :config (company-prescient-mode))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

;; Indent
(use-package aggressive-indent
  :straight t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;; Reference management

(use-package citar
  :straight t
  :after org-roam
  :hook (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :config
  (coba-leader-def
    "pp" 'citar-open "pf"
    '(lambda ()
       (interactive)
       (find-file "~/Sync/Brain/bib.bib")))
  (setq citar-bibliography '("~/Sync/Brain/bib.bib")
        citar-notes-paths '("~/Sync/Brain/")
        citar-library-paths '("~/Sync/Brain/pdf")))

(use-package citar-org-roam
  :straight t
  :after citar
  :config
  (setq citar-org-roam-note-capture-key "p")
  (citar-org-roam-mode))

;; (use-package citar-embark
;;   :after citar embark
;;   :no-require
;;   :config (citar-embark-mode))

(defun coba-file-content-as-string (filename)
  "Return the contents of FILENAME as string.
    https://gist.github.com/bigodel/56a4627afdfe9ad28f6dcc68b89a97f8"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(use-package graphviz-dot-mode
  :straight t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package dumb-jump
  :straight t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Git
(use-package magit
  :straight t
  :after evil
  :config
  (coba-leader-def
    "g"
    '(lambda ()
       (interactive)
       (coba-magit-status)))
  (general-def
    :keymaps
    'git-rebase-mode-map
    "C-j"
    'git-rebase-move-line-down
    "C-k"
    'git-rebase-move-line-up)
  (defun coba-magit-push-all ()
    "Push to all remotes.
  From https://www.reddit.com/r/emacs/comments/ja97xs/weekly_tipstricketc_thread/?utm_medium=android_app&utm_source=share"
    (interactive)
    (mapcar
     (lambda
       (remote) ;; Loops through the remotes returned by magit-list-remotes
       (magit-run-git-async
        "push" "-v" remote (magit-get-current-branch))) ;; Simply run git push -v {{remote}} {{current-branch}}
     (magit-list-remotes)) ;; Returns all remotes configured
    )
  (defun coba-magit-diff-to-main ()
    "Diff HEAD to main"
    (interactive)
    (magit-diff-range (concat (magit-main-branch) "..HEAD")))
  (transient-append-suffix
    'magit-push
    "e" ;; after `e` key
    '("a" "Push all" coba-magit-push-all))
  (transient-append-suffix
    'magit-diff
    "d" ;; after `e` key
    '("D" "Diff to main" coba-magit-diff-to-main)))

(evil-set-initial-state 'magit-commit-message-section-map 'insert)

(defun coba-magit-status ()
  "Open magit-status in full screen."
  (magit-status)
  (delete-other-windows))

(use-package forge
  :straight t
  :after magit)

(use-package magit-todos
  :straight t
  :config
  (global-hl-todo-mode 1)
  (general-def 'magit-mode-map
    "C-S-t" 'magit-todos-list))

(use-package git-timemachine
  :straight t
  :config
  ;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
  ;; Unavailable link
  (with-eval-after-load 'git-timemachine
    (evil-make-overriding-map git-timemachine-mode-map 'normal)
    ;; force update evil keymaps after git-timemachine-mode loaded
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

;; Flyckeck
(use-package flycheck
  :straight t
  :config
  (general-def
    :states
    '(normal motion)
    "gB" 'flycheck-list-errors
    "gJ" 'flycheck-next-error
    "gK" 'flycheck-previous-error)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-eglot
  :straight t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package eglot
  :straight (:type built-in)
  :config
  (general-def
    :states
    '(normal motion)
    "gr" 'eglot-rename
    "C-l" 'eglot-code-actions))

(use-package eglot-booster
  :straight (eglot-booster
             :type git
             :host github
             :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

;; formatters
(use-package format-all
  :straight t
  :after apheleia
  :general
  (general-def
    'ess-r-mode-map
    :states '(normal motion) "gf" 'format-all-buffer))

;; Yasnippets

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand))

;; TODO: Pamparam repetition, or anki-el, or org-drill

;; Spellchecking

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(add-hook 'text-mode-hook 'flyspell-mode)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package auto-dictionary
  :straight t
  :config
  (add-hook
   'flyspell-mode-hook (lambda ()
                         (auto-dictionary-mode 1)))
  (coba-leader-def "zz" 'adict-change-dictionary))

(use-package elfeed
  :straight t
  :general
  (coba-leader-def
    "E"
    '(lambda ()
       (interactive)
       (elfeed)
       (elfeed-update)))
  :config
  (setq elfeed-feeds '(("https://archlinux.org/feeds/news/" Arch))))

;; TODO: IRC

;; Dired

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :config)

(use-package dired-subtree
  :straight t
  :after dired)

(use-package dired-filter
  :straight t)

;; Olivetti
(use-package olivetti
  :straight t
  :after polymode
  :config (coba-leader-def "W" 'olivetti-mode)
  ;; WAITING: Remove if github.com/polymode/polymode/pull/340 get's merged
  (add-to-list
   'polymode-move-these-minor-modes-from-old-buffer 'olivetti-mode)
  (add-to-list
   'polymode-move-these-vars-from-old-buffer 'olivetti-body-width)
  (add-hook 'olivetti-mode-on-hook #'visual-line-mode))

;; Libvterm
(use-package vterm
  :straight (:type git
                   :host github
                   :repo "akermu/emacs-libvterm"))

(use-package multi-vterm
  :straight t
  :general (coba-leader-def "RET" 'multi-vterm))

(use-package lsp-mode
  :straight t
  :init (setq lsp-keymap-prefix "C-l")
  ;; Optimization stuff
  (setq gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024)
        lsp-completion-provider
        :capf
        lsp-idle-delay 0.500)
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :general
  (general-def 'lsp-browser-mode-map "q" 'quit-window)
  (general-def 'lsp-mode-map "C-l i" 'lsp-ivy-workspace-symbol)
  :config
  (evil-set-initial-state 'lsp-browser-mode 'motion)
  (setq lsp-headerline-breadcrumb-segments '(symbols)
        lsp-log-io t
        lsp-print-performance t
        lsp-modeline-code-actions-segments '(count name)
        lsp-lens-enable nil))

;; ==========================
;; From https://github.com/blahgeek/emacs-lsp-booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add (if (progn
                  (require 'json)
                  (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?) ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection)) ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path
                      (executable-find
                       (car
                        orig-result)))) ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command :around
            #'lsp-booster--advice-final-command)

;; ==========================
;; end of copied snippet

;; gpg passwords

(defun coba-lookup-password (&rest keys)
  "Helper to get KEYS from emacsclient."
  (when-let ((result (apply #'auth-source-search keys)))
    (funcall (plist-get (car result) :secret))))

;; Languages


;; Systemd
(use-package systemd
  :straight t)

;; Lisp
(coba-local-leader-def
  :keymaps
  '(emacs-lisp-mode-map
    lisp-interaction-mode-map)
  "," 'eval-last-sexp "r" 'eval-region "b" 'eval-buffer)

(use-package prettier-elisp
  :straight (prettier-elisp
             :type git
             :host github
             :repo "KarimAziev/prettier-elisp"))

(use-package eterm-256color
  :straight t
  :config (add-hook 'term-mode-hook #'eterm-256color-mode))

;; Markdown
(use-package markdown-mode
  :straight t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
                                        ;("\\.Rmd\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :config
  (general-def 'markdown-view-mode-map "q" 'quit-window)
  (setq markdown-hide-markup nil))

;; Polymode
(use-package polymode
  :straight t)

(use-package poly-org
  :straight (:type git
                   :host github
                   :repo "polymode/poly-org"))

(use-package quarto-mode
  :straight t)

;; Python

;; (use-package elpy
;;   :straight t
;;   :init
;;   (elpy-enable)
;;   )

;;(use-package python-mode
;;  :straight t
;;  :hook (python-mode . lsp)
;;  :config
;;  (setq py-keep-windows-configuration t)
;;  (general-def 'python-mode-map
;;    "C-c C-c" 'py-execute-region
;;    "C-c C-l" 'py-execute-line
;;    "C-c C-b" 'py-execute-buffer)
;;  )

(use-package python
  :straight t
  :hook (python-mode . lsp)
  :config
  (general-def
    'python-mode-map
    "C-c C-c"
    'python-shell-send-region
    "C-c C-b"
    'python-shell-send-buffer))

(use-package pyvenv
  :straight t
  :config
  (general-def 'python-mode-map "C-c C-a" 'pyvenv-workon)
  (pyvenv-mode 1))

(use-package lsp-pyright
  :straight t)

;; Notebooks

(use-package ein
  :straight t
  :config (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode))

;; Yaml
(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; Make
(add-hook
 'makefile-mode-hook
 (lambda ()
   (add-hook
    'before-save-hook (lambda ()
                        (tabify (point-min)
                                (point-max)))
    nil 'make-it-local)))

;; (setq compile-command
;;       '(concat
;;         "cd "
;;         (file-name-directory
;;          (locate-dominating-file (buffer-file-name) "Makefile"))
;;         " && make "))

(coba-leader-def "C" 'project-compile)

(setq compilation-scroll-output 'first-error)

(defun coba-colorize-compilation-buffer ()
  "From https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode ."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region
     compilation-filter-start (point-max))))

(add-hook 'compilation-filter-hook 'coba-colorize-compilation-buffer)

(use-package docker
  :straight t
  :general (coba-leader-def "D" 'docker)
  :config (setq docker-run-as-root t))

(use-package dockerfile-mode
  :straight t)

(use-package powerthesaurus
  :straight t
  :general
  (coba-leader-def "it" 'powerthesaurus-lookup-synonyms-dwim))

(use-package image
  :config
  (general-def
    'image-mode-map
    :states '(normal) "J" 'image-next-file "K" 'image-previous-file))

(use-package nix-mode
  :straight t)

(use-package direnv
  :straight t
  :config (direnv-mode))

(use-package terraform-mode
  :straight t
  :config
  (setq terraform-format-on-save t)
  (general-def
    :keymaps 'terraform-mode-map
    :states '(motion normal)
    "gD" 'terraform-open-doc
    "<tab>" 'terraform-toggle-or-indent)
  (add-hook 'terraform-mode-hook #'outline-minor-mode))

(use-package dbt-mode
  :straight (:type git
                   :host github
                   :repo "CyberShadow/dbt-mode"))

(use-package csv-mode
  :straight t)

(use-package calfw
  :straight (:type git
                   :host github
                   :repo "kiwanami/emacs-calfw"))

(use-package calfw-org
  :straight (:type git
                   :host github
                   :repo "kiwanami/emacs-calfw"))

(when t (load "~/.emacs.d/big_init.el"))
