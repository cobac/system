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
(recentf-mode 1)

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
(setq vc-follow-symlinks t )					;; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t)					;; inhibit useless and old-school startup screen
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

;; Get correct path
(use-package exec-path-from-shell
	:straight t
	:config
	(when (daemonp)
		(exec-path-from-shell-initialize)))

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
	(setq doom-modeline-modal-icon nil
				doom-modeline-enable-word-count t
        doom-modeline-icon t
        mode-line-format '("%e" (:eval (doom-modeline-format--main)) "   ")
	      )
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
	)


;; Evil
(use-package evil
	:straight t
	:init
	(setq evil-want-keybinding nil) ;for evil-collection
	:general
	(general-def
		:states '(normal motion)
		"ñ" 'counsel-yank-pop
		"gt" 'undo-tree-visualize
		)
	(general-def
		:states '(visual)
    "C-=" 'count-words-region
		)
	:config
  (use-package undo-tree
    :straight t
    :config
    (setq undo-tree-visualizer-diff t)
    )
  (custom-set-variables '(evil-undo-system 'undo-tree))
  (evil-mode)
  (global-undo-tree-mode)
	(use-package evil-anzu
		:straight t)
	(use-package evil-collection
    :straight (:type git :host github :repo "emacs-evil/evil-collection");:branch "retain-selection")
		:config
    (evil-collection-init 'mu4e)
		(evil-collection-init 'dired)
		(evil-collection-init 'ediff)
		(evil-collection-init 'magit-todos)
    (evil-collection-init 'vterm)
		(evil-collection-init 'info)
		(evil-collection-init 'lsp-ui-menu)
		(evil-collection-init 'docker)
		;;(evil-collection-init 'calendar)
    (evil-collection-init 'magit)
    (evil-collection-init 'xref)
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
	(use-package evil-exchange
		:straight t
		)
	)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(use-package better-jumper
	:straight t
  :after evil
	:config
	(better-jumper-mode +1)
	(general-def :states 'motion
		"C-o" 'better-jumper-jump-backward
		"C-i" 'better-jumper-jump-forward)
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
		"C-u" 'ivy-dispatching-done
		"C-)" 'ivy-immediate-done
		)
	)

(use-package counsel
	:straight t
	:config
	(general-def 'ivy-switch-buffer-map
		"C-k" 'ivy-previous-line
		"C-d" 'ivy-switch-buffer-kill)
	(coba-leader-def
		"f" '(:ignore t :which-key "Files")
		"fi" '(lambda() (interactive)(find-file "~/.emacs.d/init.el"))
		"fp" '(lambda() (interactive)(counsel-find-file "~/Documentos/Psicología/resma"))
		"ff" 'counsel-find-file
		"fr" 'counsel-recentf
		"FF" 'counsel-fzf
		"SPC" 'counsel-switch-buffer
		"x" 'counsel-M-x
		"/" 'counsel-rg
		"T" 'counsel-load-theme
		)
	(general-def 'counsel-find-file-map
		"C-h" 'counsel-up-directory
		"C-l" 'ivy-alt-done
		)
	)

(use-package prescient
	:straight t
	)
(use-package ivy-prescient
	:straight t
	:after counsel
	:config
	(prescient-persist-mode)
	(ivy-prescient-mode)
	)

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
	("d" delete-window)
	)

(coba-leader-def
	"w" 'coba-hydra-windows/body
	"dd" 'evil-delete-buffer
	"s" '(:ignore t :which-key "Split")
	"sd" 'delete-other-windows
	"sh" 'split-window-below
	"ss" 'split-window-horizontally
	"sr" 'evil-window-rotate-downwards
	"l" 'evil-window-next
	"bs" 'save-some-buffers
	)

;; Openwith

(use-package openwith
	:straight t
	:config
	(setq openwith-associations
				(list
				 (list (openwith-make-extension-regexp
								'("mpg" "mpeg" "mp3" "mp4"
									"avi" "wmv" "wav" "mov" "flv"
									"ogm" "ogg" "mkv"))
							 "vlc"
							 '(file))
				 (list (openwith-make-extension-regexp
								'("doc" "docx" "xls" "ppt" "pptx" "odt" "ods" "odg" "odp"))
							 "libreoffice"
							 '(file))
				 (list (openwith-make-extension-regexp
								'("pdf" "ps" "ps.gz" "dvi"))
							 "zathura"
							 '(file))

				 (list (openwith-make-extension-regexp
								'("png" "jpg" "jpeg" ))
							 "feh"
							 '(file))
				 ))
	(openwith-mode 1)
	)

;; Numbers stuff TODO: check visual selection
;; I tried :(
(use-package evil-numbers
  :straight (:type git :host github :repo "janpath/evil-numbers");:branch "retain-selection")
  :after evil
  :config
  (general-def
    :states '(normal motion visual)
    "C-a" 'evil-numbers/inc-at-pt
    "C-x" 'evil-numbers/dec-at-pt)
  )
;; Can't get to work block without incremental
;;(use-package plus-minus
;;:straight (:host github :repo "peterwu/plus-minus")
;;:general
;;(general-def
;;  :states '(normal motion)
;;  "C-a" '+/-:forward+
;;  "C-x" '+/-:forward-
;;  )
;;(general-def
;;  :states '(visual)
;;  "C-a" '+/-:region+
;;  "C-x" '+/-:region-
;;  "C-A" '+/-:block+
;;  "C-X" '+/-:block-
;;  )
;;)
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
					;;(:discard(:and(:category "Annuals" :scheduled past)))
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
(set-face-attribute 'org-headline-done nil :strike-through t)
(use-package transient
	:straight t)
(use-package org-ql
	:straight (:host github :repo "alphapapa/org-ql"))

(setq org-agenda-files '("~/Sync/Org/todo.org" "~/Sync/Org/refile.org" "~/Sync/Org/annuals.org")
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
			org-agenda-start-on-weekday nil
			org-outline-path-complete-in-steps nil
			calendar-week-start-day 1
			org-default-notes-file "~/Sync/Org/refile.org"
			org-capture-templates '(
															("t" "Todo" entry (file "~/Sync/Org/refile.org")
															 "* TODO %?"
															 :empty-lines 1)
															("l" "Link" entry (file "~/Sync/Org/refile.org")
															 "* TODO [%?[][]]\n:PROPERTIES:\n:CREATED: %U\n:END:"
															 :empty-lines 1)
															("f" "File link" entry (file "~/Sync/Org/refile.org")
															 "* TODO %A\n:PROPERTIES:\n:CREATED: %U\n:END:"
															 :empty-lines 1)
															("c" "Check Computer" entry (file+olp "~/Sync/Org/todo.org" "Computer" "Check")
															 "* TODO [%?[][]]\n:PROPERTIES:\n:CREATED: %U\n:END:"
															 :empty-lines 1)
															("p" "Check Psychology" entry (file+olp "~/Sync/Org/todo.org" "Psychology" "Check")
															 "* TODO [%?[][]]\n:PROPERTIES:\n:CREATED: %U\n:END:"
															 :empty-lines 1)
															("m" "Movies" entry (file+olp "~/Sync/Org/todo.org" "Leisure" "Movies")
															 "* TODO %?"
															 :empty-lines 1)
															("n" "Movies waiting" entry (file+olp "~/Sync/Org/todo.org" "Leisure" "Movies" "Waiting")
															 "* TODO %?"
															 :empty-lines 1)
															("s" "Series" entry (file+olp "~/Sync/Org/todo.org" "Leisure" "Series")
															 "* TODO %?"
															 :empty-lines 1)
															("w" "Series waiting" entry (file+olp "~/Sync/Org/todo.org" "Leisure" "Series" "Waiting")
															 "* TODO %?"
															 :empty-lines 1)
															("b" "Books" entry (file+olp "~/Sync/Org/todo.org" "Leisure" "Books")
															 "* TODO %?"
															 :empty-lines 1)
															("o" "Otro" entry (file+olp "~/Sync/Org/todo.org" "Other")
															 "* TODO %?"
															 :empty-lines 1)
															)
			org-refile-targets (quote(
																("~/Sync/Org/todo.org" :maxlevel . 10)
																("~/Sync/Org/annuals.org" :maxlevel . 10)
																))
			org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED"))
			org-agenda-custom-commands '(("w" "Weekly view" (
																											 (org-ql-block '(and (todo "WAITING")))
																											 (agenda))))
			org-icalendar-combined-agenda-file "~/Sync/Org/calendar.ics"
			org-icalendar-include-todo t
			org-icalendar-include-body 1000
			org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start)
			org-icalendar-use-deadline	'(event-if-todo event-if-not-todo todo-start)
			)

(general-define-key
 :keymaps 'org-capture-mode-map
 [remap evil-save-and-close]					'org-capture-finalize
 [remap evil-save-modified-and-close] 'org-capture-finalize
 [remap evil-quit]										'org-capture-kill)

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%e-%m-%Y, %a>" . "<%e-%m-%Y, %a %H:%M>"))

(coba-leader-def
	"c"	 'org-capture
	"a"	 '(:ignore t :which-key "Org-Agenda")
	"aa" '(lambda() (interactive)(coba-org-agenda-weekly))
	"o"	 '(lambda() (interactive)(find-file "~/Sync/Org/todo.org"))
	"R"	 '(lambda() (interactive)(find-file "~/Sync/Org/refile.org"))
	"ad" '(lambda ()(interactive)(org-ql-search "~/Sync/Org/todo.org"
																 '(done) :sort '(date priority todo)))
	"ap" '(lambda()(interactive)
					(org-ql-search (org-agenda-files)
						'(and (tags "track") (or (todo "TODO") (todo "WAITING")))
						:title "Projects"
						:sort '(date priority todo)
						:super-groups '((:auto-property "Project")))
					(delete-other-windows))
	)

(defun coba-org-agenda-weekly ()
	"Helper to open Agenda in weekly view by default."
	(org-agenda nil "w")
	(delete-other-windows)
	)

(defun coba-org-create-project (PROJECT)
	"Create an 'org-mode' PROJECT for querying with org-ql."
	(interactive "sProject name: ")
	(org-toggle-tag "track")
	(org-set-property "Project" PROJECT)
	)

(defun coba-org-icalendar-combine-agenda-files-hook ()
	"Create a .ics file from agenda files asynchronously when an `org-mode` file is saved."
	(interactive)
	(when (eq (buffer-name) "todo.org")
		(org-icalendar-combine-agenda-files t)
		)
	)
;;(add-hook 'org-mode-hook
;;					(lambda ()
;;						(add-hook 'after-save-hook 'coba-org-icalendar-combine-agenda-files-hook nil 'make-it-local)))

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
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.85))

(use-package org-pomodoro
	:straight t
	:general
	(coba-leader-def
		"t" 'org-pomodoro)
	:config
	(setq org-pomodoro-keep-killed-pomodoro-time t
				org-pomodoro-manual-break t
				org-pomodoro-play-sounds nil)
	)

(general-def
	:states '(normal motion)
	:keymaps 'org-mode-map
	"C-t" 'org-todo
	"C-S-T" 'coba-org-todo-yesterday-twice
	"ga"	'org-archive-subtree-default
	"gr"	'org-refile
	"gR"	'org-refile-goto-last-stored
	"C-j" 'org-metadown
	"C-k" 'org-metaup
	"C-h" 'org-metaleft
	"C-l" 'org-metaright
	"C-J" 'org-shiftmetadown
	"C-K" 'org-shiftmetaup
	"C-H" 'org-shiftmetaleft
	"C-L" 'org-shiftmetaright
	"C-P" 'org-latex-preview
	"gG"	'counsel-outline
	)

(general-def
	:states '(normal motion)
	:keymaps 'org-agenda-mode-map
	"ga" 'org-agenda-archive
	"C-t" 'org-agenda-todo
	"C-S-T" 'coba-org-agenda-todo-yesterday-twice
	)

(coba-local-leader-def
	:keymaps 'org-agenda-mode-map
	"s" 'org-agenda-schedule
	"d" 'org-agenda-deadline
	)

(defun coba-org-todo-yesterday-twice ()
	"Call `org-todo-yesterday` twice."
  (interactive)
  (org-todo-yesterday)
  (org-todo-yesterday)
	)

(defun coba-org-agenda-todo-yesterday-twice ()
	"Call `org-todo-yesterday` twice."
  (interactive)
  (org-agenda-todo-yesterday)
  (org-agenda-todo-yesterday)
	)
(general-def
	:states '(normal motion)
	:keymaps 'org-ql-view-map
	"ga" 'org-agenda-archive
	)

(coba-local-leader-def
	:keymaps 'org-mode-map
	:states '(normal motion)
	"s" 'org-schedule
	"d" 'org-deadline
	"t" 'counsel-org-tag
	"P" 'coba-org-create-project
	)

(use-package org-superstar
	:straight t
	:config
	(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
	)

(add-hook 'org-mode-hook
					(lambda ()
						visual-line-mode))
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
	 (dot . t)
	 ))
(setq
 org-confirm-babel-evaluate nil
 )
(setf
 org-babel-default-header-args:R '((:output . "results"))
 )

(use-package org-download
	:straight t
	:after general
	:config
	(coba-local-leader-def 'org-mode-map
		"p" 'org-download-clipboard)
  (setq-default org-download-image-dir "~/Brain/pictures"
                org-download-heading-lvl nil))

(use-package org-roam
	:straight t
	:general
	(coba-leader-def
		"r" 'org-roam-find-file)
	(general-def
		:keymaps 'org-mode-map
		"C-i" 'org-roam-insert)
	(coba-local-leader-def
		:keymaps 'org-mode-map
		"," 'org-roam
		)
	:config
	(setq org-roam-directory "~/Brain"
				org-roam-index-file "README.org"
        org-roam-db-location "~/Brain/roam.db")

	(setq org-roam-capture-templates '(("d" "default" plain (function org-roam-capture--get-point)
																			"- tags :: %?\n\n"
																			:file-name "${slug}"
																			:head "#+STARTUP: latexpreview\n#+TITLE: ${title}\n#+roam_tags:\n#+roam_alias:\n\n"
																			:unnarrowed t))
				)

	(org-roam-mode 1)
	)

(use-package company-org-roam
	:straight (:host github :repo "org-roam/company-org-roam")
	:after company
	:config
	(push 'company-org-roam company-backends))

(use-package org-roam-server
	:straight t
	:config
	(setq org-roam-server-host "127.0.0.1"
				org-roam-server-port 8080
				org-roam-server-authenticate nil
				org-roam-server-export-inline-images t
				org-roam-server-serve-files nil
				org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
				org-roam-server-network-poll t
				org-roam-server-network-arrows nil
				org-roam-server-network-label-truncate t
				org-roam-server-network-label-truncate-length 60
				org-roam-server-network-label-wrap-length 20)
	(org-roam-server-mode)
	)

(use-package org-roam-bibtex
	:straight t
	:after org-roam
	:config
	(add-hook 'after-init-hook #'org-roam-bibtex-mode)
	(setq orb-templates '(("d" "default" plain (function org-roam-capture--get-point) "- tags :: %?\n\n" :file-name "${citekey}"
												 :head "#+STARTUP: latexpreview\n#+TITLE: ${citekey}\n#+roam_alias: \"${author-abbrev}: ${title}\"\n#+ROAM_KEY: ${ref}\n\n"
												 :unnarrowed t))
				)
	)

;; Company

(use-package company
	:straight t
	:config
	(global-company-mode)

	(setq company-idle-delay 0
				company-minimum-prefix-length 2
				company-selection-wrap-around t
				)
	(setq default-company-backends '(company-semantic
																	 company-gtags
																	 company-files
																	 company-keywords
																	 company-capf
																	 company-yasnippet
																	 company-abbrev
																	 company-dabbrev
																	 company-dabbrev-code)
				company-backends (list default-company-backends))
	(general-def 'company-active-map
		"C-j" 'company-select-next
		"C-k" 'company-select-previous
		"C-s" 'company-search-candidates
		)
	)

(use-package company-quickhelp
	:straight t
	:after company
	:config
	(company-quickhelp-mode))

(use-package company-prescient
	:straight t
	:after company
	:config
	(company-prescient-mode)
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
	 bibtex-completion-bibliography "~/Brain/bib.bib"
	 bibtex-completion-library-path "~/Brain/pdf"
	 bibtex-completion-notes-path		"~/Brain/";;"bibnotes.org"
	 bibtex-completion-pdf-open-function (lambda (fpath)
																				 (call-process "zathura" nil 0 nil fpath))
	 bibtex-completion-notes-template-multiple-files "#+TITLE: {author-or-editor} (${year}): ${title} "
	 )
	;; So that org-ref inherits ivy-bibtex format
	;; From: https://github.com/jkitchin/org-ref/issues/717#issuecomment-633788035
	(ivy-set-display-transformer
	 'org-ref-ivy-insert-cite-link
	 'ivy-bibtex-display-transformer)

	:general
	(coba-leader-def
		"pp" 'ivy-bibtex
		"pd" 'doi-add-bibtex-entry
		"pf" '(lambda() (interactive)(find-file "~/Brain/bib.bib"))
		"pn" '(lambda() (interactive)(find-file "~/Brain/bibnotes.org"))
		)
	)

(defun coba-file-content-as-string (filename)
  "Return the contents of FILENAME as string.
https://gist.github.com/bigodel/56a4627afdfe9ad28f6dcc68b89a97f8"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defvar org-tex-apa-template
  (coba-file-content-as-string "~/.emacs.d/ox-templates/apa-article.tex"))

(defvar org-tex-apa-es-template
  (coba-file-content-as-string "~/.emacs.d/ox-templates/apa-article-es.tex"))

(defvar org-tex-report-template
  (coba-file-content-as-string "~/.emacs.d/ox-templates/report.tex"))

(defvar org-tex-graphix-template
  (coba-file-content-as-string "~/.emacs.d/ox-templates/graphix.tex"))

(defvar org-tex-footmisc-template
  (coba-file-content-as-string "~/.emacs.d/ox-templates/footmisc.tex"))

(defvar org-tex-math-template
  (coba-file-content-as-string "~/.emacs.d/ox-templates/math.tex"))

(defvar custom-tex-template nil
  "Custom latex preamble for org-export."
  )

(setq custom-tex-template (mapconcat 'identity (list org-tex-apa-template
                                                     org-tex-math-template
                                                     org-tex-graphix-template) "\n"))

(defun coba-define-org-tex-template ()
  "Define `org-latex-classes' concatenating snippets."
  (interactive)

  (setq org-tex-apa-template
        (coba-file-content-as-string "~/.emacs.d/ox-templates/apa-article.tex"))

  (setq org-tex-apa-es-template
        (coba-file-content-as-string "~/.emacs.d/ox-templates/apa-article-es.tex"))

  (setq org-tex-report-template
        (coba-file-content-as-string "~/.emacs.d/ox-templates/report.tex"))

  (setq org-tex-graphix-template
        (coba-file-content-as-string "~/.emacs.d/ox-templates/graphix.tex"))

  (setq org-tex-footmisc-template
        (coba-file-content-as-string "~/.emacs.d/ox-templates/footmisc.tex"))

  (setq org-tex-math-template
        (coba-file-content-as-string "~/.emacs.d/ox-templates/math.tex"))

  (setq org-latex-classes
        `(("custom" ,(format "%s
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
[EXTRA]" custom-tex-template)
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

          ("custom-apa" ,(format "%s
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
[EXTRA]" custom-tex-template)
           ("\\section*{%s}" . "\\section*{%s}")
           ("\\subsection*{%s}" . "\\subsection*{%s}")
           ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph*{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph*{%s}" . "\\subparagraph*{%s}"))
          )))

(coba-define-org-tex-template)
;; TODO: Check what's up with codeblocks

(defun coba-org-latex-remove-title (str)
  "Remove empty \title{} from STR.
From https://stackoverflow.com/questions/57967064/disable-title-in-org-latex-export ."
  (replace-regexp-in-string "^\\\\title{}$" "" str))

(advice-add 'org-latex-template :filter-return 'coba-org-latex-remove-title)

(setq 
 org-startup-with-inline-images t
 org-latex-compiler "xelatex"
 org-latex-bib-compiler "bibtex"
																				;org-export-in-background t
 org-latex-default-class "custom"
 org-latex-hyperref-template nil
 org-latex-prefer-user-labels t
 org-export-with-toc nil
 org-latex-listings t
 org-latex-listings-options '(
															("basicstyle" "\\ttfamily\\color{code-fg}")
															("stringstyle" "\\ttfamily\\color{code-string}")
															("commentstyle" "\\color{code-comment}")
															("keywordstyle" "\\color{code-keyword}")
															("upquote" "true")
															("backgroundcolor" "\\color{code-bg}")
															("frame" "single")
															("framesep" "5pt")
															("rulecolor" "\\color{code-comment}")
															("framerule" "1pt")
															)
 org-latex-pdf-process
 '("ln -s ~/Brain/bib.bib bib.bib"
	 "latexmk -pdflatex='xelatex -interaction nonstopmode' -shell-escape -pdf -bibtex -f %f")
 )

(add-to-list 'org-latex-default-packages-alist '("" "txfonts" t))
(add-to-list 'org-latex-default-packages-alist '("" "graphviz" t))

(use-package org-ref
	:straight t
	:init
	(setq org-ref-completion-library 'org-ref-ivy-cite)
	:config
	(setq reftex-default-bibliography	 '("~/Brain/bib.bib")
				org-ref-default-bibliography '("~/Brain/bib.bib")
				org-ref-bibliography-notes	 "~/Brain/"
				org-ref-pdf-directory				 "~/Brain/pdf"
        org-ref-default-citation-link "parencite"
				org-ref-open-pdf-function (lambda (fpath)
																		(call-process "zathura" nil 0 nil fpath))
				)
  (general-def 'org-mode-map
    "C-c ]" 'org-ref-ivy-insert-cite-link)

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

(use-package graphviz-dot-mode
	:straight t
	:config
	(setq graphviz-dot-indent-width 4)
	(use-package company-graphviz-dot)
	)

(use-package dumb-jump
	:straight t
	:config
	(setq dumb-jump-prefer-searcher 'rg
				dumb-jump-selector 'ivy)
	:general
	(coba-leader-def
		"G" '(:ignore t :which-key "dumb jump")
		"GG" 'dumb-jump-go
		"GS" 'dumb-jump-go-other-window
		"GF" 'dumb-jump-back)
	)

;; TODO : pdf

;; Git
(use-package magit
	:straight t
	:after evil
	:config
	(coba-leader-def
		"g" '(lambda() (interactive)(coba-magit-status))
		)
	(general-def
		:keymaps 'git-rebase-mode-map
		"C-j" 'git-rebase-move-line-down
		"C-k" 'git-rebase-move-line-up)
	(defun coba-magit-push-all ()
		"Push to all remotes.
From https://www.reddit.com/r/emacs/comments/ja97xs/weekly_tipstricketc_thread/?utm_medium=android_app&utm_source=share"
		(interactive)
		(mapcar (lambda(remote)	 ;; Loops through the remotes returned by magit-list-remotes
							(magit-run-git-async "push" "-v" remote (magit-get-current-branch))) ;; Simply run git push -v {{remote}} {{current-branch}}
						(magit-list-remotes)) ;; Returns all remotes configured
		)

	(transient-append-suffix 'magit-push "e"	;; Puts the following command after the 'e' option on the magit-push menu
		'("a" "Push all" coba-magit-push-all))	;; Configures the my/magit-push-all method to be callable with "a" while on the magit-push menu. It's description will read "Push All"
	)
(evil-set-initial-state 'magit-commit-message-section-map 'insert)

(defun coba-magit-status ()
	"Open magit-status in full screen."
	(magit-status)
	(delete-other-windows)
	)

(use-package forge
	:straight t
  :after magit)

(use-package magit-todos
	:straight t
	:config
	(global-hl-todo-mode 1)
	(magit-todos-mode 1)
	)

(use-package git-timemachine
	:straight t
	:config
	;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
	;; Unavailable link
	(with-eval-after-load 'git-timemachine
		(evil-make-overriding-map git-timemachine-mode-map 'normal)
		;; force update evil keymaps after git-timemachine-mode loaded
		(add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))
	)

;; Flyckeck
(use-package flycheck
	:straight t
	:general
	(general-def
		:prefix "C-S-f"
		"v" 'flycheck-verify-setup
		"C-f" 'flycheck-list-errors
		)
	:config
	(add-hook 'after-init-hook #'global-flycheck-mode)
	)

;; formatters
(use-package format-all
	:straight t
	:after apheleia
	:general
	(general-def 'ess-r-mode-map
		:states '(normal motion)
		"gf" 'format-all-buffer)
	)

(use-package apheleia
	:straight (:host github :repo "raxod502/apheleia")
	:general
	(general-def
		:states '(normal motion)
		"gf" 'apheleia-format-buffer)
	;;	:config
	;;	(push '(ess-r-mode . styler)
	;;				apheleia-mode-alist)
	;;
	;;	(push '(styler . ((concat
	;;										 "Rscript -e"
	;;										 "library(styler)"
	;;																				;	 "options(styler.colored_print.vertical=FALSE);"
	;;										 " con <-"
	;;										 file
	;;										 ";"
	;;										 " out <- styler::style_text(readLines(con));"
	;;										 " close(con);"
	;;										 " out")))
	;;				apheleia-formatters)
  :config
  (apheleia-global-mode)
	)


;; Yasnippets

(use-package yasnippet
	:straight t
	:config
	(yas-global-mode 1)
	(define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
	)

;; TODO: Pamparam repetition, or anki-el, or org-drill


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

(use-package mu4e
  :straight t
  :general
  (coba-leader-def
    "m" 'mu4e)
  :config
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  (setq mail-user-agent 'mu4e-user-agent
        mu4e-change-filenames-when-moving t
        mu4e-get-mail-command "/usr/bin/mbsync -a"
        mu4e-update-interval 120
        ;;mu4e-html2text-command "html2text --unicode-snob"
        ;;shr-color-visible-luminance-min 80
        message-kill-buffer-on-exit t
        mu4e-index-update-error-warning nil
        mu4e-view-show-images t
        mu4e-attachment-dir "/tmp"
        message-send-mail-function 'smtpmail-send-it
        mu4e-compose-dont-reply-to-self t
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask
        )
  (add-to-list 'mu4e-bookmarks
               '(:name "Coba Inbox"
                       :query "maildir:/coba/Inbox"
                       :key ?j)
               )
  (add-to-list 'mu4e-bookmarks
               '(:name "Cosas Inbox"
                       :query "maildir:/cosas/Inbox"
                       :key ?k)
               )
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "coba"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/coba" (mu4e-message-field msg :maildir))))
          :vars '((user-full-name        . "Coba")
                  (user-mail-address     . "coba@cobac.eu")
                  (mu4e-drafts-folder    . "/coba/Inbox/Drafts")
                  (mu4e-sent-folder      . "/coba/Inbox/Sent")
                  (mu4e-refile-folder    . "/coba/Inbox/Archives")
                  (mu4e-trash-folder     . "/coba/Inbox/Trash")
                  (smtpmail-smtp-user    . "coba@cobac.eu")
                  (smtpmail-smtp-server  . "mail.your-server.de")
                  (smtpmail-stream-type  . ssl)
                  (smtpmail-smtp-service . 465)))
         (make-mu4e-context
          :name "j-cosas"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/cosas" (mu4e-message-field msg :maildir))))
          :vars '((user-full-name        . "David Coba")
                  (user-mail-address     . "cosas@cobac.eu")
                  (mu4e-drafts-folder    . "/cosas/Inbox/Drafts")
                  (mu4e-sent-folder      . "/cosas/Inbox/Sent")
                  (mu4e-refile-folder    . "/cosas/Inbox/Archives")
                  (mu4e-trash-folder     . "/cosas/Inbox/Trash")
                  (smtpmail-smtp-user    . "cosas@cobac.eu")
                  (smtpmail-smtp-server  . "mail.your-server.de")
                  (smtpmail-stream-type  . ssl)
                  (smtpmail-smtp-service . 465)))))

  (setq mu4e-split-view 'horizontal
        mu4e-view-show-addresses t
        mu4e-view-show-images t
        )
  (add-to-list 'mu4e-view-actions '("browser" . mu4e-action-view-in-browser) t)

  (coba-local-leader-def 'mu4e-view-mode-map
    "t" '(lambda () (interactive)(org-capture "nil" "f"))
    )
  (add-to-list 'auto-mode-alist '("\\.eml\\'" . org-mode))
  )

(use-package mu4e-alert
  :straight t
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (setq doom-modeline-mu4e t)
  )

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
;;  (setq mu4e-headers-fields '((:empty         .    2)
;;                              (:human-date    .   12)
;;                              (:flags         .    6)
;;                              (:mailing-list  .   10)
;;                              (:from          .   22)
;;                              (:subject       .   nil)))
;;  (define-key mu4e-headers-mode-map (kbd "<tab>")     'mu4e-headers-toggle-at-point)
;;  )

;; TODO: check inline thing
(use-package org-msg
  :straight t
  :config
  (setq
   org-msg-default-alternatives '(text html)
   org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	 org-msg-startup "inlineimages"
   org-msg-convert-citation t)
  (setq-default org-html-with-latex 'dvipng)
  (org-msg-mode)
  (add-to-list  'mm-inhibit-file-name-handlers 'openwith-file-handler)
  )


(mu4e)

;; TODO: IRC

;; Dired

(use-package dired
	:config
	)

;; Olivetti
(use-package olivetti
	:straight t
	:config
	(coba-leader-def
		"W" 'olivetti-mode)
	)

;; Libvterm
(use-package vterm
  :straight (:type git :host github :repo "akermu/emacs-libvterm")
	:general
	(coba-leader-def
		"RET" 'vterm)
  )


(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-l")
  ;; Optimization stuff
  (setq gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024)
        lsp-completion-provider :capf
        lsp-idle-delay 0.500
        )
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :general
  (general-def 'lsp-browser-mode-map
    "q" 'quit-window)
  (general-def 'lsp-mode-map
    "C-l i" 'lsp-ivy-workspace-symbol)
  :config
  (evil-set-initial-state 'lsp-browser-mode 'motion)
  (setq lsp-headerline-breadcrumb-segments '(symbols)
        lsp-log-io t
        lsp-print-performance t
        ) 
  ) 

(use-package lsp-ui
  :straight t)

(use-package lsp-ivy
  :straight t)

;; debugger
;;(use-package dap-mode
;;  :straight t)
;; (use-package dap-LANGUAGE
;;   :straight t)

;; gpg passwords

(defun coba-lookup-password (&rest keys)
  "Helper to get KEYS from emacsclient."
  (when-let ((result (apply #'auth-source-search keys)))
    (funcall (plist-get (car result) :secret))))

(use-package atomic-chrome
  :straight t
  :config
  (atomic-chrome-start-server)
  (setq atomic-chrome-buffer-open-style 'full)
  )

																				; Languages
;; Systemd
(use-package systemd
	:straight t)

;; Lisp
(coba-local-leader-def
	:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
	"r" 'eval-region
	"b" 'eval-buffer)

;; ESS
(use-package ess
	:straight t
                                        ;  :hook
                                        ;  (ess-r-mode . lsp)
                                        ;  (inferior-ess-r-mode . lsp)
	:config
	(require 'ess-site)
	(setq
	 ess-ask-for-ess-directory nil
	 comint-move-point-for-output 'others
	 comint-scroll-to-bottom-on-input 'this
	 ess-eval-visibly 'nowait
	 ess-style 'RStudio
	 )

  (evil-set-initial-state 'ess-r-help-mode 'motion)
  (general-def 'ess-r-help-mode-map
    "q" 'quit-window)

	(general-def
		:keymaps '(comint-mode-map)
		"C-k" 'comint-previous-input
		"C-j" 'comint-next-input
		"C-h" 'comint-previous-matching-input-from-input
		"C-ñ" 'ess-insert-assign)

	(general-def
		:states '(insert)
		:keymaps 'ess-mode-map
		"C-ñ" 'ess-insert-assign
		)

	(coba-local-leader-def
		:keymaps 'ess-mode-map
		"b" 'ess-eval-buffer
		)
	)

(evil-set-initial-state 'ess-julia-help-mode 'motion)
(general-def 'ess-julia-help-mode-map
  "q" 'quit-window)

;;	Stan
(use-package stan-mode
	:straight t
	:after company
	:mode ("\\.stan\\'" . stan-mode)
	:hook (stan-mode . stan-mode-setup)
	:config
	(setq stan-indentation-offset 2)
	)
(use-package company-stan
	:straight t
	:after stan-mode
	:hook (stan-mode . company-stan-setup)
	)
(use-package eldoc-stan
	:straight t
	:after stan-mode
	:hook (stan-mode . eldoc-stan-setup)
	)
(use-package flycheck-stan
	:straight t
	:after stan-mode
	:hook ((stan-mode . flycheck-stan-stanc2-setup)
				 (stan-mode . flycheck-stan-stanc3-setup))
	:config
	(setq flycheck-stanc-executable nil
				flycheck-stanc3-executable nil)
	)
(use-package stan-snippets
	:straight t
	:after stan-mode
	)

(use-package lsp-julia
  :straight (:type git :host github :repo "non-Jedi/lsp-julia")
  :after lsp-mode
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.6")
  )

;;(use-package eglot
;;  :straight t
;;  :config
;;  (setq eglot-connect-timeout 99999999)
;;  )
;;
;;(use-package eglot-jl
;;  :straight t
;;  )

(use-package julia-mode
  :straight t
  :config
  (add-hook 'julia-mode-hook #'lsp)
  )

;;(use-package julia-snail
;;  :straight (:type git :host github :repo "gcv/julia-snail")
;;  ;; :requires vterm
;;  ;;  :after julia-mode
;;  :general
;;  (general-def 'julia-snail-mode-map
;;    "C-c C-k" 'coba-julia-snail-save-and-send-buffer-file)
;;  :config
;;  (defun coba-julia-snail-save-and-send-buffer-file()
;;    "Save buffer and call julia-snail-send-buffer-file."
;;    (interactive)
;;    (save-buffer)
;;    (julia-snail-send-buffer-file))
;;  )
;;
;;;; Idk why it does not run inside the usepackage macro
;;(add-hook 'julia-mode-hook #'julia-snail-mode)

;; (use-package julia-repl
;;   :straight t
;;   ;;:after julia-mode
;;   :config
;;   ;; Obsolete but... works
;;   (setq display-buffer-reuse-frames t)
;;   (julia-repl-set-terminal-backend 'vterm) 
;;   (add-hook 'julia-mode-hook #'julia-repl-mode)
;;   )

(use-package julia-vterm
  :straight t
  :config
  (add-hook 'julia-mode-hook #'julia-vterm-mode)
  (general-def 
	  :states '(normal visual insert motion emacs)
	  :keymaps 'julia-mode-map
    "C-c C-c" 'julia-vterm-send-region-or-current-line
    "C-t" 'coba-julia-vterm-define-threads
    )

  (defun coba-julia-vterm-define-threads ()
    (interactive)
    (let ((val (read-from-minibuffer "Number of threads: ")))
      (setq julia-vterm-repl-program (concat "/path/to/julia -t " val))))
  )

(use-package ob-julia-vterm
  :straight t
  :config 
  (add-to-list 'org-babel-load-languages '(julia-vterm . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
  )

(use-package eterm-256color
  :straight t
  :config
  (add-hook 'term-mode-hook #'eterm-256color-mode))



;; Markdown
(use-package markdown-mode
	:straight t
	:mode
	(("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
																				;("\\.Rmd\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)
	 )
  :config
  (general-def 'markdown-view-mode-map
    "q" 'quit-window)
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

(use-package poly-org
	:straight t
	)

;; Latex
(use-package tex
	:straight auctex
	:config
	(TeX-global-PDF-mode t)
	(setq TeX-source-correlate-mode t
				TeX-source-correlate-start-server t
				TeX-parse-self t
				TeX-auto-save t
				TeX-view-program-selection '((output-pdf "Zathura"))
				LaTeX-command "xelatex --synctex=1"
				pdf-latex-command "XeLaTeX")

	(add-hook 'LaTeX-mode-hook
						(lambda ()
							(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
							(setq TeX-command-extra-options "-file-line-error -shell-escape"
										TeX-command-default "XeLaTeX"
										TeX-auto-untabify t
										TeX-engine 'xetex
										TeX-show-compilation nil)
							(TeX-global-PDF-mode t)
							(setq TeX-save-query nil)))

	(coba-local-leader-def
		:keymaps 'LaTeX-mode-map
		"s" 'LaTeX-section
		"e" 'LaTeX-environment
		"m" 'TeX-insert-macro
		"p" 'preview-buffer
		"c" 'coba-texcount
		)
	(general-def
		:states '(normal visual insert motion emacs)
		:keymaps 'LaTeX-mode-map
		"C-f" 'TeX-font
		"C-c C-c" 'TeX-command-run-all
		"C-c C-a" 'TeX-command-master
		)

	(defun coba-texcount ()
		"Print number of output words from the current buffer. It invoques texcount."
		(interactive)
		(save-buffer)
		(shell-command (concat "texcount " (buffer-name))))

  (defun coba-bibtex-check()
    "Check the validity of a bibtex file."
    (interactive)
    (save-buffer)
    (shell-command (concat "biber -tool -V " (buffer-name))))
  (coba-local-leader-def
    :keymaps 'bibtex-mode-map
    "c" 'coba-bibtex-check)
	)

;; Haskell
(use-package haskell-mode
	:straight t
	:general

	(general-def '(haskell-mode-map haskell-interactive-mode-map)
		"C-ñ" '(lambda () (interactive) (insert "-> "))
		"C-Ñ" '(lambda () (interactive) (insert "<- ")))
	(general-def 'haskell-mode-map 
		"C-c C-c" 'haskell-process-load-file)
	(coba-local-leader-def 'haskell-mode-map
		"r" 'hlint-refactor-refactor-at-point
		"h" 'hoogle)
  (general-def 'haskell-interactive-mode-map
    :states '(insert motion)
    "C-k" 'haskell-interactive-mode-history-previous
    "C-j" 'haskell-interactive-mode-history-next)
  :config
  (evil-set-initial-state 'haskell-error-mode 'motion)
	)

(use-package flycheck-haskell
	:straight t
	:config
	(add-hook 'haskell-mode-hook #'flycheck-haskell-setup)
	(setq-default flycheck-disabled-checkers '(haskell-ghc haskell-stack-ghc))
	)

(use-package hlint-refactor
	:straight t
	)

;; Make
(add-hook 'makefile-gmake-mode-hook
					'(lambda ()
						 (add-hook 'before-save-hook
											 (lambda ()
												 (tabify (point-min) (point-max))))))

(use-package docker
  :straight t
  :general
  (coba-leader-def
    "D" 'docker)
  :config
  (setq docker-run-as-root t))
