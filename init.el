(setq-default line-spacing 1)

(setq
 gc-cons-threshold (* 100 1024 1024)
 mac-command-modifier 'meta
 native-comp-async-report-warnings-errors nil
 read-process-output-max (* 1024 1024)
 split-height-threshold nil
 split-width-threshold 0
 straight-use-package-by-default t
 use-package-enable-imenu-support t)

(set-locale-environment "en_US.UTF-8")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package ace-window
  :custom
  (aw-dispatch-always t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("C-c o" . ace-window))

(use-package cape
  :after corfu
  :config)

(use-package corfu
  :custom
  (corfu-auto t)
  :config
  (global-corfu-mode)
  (defun meow-exit-corfu ()
    "Close corfu popup if it is active."
    (when corfu-mode (corfu-quit)))
  (add-hook 'meow-insert-exit-hook #'meow-exit-corfu))

(use-package denote
  :custom
  (denote-directory "~/org")
  (denote-dired-directories `(,denote-directory))
  (denote-org-front-matter
   "#+title:       %s
#+date:       %s
#+filetags:   %s
#+identifier: %s
\n")
  :preface
  ;; TODO: Check the same date journal file and jump to it
  (defun denote-journal ()
    "Create an entry tagged 'journal' with the date as its title."
    (interactive)
    (denote
     (format-time-string "%B %e, %Y")
     '("journal")))
  :hook (dired-mode . denote-dired-mode-in-directories)
  :bind (("C-c n j" . denote-journal)))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-dwim-target t))

  (use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-Iosvkem t))

(use-package eat
  :straight (eat :fetcher git
		 :url "/home/akib/projects/emacs-eat"
		 :files ("*.el" ("term" "term/*.el") "*.texi"
			 "*.ti" ("e" "e/*")
			 ("integration" "integration/*")
			 (:exclude ".dir-locals.el" "*-tests.el")))
  :hook (eshell-mode . eat-eshell-mode))

(use-package editorconfig
  :config
  (editorconfig-mode))

;; Wait for multi-server support to be implemented
(use-package eglot
  :disabled t
  :hook ((typescript-mode
	  typescript-tsx-mode)
	 . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	       '(typescript-tsx-mode "typescript-language-server" "--stdio")))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  (lsp-keymap-prefix "C-c l")
  :hook ((ruby-mode
	  typescript-mode
	  typescript-tsx-mode)
	 . lsp-deferred))

(use-package meow
  :config
  (setq meow-use-clipboard t)
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode))

(use-package magit
  :custom
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  (magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  :bind ("C-x g" . magit-status))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
	    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
	custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :straight (:type built-in)
  :custom
  (org-log-done 'time)
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets
   '((org-agenda-files :maxlevel . 1)))
  (org-refile-use-outline-path 'file)
  (org-todo-keywords '((sequence "TODO(t)" "PROG(p)" "|" "DONE(d)")
		       (sequence "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "STOP(s@/!)")))
  (org-use-fast-todo-selection 'expert))

(use-package org-agenda
  :straight (:type built-in)
  :custom
  (org-agenda-files `(,org-directory ,(concat (file-name-as-directory org-directory) "daily")))
  :bind ("C-c A" . org-agenda))

;; Trying out denote
(use-package org-roam
  :disabled t
  :custom
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-directory org-directory)
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ;; Dailies
	 ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode))

(use-package org-modern
  :config
  (global-org-modern-mode))

(use-package popwin
  :config
  (popwin-mode))

(use-package prettier-js
  :hook (typescript-tsx-mode . maybe-enable-prettier-js-mode)
  :init
  (defun maybe-enable-prettier-js-mode ()
    (if (locate-dominating-file default-directory ".prettierrc")
	(prettier-js-mode 1))))

(use-package project
  :straight (:type built-in)
  :commands (project-find-file
	     project-switch-project))

(use-package recentf
  :straight (:type built-in)
  :config
  (recentf-mode))

(use-package savehist
  :straight (:type built-in)
  :config
  (savehist-mode))

(use-package tree-sitter
  :custom
  (tree-sitter-major-mode-language-alist
   '((typescript-tsx-mode . tsx)))
  :hook ((typescript-mode
	  web-mode)
	 . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :config
  (tree-sitter-require 'tsx))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
	 ("\\.tsx\\'" . typescript-tsx-mode))
  :config
  (define-derived-mode typescript-tsx-mode web-mode "TypeScript-TSX"))

(use-package vertico
  :config
  (vertico-mode))

(use-package web-mode
  :commands (web-mode))
