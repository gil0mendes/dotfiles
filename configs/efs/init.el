  ;; Tangled from dotfiles/configs/efs/README.org

  ;; -*- lexical-binding: t -*-

  (message "🚜 Loading init.el")

  ;; https://github.com/radian-software/straight.el#getting-started
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

  ;; https://github.com/radian-software/straight.el#integration-with-use-package
  (straight-use-package 'use-package)

  ;; The default is 800 kilobytes.  Measured in bytes.
  (setq gc-cons-threshold (* 50 1000 1000))

  ;; Profile emacs startup
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (message "*** Emacs loaded in %s seconds with %d garbage collection."
		       (emacs-init-time "%.2f")
		       gcs-done)))

  (let ((normal-gc-cons-threshold gc-cons-threshold)
      (normal-gc-cons-percentage gc-cons-percentage)
      (normal-file-name-handler-alist file-name-handler-alist)
      (init-gc-cons-threshold most-positive-fixnum)
      (init-gc-cons-percentage 0.6))
  (setq gc-cons-threshold init-gc-cons-threshold
        gc-cons-percentage init-gc-cons-percentage
        file-name-handler-alist nil)
  (add-hook 'after-init-hook
            `(lambda ()
               (setq gc-cons-threshold ,normal-gc-cons-threshold
                     gc-cons-percentage ,normal-gc-cons-percentage
                     file-name-handler-alist ',normal-file-name-handler-alist))))

  (setq inhibit-startup-echo-area-message t)
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message nil)

  (setq inhibit-default-init t)
  (setq initial-major-mode 'fundamental-mode)

(server-start)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (setq auto-save-default nil
  ring-bell-function 'ignore)

  (winner-mode 1)

(setq custom-file (make-temp-file ""))

(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq initial-scratch-message (format ";; Scratch buffer - started on %s\n\n" (current-time-string)))

  (setq use-dialog-box nil)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)

  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (show-paren-mode 1)

  (setq user-full-name      "Gil Mendes"
        user-mail-address   "gil00mendes@gmail.com")

  ;; https://github.com/emacsmirror/undo-fu
  (use-package undo-fu
  :straight (undo-fu :type git
                     :host github
                     :repo "emacsmirror/undo-fu"))

(setq world-clock-list '(("America/Los_Angeles" "Pacific")
			 ("Europe/Lisbon" "Portugal")
			 ("Etc/UTC" "UTC"))
      world-clock-time-format "%a, %d %b %I:%M %p %Z")

(use-package ibuffer
  :straight (:type built-in)
  :config
  (setq ibuffer-expert t
	ibuffer-display-summary nil
	ibuffer-show-empty-filter-groups nil
	ibuffer-use-header-line t
	ibuffer-formats '((mark modified read-only locked " "
				(name 30 30 :left :elide)
				" "
				(size 9 -1 :right)
				" "
				(mode 16 16 :left :elide)
				" " filename-and-process)
			  (mark " "
				(name 16 -1)
				" " filename)))
  :bind (("C-x C-b" . ibuffer)))

;; https://github.com/redguardtoo/evil-nerd-commenter
(use-package evil-nerd-commenter
  :straight (evil-nerd-commenter :type git
				 :host github
				 :repo "redguardtoo/evil-nerd-commenter")
  :bind (("C-/" . evilnc-comment-or-uncomment-lines)))

  (global-hl-line-mode)

  ;; https://www.emacswiki.org/emacs/FillColumnIndicator
  (setq display-fill-column-indicator-column 119)
  (global-display-fill-column-indicator-mode 1)

  (set-face-attribute 'default nil :font "Comic Code Ligatures" :height 120)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
		     :font "Comic Code Ligatures"
		     :height 120)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
		    :font "Comic Code Ligatures")

  ;; https://github.com/purcell/default-text-scale
  (use-package default-text-scale
    :straight (default-text-scale :type git
				  :host github
				  :repo "purcell/default-text-scale")
    :hook ((after-init . default-text-scale-mode)))

  (setq doom-theme 'doom-nord-light)
  (use-package doom-themes
    :straight (doom-themes :type git
			   :host github
			   :repo "doomemacs/themes"))

  (defun g0m/auto-update-theme ()
    "depending on time use different theme"
    ;; very early => gruvbox-light, solarized-light, nord-light
    (let* ((hour (nth 2 (decode-time (current-time))))
	   (theme (cond ((<= 7 hour 8)   'doom-gruvbox-light)
			((= 9 hour)      'doom-solarized-light)
			((<= 10 hour 16) 'doom-nord-light)
			((<= 17 hour 18) 'doom-gruvbox-light)
			((<= 19 hour 22) 'doom-oceanic-next)
			(t               'doom-laserwave))))
      (when (not (equal doom-theme theme))
	(setq doom-theme theme)
	(load-theme doom-theme t))
      ;; run that function again next hour
      (run-at-time (format "%02d:%02d" (+ hour 1) 0) nil 'g0m/auto-update-theme)))

  (g0m/auto-update-theme)

  (use-package svg-tag-mode
    :straight (svg-tag-mode :type git
			    :host github
			    :repo "rougier/svg-tag-mode"))

  ;; https://github.com/emacsorphanage/dired-k
  (use-package dired-k
  :straight (dired-k :type git
                     :host github
                     :repo "emacsorphanage/dired-k")
  :init
  (setq dired-k-style 'git)
  :hook (dired-initial-position-hook . dired-k))

  ;; https://github.com/domtronn/all-the-icons.el
  (use-package all-the-icons
    :straight (all-the-icons :type git
			     :host github
			     :repo "domtronn/all-the-icons.el"))

  ;; https://www.emacswiki.org/emacs/SmoothScrolling
  (setq-default scroll-conservatively 100)

  (use-package org-mode
    :ensure nil
    :hook
    (org-mode . org-indent-mode)
    (org-mode . variable-pitch-mode)
    (org-mode . visual-line-mode)
          :custom-face
  (avy-lead             ((t (:inherit fixed-pitch))))
  (avy-lead-face-0      ((t (:inherit fixed-pitch))))
  (avy-lead-face-1      ((t (:inherit fixed-pitch))))
  (avy-lead-face-2      ((t (:inherit fixed-pitch))))
  (company-tooltip      ((t (:inherit fixed-pitch))))
  (org-table            ((t (:inherit fixed-pitch))))
  (org-formula          ((t (:inherit fixed-pitch))))
  (org-checkbox         ((t (:inherit fixed-pitch))))
  (org-code             ((t (:inherit fixed-pitch))))
  (org-verbatim         ((t (:inherit fixed-pitch))))
  (org-special-keyword  ((t (:inherit fixed-pitch))))
  (org-meta-line        ((t (:inherit fixed-pitch))))
  (org-block            ((t (:foreground nil :inherit fixed-pitch))))
  (org-indent           ((t (:inherit (org-hide fixed-pitch)))))
  (fixed-pitch          ((t (:family "Menlo" :height 120))))
  (variable-pitch       ((t (:family "ETBembo" :height 160))))

  :init
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-cycle-separator-lines 2
        org-clock-persist 'history)

  (font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  :config
  (org-clock-persistence-insinuate)

  (dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))))

  ;; https://github.com/integral-dw/org-superstar-mode
  (use-package org-superstar
    :straight (org-superstar :type git
                             :host github
                             :repo "integral-dw/org-superstar-mode")
    :hook (org-mode . org-superstar-mode)
    :custom
    (org-superstar-remove-leading-stars t)
    (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

  ;; https://github.com/awth13/org-appear
  (use-package org-appear
    :straight (org-appear :type git
                          :host github
                          :repo "awth13/org-appear")
    :hook (org-mode . org-appear-mode))

;; https://github.com/bbatsov/projectile/
(use-package projectile
  :straight (projectile :type git
			:host github
			:repo "bbatsov/projectile")
  :custom
  (projectile-mode-line-prefix "🗄")
  :hook (after-init .projectile-mode)
  :bind (:map projectile-mode-map
	      ("C-x p" .  projectile-command-map)))

(use-package treesit
  :commands (treesit-install-language-grammar g0m/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (css . ("https://github.com/tree-sitter/tree-sitter-css"))
     (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
     (go . ("https://github.com/tree-sitter/tree-sitter-go"))
     (html . ("https://github.com/tree-sitter/tree-sitter-html"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (make . ("https://github.com/alemuller/tree-sitter-make"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
     (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
     (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
     (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
     (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))))
  :config
  (dolist (mapping '((python-mode . python-ts-mode)
		   (ruby-mode . ruby-ts-mode)
		   (css-mode . css-ts-mode)
		   (js-mode . js-ts-mode)
		   (javascript-mode . js-ts-mode)
		   (typescript-mode . typescript-ts-mode)
		   (js-json-mode . json-ts-mode)
		   (sh-mode . bash-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))
  (add-to-list 'auto-mode-alist '("\\.[tj]sx?\\'" . tsx-ts-mode))
  (defun g0m/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "🤟 `%s' parser was installed." lang)
	      (sit-for 0.75)))))

(use-package eglot
  :straight (:type built-in)
  :custom
  (read-process-output-max (* 1024 1024))
  (eglot-autoshutdown t)
  :hook ((css-ts-mode-hook . eglot-ensure)
	 (html-mode-hook .eglot-ensure)
	 (js-base-mode-hook . eglot-ensure)
	 (tsx-ts-mode-hook . eglot-ensure)))

(use-package savehist
  :straight (:type built-in)
  :init
  (savehist-mode))

;; https://github.com/minad/vertico
(use-package vertico
  :straight (vertico :type git
		     :host github
		     :repo "minad/vertico")
  :init
  (vertico-mode))

;; https://github.com/minad/corfu
(use-package corfu
  :straight (corfu :type git
		   :host github
		   :repo "minad/corfu")
  :bind
  (:map corfu-map
	("C-j" . corfu-next)
	("C-k" . corfu-previous)
	("TAB" . corfu-inser))
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-default 0.25)
  (corfu-count 10)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  (corfu-separator ?\s)                 ; Necessary for use with orderless
  (corfu-quit-no-match 'separator)
  (corfu-preselect-first t)             ; Preselect first candidate
  :init
  (global-corfu-mode))

;; https://github.com/minad/cape
(use-package cape
  :straight (cape :type git
		  :host github
		  :repo "minad/cape")
  :init
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; https://github.com/oantolin/orderless
(use-package orderless
  :straight (orderless :type git
		       :host github
		       :repo "oantolin/orderless")
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; https://github.com/minad/marginalia
(use-package marginalia
  :straight (marginalia :type git
			:host github
			:repo "minad/marginalia")
  :init
  (marginalia-mode))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :straight (which-key :type git
		       :host github
		       :repo "justbur/emacs-which-key")
  :delight
  :config
  (which-key-mode))

;; https://github.com/iyefrat/all-the-icons-completion
(use-package all-the-icons-completion
  :straight (all-the-icons-comletion :type git
				     :host github
				     :repo "iyefrat/all-the-icons-completion")
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; https://github.com/jdtsmith/kind-icon
(use-package kind-icon
  :straight (kind-icon :type git
		       :host github
		       :repo "jdtsmith/kind-icon")
  :after corfu
  :custom
  (kind-icons-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'
  )

  (use-package async
    :straight (async :type git
                   :host github
                   :repo "jwiegley/emacs-async"))

    ;; https://github.com/emacs-evil/evil
    ;; https://github.com/noctuid/evil-guide
    (use-package evil
      :straight (evil :type git
		    :host github
		    :repo "emacs-evil/evil")
      :after
      undo-fu
      :init
      ;; pre-set some evil vars prior to package load
      (setq evil-respect-visual-line-mode t)
      (setq evil-undo-system 'undo-fu)
      (setq evil-want-integration t)
      (setq evil-want-keybinding nil)
      (setq evil-mode-line-format nil)
      :config
      (message "😈 Configured evil-mode"))

  (use-package evil-collection
    :straight (evil-collection :type git
			       :host github
			       :repo "emacs-evil/evil-collection")
    :after evil
    :custom
    (evil-collection-setup-minibuffer t)
    :config
    (evil-mode 0)
    (message "😈 Enable evil-mode")
    (evil-collection-init))
