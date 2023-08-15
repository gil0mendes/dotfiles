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

  (setq auto-save-default nil
  ring-bell-function 'ignore)

  (winner-mode 1)

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

  (defun g/auto-update-theme ()
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
      (run-at-time (format "%02d:%02d" (+ hour 1) 0) nil 'g/auto-update-theme)))

  (g/auto-update-theme)

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

  (setq user-full-name      "Gil Mendes"
        user-mail-address   "gil00mendes@gmail.com")

  ;; https://github.com/emacsmirror/undo-fu
  (use-package undo-fu
  :straight (undo-fu :type git
                     :host github
                     :repo "emacsmirror/undo-fu"))

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
    (evil-mode 1)
    (message "😈 Enable evil-mode")
    (evil-collection-init))

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
