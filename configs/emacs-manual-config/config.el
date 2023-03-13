(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package auto-compile
  :ensure t)

(add-hook 'emacs-startup-hook
	(lambda ()
	       (message "Loaded Emacs in %.03fs"
			(float-time (time-subtract after-init-time before-init-time)
      ))))

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

(setq use-dialog-box nil)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

(advice-add #'x-apply-session-resources :override #'ignore)

(setq user-full-name    "Gil Mendes"
      user-mail-address "gil00mendes@gmail.com")

(eval-and-compile
  (defvar data-dir
    (if (getenv "XDG_DATA_HOME")
        (concat (getenv "XDG_DATA_HOME") "/emacs/")
      (expand-file-name "~/.local/share/emacs/"))
    "Directory for data.")

  (defvar cache-dir
    (if (getenv "XDG_CACHE_HOME")
        (concat (getenv "XDG_CACHE_HOME") "/emacs/")
      (expand-file-name "~/.cache/emacs/"))
    "Directory for cache.")

  (defvar pictures-dir
    (or (getenv "XDG_PICTURES_DIR")
        (expand-file-name "~/Pictures/"))
    "Directory for pictures."))

(setq use-package-always-ensure t)

(server-start)

(cond
 ((string-equal system-type "darwin")
  (setq frame-resize-pixelwise  t)
  (menu-bar-mode t)))

(setq auto-save-default       nil
      inhibit-startup-message t
      ring-bell-function      'ignore)
(dolist (mode
         '(tool-bar-mode
           tooltip-mode
           scroll-bar-mode
           blink-cursor-mode))
  (funcall mode 0))

(setq default-frame-alist
      (append (list
               '(min-height . 1)
               '(height     . 45)
               '(min-width  . 1)
               '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0))))

(setq-default fill-column 120)

(winner-mode 1)

(global-hl-line-mode)

(setq custom-file (make-temp-file ""))

(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq initial-scratch-message (format ";; Scratch buffer - started on %s\n\n" (current-time-string)))

(use-package flyspell
  :ensure nil
  :bind
  (:map flyspell-mode-map ("C-;" . nil))
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args   '("--sug-mode=ultra" "--lang=en_GB")))

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

  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(setq comp-async-report-warnings-errors        nil
      native-comp-async-report-warnings-errors nil)

(use-package use-package)

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom
  (vertico-resize t))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package embark
  :bind
  (("C-," . embark-act))

  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package consult
  :bind (("C-s"   . consult-line)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)

         ("C-x M-:" . consult-complex-command)
         ("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)

         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)

         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)

         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)

         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)

         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Add thin lines, sorting and hide the mode line of the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))
