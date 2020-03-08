;; ================================
;; This is the emacs home directory

(setq debug-on-error t)

;; ==========================
;; Package system and sources

(setq package-enable-at-startup nil)
(require 'package)

;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)

;; package archive for org mode
(add-to-list 'package-archives (cons "org" "http://orgmode.org/elpa/") t)

;; Initialize the package system
(package-initialize)

;; We will use 'use-package' to install and configure packages.
(unless (package-installed-p 'use-package)
  (Package-Refresh-Contents)
  (Package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Define the base directory for our configurations
(setq user-emacs-directory "~/.emacs.d/")

(add-to-list 'load-path (expand-file-name "configs" user-emacs-directory))

;; No need to out 'ensure' everywhere, since we don't use anything else to install packages.
(setq use-package-always-ensure t)

;; Store custom-file separately, don't freak out when it's not found
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Set path for private config. private.el is not part of Castlemacs and you can use it for your personal
;; additions. Do not change init.el yourself, it will make updates harder.
(add-hook
 'after-init-hook
 (lambda ()
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))

;; ========================
;; Configurations Functions

;; quickly visit the configuration
(defun gm:config-Visit ()
  "Visits the emacs config file"
  (interactive)
  (find-file (concat user-emacs-directory  "init.el")))
(global-set-key (kbd "C-c e") 'gm:config-visit)

;; reload configurations
(defun gm:config-reload ()
  "Reload the emacs configuration"
  (interactive)
  (load-file (concat user-emacs-directory "init.el"))
  (print "Config reloaded!"))
(global-set-key (kbd "C-c r") 'gm:config-reload)

;; Core stuff
(require 'setup-core)
(require 'setup-os)
(require 'setup-keys)
(require 'setup-appearance)

;; Modules
(require 'setup-helm)
(require 'setup-evil)
(require 'setup-projects)
(require 'setup-which-key)
(require 'setup-modeline)
