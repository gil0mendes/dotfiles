;; Tangled from dotfiles/configs/efs/README.org

;; -*- lexical-binding: t -*-

(message "🚜 Loading init.el")

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(package-install 'setup)
(require 'setup)

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
(setup (:package default-text-scale)
  (:hook default-text-scale-mode))

(setq doom-theme 'doom-nord-light)
(setup (:package doom-themes))

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

(setup (:package svg-tag-mode))

(setup (:package dired-k)
  (setq dired-k-style 'git)
  (:hook dired-initial-position-hook))

;; https://github.com/domtronn/all-the-icons.el
(setup (:package all-the-icons))
