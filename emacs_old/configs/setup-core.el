;; Garbage collection
;;
;; This fixes garbage collection, makes emacs start up faster.
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216 gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)

;; enable/disable built-in modes
(column-number-mode t)                ; enable column number
(delete-selection-mode t)
(global-hl-line-mode t)               ; highlight the current line
(savehist-mode t)
(show-paren-mode t)
(tooltip-mode -1)
(winner-mode t)

;; Revert (update) buffers automatically when underlying files are changed externally.
(use-package autorevert
  :diminish ""
  :config
  (global-auto-revert-mode t))

; save recent opened files
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 10)
(global-set-key "\C-x\ \C-r" 'recent-open-files)

;; sensible defaults
(fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(setq
 auto-revert-verbose t
 auto-save-default nil                ; Disable auto save for files
 global-auto-revert-non-file-buffers t
 history-length 1000                  ; Set the maximum length of a minibuffer list
 inhibit-startup-message t            ; Don't show the startup message
 inhibit-startup-screen t             ; ... or screen
 cursor-in-non-selected-windows t     ; Hide the cursor in inactive windows
 echo-keystrokes 0.1                  ; Show keystrokes right away, don't show the message in the scratch buffer
 initial-scratch-message nil          ; Empty sractch buffer
 initial-major-mode 'org-mode         ; Org mode by default
 sentence-end-double-space nil        ; Sentences should end in one space, come on!
 confirm-kill-emacs 'y-or-n-p         ; y and n instead of yes and no when quitting
 help-window-select t                 ; Select help window so it's easy to quit it with 'q'
 make-backup-files nil                ; Stop making backup files
 mouse-wheel-progressive-speed nil    ; Uses the real speed
 select-enable-clipboard t
 undo-tree-visualizer-timestamps 1
 vc-follow-symlinks t
 )

(delete-selection-mode 1)          ; Delete selected text when typing

;; Smoother and nicer scrolling
(setq scroll-margin 10
   scroll-step 1
   next-line-add-newlines nil
   scroll-conservatively 10000
   scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Use ESC as universal get me out of here command
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Warn only when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Move file to trash instead of removing.
(setq-default delete-by-moving-to-trash t)

(global-unset-key (kbd "s-p"))     ; Don't print

;; Delete trailing spaces and add new line in the end of a file on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Set UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;
;; Formatting

;; Indentation
(setq-default tab-width 4
	      tab-always-indent t
	      indent-tabs-mode t
	      fill-column 80)

;; Word wrapping
(setq-default word-wrap t
	      truncate-lines t
	      truncate-partial-width-windows nil)

;;(setq sentence-end-)

(provide 'setup-core)
