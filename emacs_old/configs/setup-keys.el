(defvar gm/leader-key "SPC"
  "Leader key prefix to use for key bindings.")

(defvar gm/non-normal-leader-key "M-SPC"
  "Leader key prefix to use for key bindings in non-normal evil modes.")

(defmacro gm/key (cmd &rest args)
  "Helper macro to add an argument to a key binding."
  `(lambda () (interactive) ,(append (cdr cmd) args)))

(use-package general
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix gm/leader-key
   :non-normal-prefix gm/non-normal-leader-key

   "a" 'helm-imenu

   "b" '(:ignore t :whick-key "buffers")
   "bb" 'helm-mini
   "bk" 'kill-this-buffer
   "bK" 'kill-matching-buffers

   "d" '(:ignore t :which-key "dired")
   "dd" 'dired
   "dj" 'dired-jump
   "dp" 'projectile-find-dir
   "dP" 'projectile-dired

   "e" '(:ignore t :which-key "emacs/eval")
   "eb" 'eval-buffer
   "eP" 'package-autoremove

   "f" '(:ingore t :whick-key "files")
   "ff" 'helm-find-files
   "fp" 'projectile-find-file
   "fr" 'helm-recentf
   "fR" 'rename-current-buffer-file
   "fs" 'save-buffer
   "fw" 'write-file))

(provide 'setup-keys)
