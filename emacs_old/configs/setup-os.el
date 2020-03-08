;; Pass system shell environment to Emacs. This is important primarily for shell inside Emacs, but also
;; things like Org mode export to Tex PDF don't work, since it relies on running external command
;; pdflatex, which is loaded from PATH.
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'setup-os)
