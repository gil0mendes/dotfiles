;; Configure project mode
;;

(use-package projectile
  :diminish ""
  :config
  (setq projectile-completion-system 'helm)
  (projectile-mode))

(provide 'setup-projects)
