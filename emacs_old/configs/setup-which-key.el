;; Configure whish key
;;
;; This amazing package allows to get a helpful bottom sheet with the available keys

(use-package which-key
  :diminish ""
  :config
  (setq which-key-idle-delay 0.4
	which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode t))

(provide 'setup-which-key)
