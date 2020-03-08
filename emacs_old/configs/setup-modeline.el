;; Configure modeline
;;

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  ;; set these early so they don't trigger variable watchers
  (setq doom-modeline-bar-width 3
	doom-modeline-github nil
	doom-modeline-mu4e nil
	doom-modeline-persp-name nil
	doom-modeline-minor-models nil
	doom-modeline-major-mode-icon nil
	doom-modeline-buffer-file-name-style 'relative-from-project)
  :config
  ;; filesize in modeline
  (size-indication-mode +1)

  ;; cursor column in modeline
  (column-number-mode +1)

  ;;
  ;; Extensions
  ;; displays current match and total matches on mode-line
  (use-package anzu)
  ;;  :after-call isearch-mode)

  ;; anzu support for evil-mode
  (with-eval-after-load 'evil
      (use-package evil-anzu)))

(provide 'setup-modeline)
