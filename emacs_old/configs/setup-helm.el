;; Configure the help package
;;
;; see: https://tuhdo.github.io/helm-intro.html

(use-package helm
  :diminish ""
  :config

  (setq helm-move-to-line-cycle-in-source t      ; move to end or beginning og source when reaching top or bottom of source
        helm-split-window-default-side 'other
        helm-split-window-in-side-p t            ; open helm buffer inside current window, not occupy whole other window
        helm-display-header-line nil
        helm-candidate-number-limit 200
        helm-M-x-requires-pattern 0
        helm-net-prefer-curl-p t
        helm-buffer-max-length nil
        helm-autoresize-max-height 30
        helm-autoresize-min-height 30)

  (set-face-attribute 'helm-source-header nil :height 0.1)

  (helm-mode)
  (helm-autoresize-mode t))

(use-package helm-ag
  :after helm
  :config
  (setq helm-ag-base-command "rg --smart-case --no-heading --vimgrep"))

(provide 'setup-helm)
