;; Configure Evil mode
;;
;; There is no other way to use Emacs!!!
;;

(use-package evil
  :init
  (setq-default
   evil-want-C-d-scroll t
   evil-want-C-u-scroll t)

  :config
  (evil-mode 1)
  (use-package evil-numbers)

  ;; Center screen around a search
  (defadvice
      evil-search-forward
      (after evil-search-forward-recenter activate)
    (recenter))
  (ad-activate 'evil-search-forward)

  (defadvice
      evil-search-next
      (after evil-search-next-recenter activate)
    (recenter))
  (ad-activate 'evil-search-next)

  (defadvice
      evil-search-previous
      (after evil-search-previous-recenter activate)
    (recenter))
  (ad-activate 'evil-search-previous))

;; save point position between sessions
(use-package saveplace
  :config
  (setq-default save-place t))

;; fixes the undo tree behaviour
(use-package undo-tree
  :diminish "")

(provide 'setup-evil)
