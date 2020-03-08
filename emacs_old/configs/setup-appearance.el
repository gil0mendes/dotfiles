;; Apperance configurations
;;

;; Add custom theme folder to allow us specify our theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Set beautiful dark theme
(load-theme 'exotica t)

;; Font
(when (member "JetBrains Mono" (font-family-list))
  (set-face-attribute 'default nil :font "JetBrains Mono 14"))

;; Make cursor blink 10 times and stop
(blink-cursor-mode -1)

;; Show line numbers
(global-linum-mode t)

;; set line spacing to the font size
(setq-default line-spacing 1)

;; Pretty icons
(use-package all-the-icons)
;; MUST DO M-x all-the-icons-install-fonts after

;; Hide toolbar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Show parens and other paris
(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;; add color to delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Pretty symbols
;;
;; Changes lambda to an actucal symbol and a few others as well
(when window-system
  (use-package pretty-mode
    :config
    (global-pretty-mode t)))

;; Visualize whitespaces
(require 'whitespace)

(setq whitespace-style '(face empty tabs tab-mark lines-tail trailing))

;; set the white space color
(custom-set-faces '(whitespaces-tab ((t (:foreground "#6f6e75")))))

;; make whitespace-mode use just basic coloring
;;(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
  '(
    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [182 10]) ; 10 LINE FEED
    (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    ))

(global-whitespace-mode t)

(provide 'setup-appearance)
