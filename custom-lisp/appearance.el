
;; no beeping, no startup screen, and no scratch message
(validate-setq ring-bell-function #'ignore
               inhibit-startup-screen t
               initial-scratch-message "")

;; maximize size on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; start with an eshell process running
(add-hook 'emacs-startup-hook 'eshell)

;; show parentheses
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; color for matching parentheses
(set-face-background 'show-paren-match-face "#3D3D3D")

; show column number in mode line by default
(setq column-number-mode t)

;; load theme
(load-theme 'brin t)

;; cursor style
(setq-default cursor-type 'bar)

;; color of border that separates buffers
(set-face-foreground 'vertical-border "grey22")

;; line numbers in display margin
(use-package nlinum
  :ensure t
  :config
  (validate-setq nlinum-format "%d "))

;; format of frame title in title bar
(setq frame-title-format "%b")

;; Shell mode
;;(custom-set-faces
;; '(comint-highlight-prompt ((t (:foreground "green"))))
;; '(minibuffer-prompt ((t (:foreground "green"))))
;; )


(provide 'appearance)
