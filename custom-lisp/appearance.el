
;; no beeping, no startup screen or message, and no scratch message
(validate-setq ring-bell-function #'ignore
               inhibit-startup-screen t
               initial-scratch-message "")

;; maximize size on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; show parentheses
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; color for matching parentheses
;; (set-face-background 'show-paren-match-face "#3D3D3D")

; show column number in mode line by default
(setq column-number-mode t)

(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))

;; italics for comments
(custom-set-faces
'(font-lock-comment-face ((t (:normal t)))))   ;; :italics

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

;; color of minibuffer prompt
(custom-set-faces
'(minibuffer-prompt ((t (:foreground "DarkSlateGray3"))))
)

;; Set default font
(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 140
                    :weight 'normal
                    :width 'normal)


;; macOS title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))


(provide 'appearance)
