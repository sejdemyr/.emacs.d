
;; no beeping
(setq ring-bell-function 'ignore)

;; default window width and height
(defun custom-set-frame-size ()
  (add-to-list 'default-frame-alist '(height . 58))
  (add-to-list 'default-frame-alist '(width . 196)))
(custom-set-frame-size)
(add-hook 'before-make-frame-hook 'custom-set-frame-size)

; show parentheses
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


(provide 'appearance)
