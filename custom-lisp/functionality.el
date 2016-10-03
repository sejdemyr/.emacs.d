
;; Smart M-x (e.g., command suggestions)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

; control scrolling speed
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; don't line wrap
(set-default 'truncate-lines -1)

;; delete selected text when typing
(delete-selection-mode t)

; switch between buffers using M-tab
(global-set-key [M-tab] 'other-window)


(provide 'functionality)
