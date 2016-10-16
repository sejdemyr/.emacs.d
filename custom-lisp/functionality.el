

;; control scrolling speed
(validate-setq
 scroll-conservatively 1000          ; Never recenter the screen while scrolling
 scroll-error-top-bottom t           ; Move to beg/end of buffer before signalling an error
 ;; These settings make trackpad scrolling on OS X much more predictable
 ;; and smooth
 mouse-wheel-progressive-speed nil
 mouse-wheel-scroll-amount '(3))

;; delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; don't line wrap
(set-default 'truncate-lines -1)

;; delete selected text when typing
(delete-selection-mode t)


(provide 'functionality)
