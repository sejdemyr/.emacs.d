
;; A number of setups for helm -- a powerful minibuffer input framework

(use-package helm
  :ensure t
  :bind
  (:map helm-buffer-map
        ("s-k" . helm-next-line)
        ("s-i" . helm-previous-line))
  :init
  (helm-mode 1)
  (with-eval-after-load 'helm-config
    (warn "`helm-config' loaded! Get rid of it ASAP!"))
  :config
  ;; Split inside selected window with Helm
  (validate-setq helm-split-window-in-side-p t)
  :diminish helm-mode)

(use-package helm-command               ; Command execution with Helm
  :ensure helm
  :defer t
  :bind (("M-x" . helm-M-x)))

;; Configure `display-buffer' behaviour for some special buffers.
(validate-setq
 display-buffer-alist
 `(
   ;; Give Helm Help a non-side window because Helm has very peculiar ideas
   ;; about how to display its help
   (,(rx bos "*Helm Help" (* nonl) "*" eos)
    (display-buffer-use-some-window
     display-buffer-pop-up-window))
   ;; Nail Helm to the side window
   (,(rx bos "*" (* nonl) "helm" (* nonl) "*" eos)
    (display-buffer-in-side-window)
    (side . bottom)
    (window-height . 0.4)
    (window-width . 0.6))
   ;; Put REPLs and error lists into the bottom side window
   (,(rx bos
         (or "*Help"                         ; Help buffers
             "*Warnings*"                    ; Emacs warnings
             "*Compile-Log*"                 ; Emacs byte compiler log
             "*compilation"                  ; Compilation buffers
             "*Flycheck errors*"             ; Flycheck error list
             "*shell"                        ; Shell window
             "*sbt"                          ; SBT REPL and compilation buffer
             "*ensime-update*"               ; Server update from Ensime
             "*SQL"                          ; SQL REPL
             "*Cargo"                        ; Cargo process buffers
             (and (1+ nonl) " output*")      ; AUCTeX command output
             ))
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (side            . bottom)
    (reusable-frames . visible)
    (window-height   . 0.33))
   ;; Let `display-buffer' reuse visible frames for all buffers.  This must
   ;; be the last entry in `display-buffer-alist', because it overrides any
   ;; later entry with more specific actions.
   ("." nil (reusable-frames . visible))))

(use-package helm-buffers               ; Manage buffers with Helm
  :ensure helm
  :defer t
  :bind (("s-b" . helm-mini))
  :config (setq helm-buffers-fuzzy-matching t))

(use-package helm-files                 ; Manage files with Helm
  :ensure helm
  :defer t
  :bind
  (("s-w" . helm-recentf)
   ;;("s-g" . helm-find-files)
   )
  :config
  (validate-setq
   helm-recentf-fuzzy-match t))

(use-package helm-imenu                 ; Jump to tags with Helm
  :ensure helm
  :defer t
  :bind (("s-5" . helm-imenu))
  :config
  (validate-setq helm-imenu-fuzzy-match t))

(use-package helm-ring                  ; Browse rings and registers with Helm
  :ensure helm
  :defer t
  :bind (("s-V" . helm-show-kill-ring)))

(use-package helm-flyspell              ; Helm interface to Flyspell
  :ensure t
  :after flycheck
  :bind
  (:map flyspell-mode-map
        ("s-1" . helm-flyspell-correct)))

(use-package helm-eshell
  :ensure helm
  :defer t
  :init
  (add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "s-k") 'helm-eshell-history))))

;; additional key bindings to go to previous/next line in helm mode
;; maps
(define-key helm-map (kbd "s-k") 'helm-next-line)
(define-key helm-map (kbd "s-i") 'helm-previous-line)

;; color of highlighted selection
(set-face-attribute 'helm-selection nil
                    :background "grey45")

(provide 'helm-setup)
