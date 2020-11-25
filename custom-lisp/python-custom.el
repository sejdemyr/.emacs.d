(use-package ivy
  :ensure t)

;; pyvenv manages virtual environments
;; in this case we need it to specify from where to install the jedi server via `jedi:install-server`
;; activate a virtualenv using `M-x pyenv-workon`
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1)
  (setenv "WORKON_HOME" "/Users/sejdemyr/xp-env/conda/envs/xp-env")
  )

;; jedi is used for code navigation (e.g., goto-definition)
;; requires `jedi:install-server` (potentially after setting `M-x pyenv-workon`)
;; further config in python-mode below (`my/python-company-jedi-config`)
(use-package company-jedi
  :ensure t
  :defer)

;; enable IDE type features
(use-package elpy
  :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :bind (:map elpy-mode-map
              ("s-m" . elpy-shell-send-statement)
              ("s-M" . elpy-shell-send-group-and-step)
              ("s-[" . elpy-nav-indent-shift-left)
              ("s-]" . elpy-nav-indent-shift-right)
              )
  :config
  (validate-setq
   elpy-modules (delq 'elpy-module-flymake elpy-modules) ; disable flymake to use flycheck instead
   )
  )

(use-package python
  :ensure t
  :mode ("\\.py" . python-mode)
  :config
  (validate-setq
   python-shell-interpreter "/Users/sejdemyr/xp-env/bin/xp-python"
   python-shell-completion-native-enable nil
   python-indent-guess-indent-offset-verbose nil
   python-shell-interpreter-args "-i"
   )

  (defun custom-restart-python ()
    (interactive)
    (if (get-buffer "*Python*")
        (let ((kill-buffer-query-functions nil)) (kill-buffer "*Python*")))
    (elpy-shell-send-region-or-buffer))

  (defun my/python-company-jedi-config ()
    (set (make-local-variable 'company-backends) '(company-jedi))
    (company-mode)
    (local-set-key (kbd "s-p") 'jedi:goto-definition)
    (local-set-key (kbd "s-P") 'jedi:goto-definition-pop-marker))

  (add-hook 'python-mode-hook 'my/python-company-jedi-config)
  )

(use-package highlight-indent-guides
  :config
  (validate-setq
   highlight-indent-guides-method 'character
   highlight-indent-guides-auto-enabled nil
   )
  ;;(add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (set-face-background 'highlight-indent-guides-odd-face "dimgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  )

(use-package flycheck
  :ensure t
  :hook (python-mode . flycheck-mode)
  :config
  (validate-setq
    flycheck-flake8-maximum-line-length 99
  ))


(provide 'python-custom)
