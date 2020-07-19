(use-package ivy
  :ensure t)

(use-package elpy
  :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :bind (:map elpy-mode-map
              ("s-m" . elpy-shell-send-statement-and-step)
              ("s-M" . elpy-shell-send-group-and-step)
              ("s-[" . elpy-nav-indent-shift-left)
              ("s-]" . elpy-nav-indent-shift-right)
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
   )

  (defun custom-restart-python ()
    (interactive)
    (if (get-buffer "*Python*")
        (let ((kill-buffer-query-functions nil)) (kill-buffer "*Python*")))
    (elpy-shell-send-region-or-buffer))
  )


(provide 'python-custom)
