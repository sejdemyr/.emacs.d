;;; init.el --- Simon Ejdemyr (ejdemyr@gmail.com)

;; For inspiration for this setup, I gratefully acknowledge:
;; @technomancy/better-defaults for better defaults
;; @magnars/.emacs.d for directory structure
;; @owainlewis/emacs-color-themes for nice color themes
;; @jwiegley/use-package for easy package install and management
;; @lunaryorn/.emacs.d for getting started with use-package
;; Also see: http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.


;;; Code:


;; turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; set load path to custom lisp and themes
(setq custom-lisp-dir
      (expand-file-name "custom-lisp" user-emacs-directory))
(add-to-list 'load-path custom-lisp-dir)

;; set load path to themes
(add-to-list 'custom-theme-load-path "themes")

;; set package archives
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")))

(package-initialize)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;; customization

;; validate options
(use-package validate
  :ensure t
  :init
  (use-package seq
    :ensure t))

;; use decent initial options
(use-package better-defaults
  :ensure t)

;; keep emacs custom settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; load custom lisp files
(use-package appearance)
(use-package key-bindings)
(use-package functionality)


;;; emacs speaks statistics
(use-package ess-site
  :load-path "ess/lisp/"
  :mode ("\\.R\\'" . R-mode)
  :config
  (validate-setq
   ring-bell-function #'ignore
   ess-ask-for-ess-directory nil
   inferior-R-program-name "/usr/local/bin/R"
   ess-local-process-name "R"
   ansi-color-for-comint-mode 'filter
   comint-scroll-to-bottom-on-input t
   comint-scroll-to-bottom-on-output t
   comint-move-point-for-output t
   ess-default-style 'RStudio)     ; rstudio indentation style

  ;; set assignment operator
  ess-S-assign-key (kbd "s-n")
  (ess-toggle-S-assign-key t)

  ;; disable '_' shortcut
  (ess-toggle-underscore nil)

  ;; set piping operator key binding
  ;; http://emacs.stackexchange.com/questions/8041/how-to-implement-the-piping-operator-in-ess-mode
  (defun then_R_operator ()
    "R - %>% operator or 'then' pipe operator"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (just-one-space 1))
  (define-key ess-mode-map (kbd "s-N") 'then_R_operator)
  (define-key inferior-ess-mode-map (kbd "s-N") 'then_R_operator)

  ;; key binding to evaluate current line or marked region
  (defun my-ess-eval ()
    (interactive)
    (if (and transient-mark-mode mark-active)
        (call-interactively 'ess-eval-region)
      (call-interactively 'ess-eval-line)))
  (add-hook 'ess-mode-hook
            '(lambda()
               (local-set-key (kbd "s-m") 'my-ess-eval)))

  ;; key binding to evaluate entire region (whether marked or not) and step
  (defun my-ess-eval2 ()
    (interactive)
    (call-interactively 'ess-eval-region-or-function-or-paragraph-and-step))
  (add-hook 'ess-mode-hook
            '(lambda()
               (local-set-key (kbd "s-M") 'my-ess-eval2)))

  ;; key binding to load_all() for R devlopment
  (defun my-ess-eval3 ()
    (interactive)
    (call-interactively 'ess-r-devtools-load-package))
  (add-hook 'ess-mode-hook
            '(lambda()
               (local-set-key (kbd "s-B") 'my-ess-eval3)))

  )


;;; mode-line
(use-package spaceline-config
  :ensure spaceline
  :init
  (custom-set-faces
   '(spaceline-highlight-face ((t (:foreground "FireBrick" :background "black")))))
  (setq powerline-default-separator 'nil)
  :config
  (spaceline-helm-mode)

  (spaceline-compile
   'lunaryorn
   ;; Left side of the mode line (all the important stuff)
   '(((buffer-modified buffer-id input-method) :face highlight-face)
     anzu
     ;;'(buffer-id remote-host buffer-encoding-abbrev)
     ((point-position line-column buffer-position) ; add selection-info within paren if want info about number of chars in selection
      :separator " | ")
     major-mode
     process
     (flycheck-error flycheck-warning flycheck-info)
     (python-pyvenv :fallback python-pyenv)
     ((which-function projectile-root) :separator " @ ")
     ((minor-modes :separator spaceline-minor-modes-separator) :when active)
     nyan-cat)
   ;; Right segment (the unimportant stuff)
   '((version-control :when active)
     battery))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-lunaryorn)))))


;;; support for editing html, css, and js code in one buffer
(use-package multi-web-mode
  :ensure t
  :init
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                    (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  :config
  (multi-web-global-mode 1))



;;; SQL setup
(setq sql-postgres-program "/usr/local/Cellar/postgresql/9.5.4_1/bin/psql")

;; Automatic uppercase for key words: sqlup-mode (https://github.com/Trevoke/sqlup-mode.el)
(use-package sqlup-mode
  :ensure t
  :init
  ;; Capitalize keywords in SQL mode
  (add-hook 'sql-mode-hook 'sqlup-mode)
  ;; Capitalize keywords in an interactive session (e.g. psql)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
  ;; Set key binding  to use sqlup on a region
  (global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region))

;; key binding for evaluating a paragraph
(defun my-sql-eval ()
  (interactive)
  (call-interactively 'sql-send-paragraph))
(add-hook 'sql-mode-hook
          '(lambda()
             (local-set-key (kbd "s-m") 'my-sql-eval)))
