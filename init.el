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

;; load some of the custom lisp files early
(use-package appearance)
(use-package key-bindings)
(use-package functionality)

;; Smart M-x (e.g., command suggestions)
(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))


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



;;; LaTeX/AUCTex
(setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)

(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package tex                        ; TeX editing/processing
  :bind (:map TeX-mode-map
              (("s-m" . TeX-command-master)))  ; key-binding to compile file
  :ensure auctex
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode t)
  :config
  (validate-setq
   TeX-parse-self t                      ; parse documents to provide completion for packages, etc.
   TeX-auto-save t                       ; automatically save style information
   TeX-electric-sub-and-superscript t    ; automatically insert braces after sub- and superscripts in math mode
   TeX-electric-math '("$" . "$")
   TeX-quote-after-quote t               ; don't insert magic quotes right away.
   TeX-clean-confirm nil                 ; don't ask for confirmation when cleaning
   TeX-source-correlate-mode t           ; provide forward and inverse search with SyncTeX
   TeX-source-correlate-method 'synctex)
  (setq-default TeX-master nil            ; ask for the master file
                TeX-engine 'xetex         ; use a modern engine
                TeX-PDF-mode t))

(use-package tex-buf                    ; TeX buffer management
  :ensure auctex
  :defer t
  ;; Don't ask for confirmation when saving before processing
  :config (validate-setq TeX-save-query nil))

(use-package tex-style                  ; TeX style
  :ensure auctex
  :defer t
  :config
  ;; Enable support for csquotes
  (validate-setq LaTeX-csquotes-close-quote "}"
                 LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                   ; TeX folding
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode                   ; TeX mode
  :ensure auctex
  :defer t
  :config
  (font-lock-add-keywords 'latex-mode
                          `((,(rx "\\"
                                  symbol-start
                                  "fx" (1+ (or (syntax word) (syntax symbol)))
                                  symbol-end)
                             . font-lock-warning-face))))

(use-package latex                      ; LaTeX editing
  :ensure auctex
  :defer t
  :config
  ;; Teach TeX folding about KOMA script sections
  (validate-setq
   TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                       (,(rx (0+ space) "\\subsection*{") 3)
                       (,(rx (0+ space) "\\subsubsection*{") 4)
                       (,(rx (0+ space) "\\minisec{") 5))
   ;; No language-specific hyphens please
   LaTeX-babel-hyphen "")
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)) ; Easy math input

(use-package auctex-latexmk             ; latexmk command for AUCTeX
  :ensure t
  :defer t
  :after latex
  :config (auctex-latexmk-setup))

(use-package bibtex                     ; BibTeX editing
  :defer t
  :config
  ;; Run prog mode hooks for bibtex
  (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
  ;; Use a modern BibTeX dialect
  (bibtex-set-dialect 'biblatex))

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  ;; Plug into AUCTeX
  (validate-setq
   reftex-plug-into-AUCTeX t)
  :diminish reftex-mode)

(use-package auctex-skim                ; Skim as viewer for AUCTeX
  :after tex)
