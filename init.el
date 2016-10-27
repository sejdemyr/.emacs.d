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
(use-package eshell-custom)
(use-package key-bindings)
(use-package functionality)

;; Smart M-x (e.g., command suggestions)
(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

;; auto-completion
(use-package auto-complete
  :ensure t
  :config (ac-config-default))


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
   ess-default-style 'RStudio)         ; rstudio indentation style

  ;; set assignment operator
  (setq ess-S-assign-key (kbd "s-n"))
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
   '(spaceline-highlight-face ((t (:foreground "DarkSlateGray4" :background "black")))))
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


;;; Spelling and syntax checking
(use-package ispell                     ; Spell checking
  :defer t
  :config
  (validate-setq
   ispell-program-name "/usr/local/bin/aspell"
   ispell-silently-savep t              ; Don't ask when saving the private dict
   ;; Increase the height of the choices window to take our header line
   ;; into account.
   ispell-choices-win-default-height 5)
  (unless ispell-program-name
    (warn "No spell checker available.  Install Hunspell or ASpell for OS X.")))

(use-package flyspell                   ; On-the-fly spell checking
  :init
  (dolist (hook '(text-mode-hook message-mode-hook))
    (add-hook hook 'turn-on-flyspell))
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (validate-setq
   flyspell-use-meta-tab nil
   ;; Make Flyspell less chatty
   flyspell-issue-welcome-flag nil
   flyspell-issue-message-flag nil))


;;; LaTeX/AUCTex
(setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)

(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package tex                        ; TeX editing/processing
  :bind (:map TeX-mode-map
              ("s-m" . TeX-command-master)  ; key-binding to compile file
              ("s-M" . TeX-view))
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
  :bind (:map LaTeX-mode-map
              ("s-N" . LaTeX-environment))
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
  :bind (:map reftex-mode-map
              ("s-n" . reftex-citation))
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  ;; Plug into AUCTeX
  (validate-setq
   reftex-plug-into-AUCTeX t
   reftex-insert-label-flags '(t t)     ; Automatically derive labels, and prompt for confirmation
   reftex-default-bibliography '("/Users/simonejdemyr/dropbox/literature/bib-ejdemyr.bib"))
  ;; Provide basic RefTeX support for biblatex
  (unless (assq 'biblatex reftex-cite-format-builtin)
    (add-to-list 'reftex-cite-format-builtin
                 '(biblatex "The biblatex package"
                            ((?\C-m . "\\citep{%l}")
                             (?c . "\\cite{%l}")
                             (?P . "\\citep[][p.\\]{%l}")
                             (?e . "\\citep[e.g.,][]{%l}")
                             (?s . "\\citep[see][]{%l}")
                             (?t . "\\citet{%l}")
                             (?a . "\\citeauthor{%l}")
                             (?y . "\\citeyear{%l}"))))
    (setq reftex-cite-format 'biblatex))

  :diminish reftex-mode)


;;; File handling
(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :config
  ;; Ignore some additional directories and file extensions
  (dolist (name '("*Messages*" "*ESS*"))
    ;; Ignore some additional directories
    (add-to-list 'ignoramus-file-basename-exact-names name))

  (dolist (ext '(".fls" ".out" ".gz" ".log" ".aux"; LaTeX
                 ))
    (add-to-list 'ignoramus-file-endings ext))

  (ignoramus-setup))

(use-package recentf                    ; Save recently visited files
  :init (recentf-mode)
  :bind (("s-w" . recentf-open-files))
  :config
  (validate-setq
   recentf-max-saved-items 200
   recentf-max-menu-items 15
   ;; Cleanup recent files only when Emacs is idle, but not when the mode
   ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
   ;; idles often enough to have the recent files list clean up regularly
   recentf-auto-cleanup 300
   recentf-exclude (list "/\\.git/.*\\'"     ; Git contents
                         "/elpa/.*\\'"       ; Package files
                         "/itsalltext/"      ; It's all text temp files
                         ;; And all other kinds of boring files
                         #'ignoramus-boring-p)))


;;; eshell configuration
(use-package eshell
  :ensure t
  :init
  (require 'em-smart)                        ; em-smart: https://www.masteringemacs.org/article/complete-guide-mastering-eshell
  :config
  (validate-setq
   eshell-where-to-jump 'begin
   eshell-review-quick-commands nil
   eshell-smart-space-goes-to-end t))


;;; Completion and expansion
;; In `completion-at-point', do not pop up completion buffers for less
;; than five candidates.  Cycle instead.
(validate-setq completion-cycle-threshold 5)

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind ("s-'" . hippie-expand)
  :config
  (progn
    (validate-setq hippie-expand-try-functions-list
                   '(try-expand-dabbrev
                     try-expand-dabbrev-all-buffers
                     try-expand-dabbrev-from-kill
                     try-complete-file-name-partially
                     try-complete-file-name
                     try-expand-all-abbrevs
                     try-expand-list
                     try-expand-line
                     try-complete-lisp-symbol-partially
                     try-complete-lisp-symbol))))

(use-package yasnippet                  ; Snippets
  :ensure t
  :defer t)



;;; TO DO: automatically load yasnippet; add extensions
;;; (https://www.emacswiki.org/emacs/Yasnippet)


;;; Markdown/ESS with polymode
(use-package polymode
  :ensure t
  :bind (:map polymode-mode-map
              ("s-9" . ess-render-rmarkdown))  ; render rmd file (https://github.com/vspinu/polymode/issues/30)
  :mode
  (("\\.md\\'" . poly-markdown-mode)
   ("\\.Rmd" . poly-markdown+r-mode))
  :init
  (defun ess-render-rmarkdown ()
    "Compile R markdown (.Rmd). Should work for any output type."
    (interactive)
    ;; Check if attached R-session
    (condition-case nil
        (ess-get-process)
      (error
       (ess-switch-process)))
    (let* ((rmd-buf (current-buffer)))
      (save-excursion
        (let* ((sprocess (ess-get-process ess-current-process-name))
               (sbuffer (process-buffer sprocess))
               (buf-coding (symbol-name buffer-file-coding-system))
               (buffer-file-name-html (concat (file-name-sans-extension buffer-file-name) ".html"))
               (R-cmd
                (format "library(rmarkdown); rmarkdown::render(\"%s\", output_file = 'index.html')"
                        buffer-file-name buffer-file-name-html)))
          (message "Running rmarkdown on %s" buffer-file-name)
          (ess-execute R-cmd 'buffer nil nil)
          (switch-to-buffer rmd-buf)
          (ess-show-buffer (buffer-name sbuffer) nil)))))
  :config
    (require 'poly-R)		; Load necessary modes
    (require 'poly-markdown))


;;; Markdown: markdown-mode
;; http://jblevins.org/projects/markdown-mode/
;; & https://github.com/basille/.emacs.d/blob/master/init.el
(use-package markdown-mode
  :ensure t				; Check and install if necessary
  :commands markdown-mode		; Autoloads for markdown-mode
  :init
  (defun rmd-R-fenced-code-block ()
    "Adds a fenced block for R code in Markdown"
    (interactive)
    (insert "\n```{r}\n\n```\n")
    (previous-line)
    (previous-line))
  (defun rmd-R-inline-code ()
    "Insert inline R code in Markdown"
    (interactive)
    (insert "`r `")
    (backward-char))
  :config
  (progn
    (add-hook 'markdown-mode-hook
	      (lambda ()
		(imenu-add-menubar-index) ; Add imenu
		(local-set-key [s-return] 'rmd-R-fenced-code-block) ; C-return to insert a new R chunk
		(local-set-key [M-return] 'rmd-R-inline-code)))))  ; C-S-return to insert inline R code)


;;; Minibuffer and Helm
(validate-setq
 history-length 1000                    ; Store more history
 use-dialog-box nil                     ; Never use dialogs for minibuffer input
 )

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config (validate-setq savehist-save-minibuffer-history t
                         savehist-autosave-interval 180))

(use-package async                      ; Needed for helm
  :ensure t)

(use-package helm-setup)                ; Custom lisp file with all Helm configs


;;; load final settings
(use-package final-settings)
