;;; init.el --- Simon Ejdemyr (ejdemyr@gmail.com)

;; For inspiration for this setup, I gratefully acknowledge:
;; @technomancy/better-defaults for better defaults
;; @magnars/.emacs.d for directory structure
;; @owainlewis/emacs-color-themes for nice color themes
;; @jwiegley/use-package for easy package install and management
;; @lunaryorn/.emacs.d for getting starteduse-package
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
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; path to R and other programs
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

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

;; Simplify restart
(use-package restart-emacs
  :ensure t)

;; Smart M-x (e.g., command suggestions)
(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

;; auto-completion
(use-package company
  :ensure t
  :init
  (global-company-mode)
  ;; disable for R-mode
  (add-hook 'R-mode-hook (lambda () (company-mode -1)))
  :bind (:map company-active-map
         ("s-k" . company-select-next)
         ("s-i" . company-select-previous)
         ("s-a" . company-abort)
         ("s-l" . company-complete)
         )
  :config
  (validate-setq
   company-idle-delay 0.2
   company-minimum-prefix-length 1
   company-selection-wrap-around t
   company-tooltip-limit 20
   )
  )



;;; emacs speaks statistics
(use-package ess
  :ensure t
  :init
  (require 'ess-site)
  ;; key binding for insert-assign that doesn't add extra spaces:
  (defun r-insert-assign-space-aware ()
    (interactive)
    (just-one-space 1)
    (insert "<-")
    (just-one-space 1))
  ;; key binding for pipe:
  (defun r-pipe-operator ()
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (just-one-space 1))
  ;; key binding for evaluating line or selected text:
  (defun r-eval-line-or-selected ()
    (interactive)
    (if (and transient-mark-mode mark-active)
        (call-interactively 'ess-eval-region)
      (call-interactively 'ess-eval-line)))
  :mode ("\\.R\\'" . R-mode)
  :bind (:map ess-r-mode-map
         ("s-n" . r-insert-assign-space-aware)
         ("s-N" . r-pipe-operator)
         ("s-m" . r-eval-line-or-selected)
         ("s-M" . ess-eval-region-or-function-or-paragraph-and-step)
         :map inferior-ess-r-mode-map
         ("s-n" . r-insert-assign-space-aware)
         ("s-N" . r-pipe-operator)
         ("s-m" . r-eval-line-or-selected)
         ("s-M" . ess-eval-region-or-function-or-paragraph-and-step))
  :config
  (validate-setq
   ring-bell-function #'ignore
   ess-ask-for-ess-directory nil
   ;;inferior-R-program-name "/usr/local/bin/R"
   inferior-R-program-name "/Users/sejdemyr/xp-env/bin/xp-R"
   ess-local-process-name "R"
   ansi-color-for-comint-mode 'filter
   comint-scroll-to-bottom-on-input t
   comint-scroll-to-bottom-on-output t
   comint-move-point-for-output t
   ess-default-style 'RStudio)         ; rstudio indentation style
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
(setq sql-postgres-program "/usr/local/Cellar/postgresql/9.6.1/bin/psql")

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


;; ;;; Spelling and syntax checking
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
   reftex-default-bibliography '("/Users/sejdemyr/Dropbox/literature/bib-ejdemyr.bib"))
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

(use-package auctex-skim                ; Skim as viewer for AUCTeX
  :after tex)


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
  :config
  (validate-setq
   recentf-max-saved-items 500
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

(use-package yasnippet                  ; Snippets (https://www.emacswiki.org/emacs/Yasnippet)
  :bind (("s-t" . yas-expand)
         ("s-T" . yas-next-field-or-maybe-expand))
  :ensure t
  :defer t
  :config
  (yas-global-mode 1))


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
         	(local-set-key [M-return] 'rmd-R-inline-code))))    ; C-S-return to insert inline R code)
   )

;;; Markdown/ESS with polymode
(use-package poly-R
  :ensure t
  )

(use-package poly-markdown
  :ensure t
  )

(use-package polymode
  :ensure t
  :bind (:map polymode-mode-map
              ("s-9" . ess-render-rmarkdown))  ; render rmd file (https://github.com/vspinu/polymode/issues/30)
  :mode
  (("\\.md\\'" . poly-markdown-mode)
   ("\\.Rmd" . poly-markdown+r-mode)
   ("\\.py" . poly-sql-mode))
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
                (format "library(rmarkdown); rmarkdown::render(\"%s\", output_file = \"%s\")"   ;; output_file = 'index.html')"
                        buffer-file-name buffer-file-name-html)))
          (message "Running rmarkdown on %s" buffer-file-name)
          (ess-execute R-cmd 'buffer nil nil)
          (switch-to-buffer rmd-buf)
          (ess-show-buffer (buffer-name sbuffer) nil)))))

  ;;;;; NOTE: Polymode has been refactored without backward compatability for some of the Python/SQL modes below

  ;; (defcustom pm-host/python
  ;;   (pm-bchunkmode "python"
  ;;                  :mode 'python-mode
  ;;                  :font-lock-narrow nil)
  ;;   "Python host chunkmode"
  ;;   :group 'hostmodes
  ;;   :type 'object)

  ;; (defcustom pm-inner/sql
  ;;   (pm-hbtchunkmode "sql"
  ;;                    :mode 'sql-mode
  ;;                    :head-reg  "r\"\"\""
  ;;                    :tail-reg  "\"\"\"")
  ;;   "sql typical chunk."
  ;;   :group 'innermodes
  ;;   :type 'object)

  ;; (defcustom pm-poly/sql
  ;;   (pm-polymode-one "sql"
  ;;                    :hostmode 'pm-host/python
  ;;                    :innermode 'pm-inner/sql)
  ;;   "SQL typical polymode."
  ;;   :group 'polymodes
  ;;   :type 'object)

  ;; (define-polymode poly-sql-mode pm-poly/sql)
  ;; (add-to-list 'auto-mode-alist '("\\.py" . poly-sql-mode))

  (defun remove-electric-indent-mode ()
    (electric-indent-local-mode -1))

  (add-hook 'sql-mode-hook 'remove-electric-indent-mode)
  ;;(define-key sql-mode-map (kbd "RET") 'electric-newline-and-maybe-indent)
  ;;(define-key sql-mode-map (kbd "RET") 'ess-noweb-newline)

  (setq-default tab-width 4)
  (setq markdown-enable-math t)


  :config
  (require 'poly-R)		; Load necessary modes
  (require 'poly-markdown)
  )



;;; Minibuffer and Helm
(validate-setq
 history-length 3000                    ; Store more history
 use-dialog-box nil                     ; Never use dialogs for minibuffer input
 )

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config (validate-setq savehist-save-minibuffer-history t
                         savehist-autosave-interval 180))

(use-package async                      ; Needed for helm
  :ensure t)

(use-package helm-setup)                ; Custom lisp file with all Helm configs


;;; Python & elpy
(use-package python-custom)             ; See custom lisp files


;;; Stan
(use-package stan-mode
  :ensure t)


;;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))


;;; load final settings
(use-package final-settings)
(put 'dired-find-alternate-file 'disabled nil)


;;; Indenting ------------------------
(defun indent-region-custom(numSpaces)
  (progn
    ;; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))
    ;; if there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end))
      )

    (save-excursion ; restore the position afterwards
      (goto-char regionStart) ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd) ; go to the end of region
      (setq end (line-end-position)) ; save the end of the line

      (indent-rigidly start end numSpaces) ; indent between start and end
      (setq deactivate-mark nil) ; restore the selected region
      )
    )
  )

(defun untab-region (N)
  (interactive "p")
  (indent-region-custom -1)
  )

(defun tab-region (N)
  (interactive "p")
  (if (active-minibuffer-window)
      (minibuffer-complete)    ; tab is pressed in minibuffer window -> do completion
    (if (use-region-p)    ; tab is pressed is any other buffer -> execute with space insertion
        (indent-region-custom 1) ; region was selected, call indent-region-custom
      (insert "    ") ; else insert a space as expected
      )
    )
  )

(global-set-key (kbd "s-[") 'untab-region)
(global-set-key (kbd "s-]") 'tab-region)

;; Threshold for splitting windows horizontally (avoid)
(setq split-height-threshold 1)

;; Threshold for splitting windows vertically
(setq split-width-threshold 260)
