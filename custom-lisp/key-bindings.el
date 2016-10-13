;; ergoemacs -- partial adoption
;; many, but not all, of these settings follow 'theme standard'
;; (assumes qwerty) at https://ergoemacs.github.io/

;;; define custom functions

;; deleting words without copying them to the kill-ring
;; (http://ergoemacs.org/emacs/emacs_kill-ring.html)
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))


;;; set key bindings

(global-set-key (kbd "s-j") 'backward-char)
(global-set-key (kbd "s-l") 'forward-char)
(global-set-key (kbd "s-i") 'previous-line)
(global-set-key (kbd "s-k") 'next-line)

;; move by word
(global-set-key (kbd "s-u") 'backward-word)
(global-set-key (kbd "s-o") 'forward-word)

;; move up or down a full paragraph
(global-set-key (kbd "s-U") 'backward-paragraph)
(global-set-key (kbd "s-O") 'forward-paragraph)

;; delete previous/next char
(global-set-key (kbd "s-d") 'delete-backward-char)
(global-set-key (kbd "s-f") 'delete-char)

;; delete previous/next word
(global-set-key (kbd "s-e") 'my-backward-delete-word)
(global-set-key (kbd "s-r") 'my-delete-word)

;; move to the beginning/end of line
(global-set-key (kbd "s-H") 'beginning-of-line)
(global-set-key (kbd "s-h") 'end-of-line)

;; select entire buffer
(global-set-key (kbd "s-a") 'mark-whole-buffer)

;; ido kill buffer
(global-set-key (kbd "s-)") 'ido-kill-buffer)

;; set mark
(global-set-key (kbd "s-y") 'set-mark-command)

;; isearch
(global-set-key (kbd "s-;") 'isearch-forward)
(global-set-key (kbd "s-:") 'isearch-backward)
(define-key isearch-mode-map (kbd "s-;") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "s-:") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "s-d") 'isearch-delete-char)

;; split screen
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)

;; ido switch buffer
(global-set-key (kbd "s-b") 'ido-switch-buffer)

;; move to next currently visible window
(global-set-key (kbd "s-`") 'other-window)

;; move to previous/next buffer
(global-set-key (kbd "s-.") 'next-buffer)
(global-set-key (kbd "s-,") 'previous-buffer)

;; move to top/bottom of buffer
(global-set-key (kbd "s-<") 'beginning-of-buffer)
(global-set-key (kbd "s->") 'end-of-buffer)

;; ido find file
(global-set-key (kbd "s-g") 'ido-find-file)

;; quit (e.g., after M-x and isearch)
(define-key minibuffer-local-map (kbd "s-G") 'keyboard-escape-quit)
(define-key isearch-mode-map (kbd "s-G") 'keyboard-quit)

;; key bindings in mini buffer (M-x)
(define-key minibuffer-local-map (kbd "s-j") 'previous-history-element)
(define-key minibuffer-local-map (kbd "s-l") 'next-history-element)

;; select line
(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))
(global-set-key (kbd "s-7") 'select-current-line)

;; find frequently used files quickly
(global-set-key
 (kbd "<f5>")
 (lambda ()
   (interactive)
   (find-file "~/.emacs.d/init.el")))

(global-set-key
 (kbd "<f6>")
 (lambda ()
   (interactive)
   (find-file "~/dropbox/prog/r/commands.R")))

;; disable default key bindings that accomplish same thing as above
(global-unset-key (kbd "M-f")) ; forward-word
(global-unset-key (kbd "M-b")) ; backward-word
(global-unset-key (kbd "M-d")) ; kill-word
(global-unset-key (kbd "M-d")) ; delete-char
(global-unset-key (kbd "C-b")) ; backward-char
(global-unset-key (kbd "C-f")) ; forward-char
(global-unset-key (kbd "C-p")) ; previous-line
(global-unset-key (kbd "C-n")) ; next-line
(global-unset-key (kbd "C-SPC")) ; set-mark-command
(global-unset-key (kbd "s-L")) ; run shell command
(global-unset-key (kbd "s-t")) ; no font
(global-unset-key (kbd "s-n")) ; no new window
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-M")) ; no minimize window
(global-unset-key (kbd "s-D")) ; no 'dired'
(global-unset-key (kbd "s-p")) ;
(global-unset-key (kbd "s-P")) ; no printing


(provide 'key-bindings)
