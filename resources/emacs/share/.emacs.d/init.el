;; =========================================
;;
;; ========== Emacs configuration ==========
;;
;; =========================================


;; --------------------
;; INIT
;; --------------------

;; Enable byte compilation before loading anything
(custom-set-variables
 '(auto-compile-on-load-mode t)
 '(auto-compile-on-save-mode t)
 )

;; Setup package manager to also use melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
    (package-initialize))

;; Load custom variables
(setq custom-file "~/.emacs.d/lisp/customize.el")
(load custom-file)

;; --------------------
;; FUNCTIONS
;; --------------------

(defun insert-header-guard ()
  (interactive)
  (save-excursion
    (when (buffer-file-name)
      (let*
          ((name (file-name-nondirectory buffer-file-name))
           (macro (replace-regexp-in-string
                   "\\." "_"
                   (replace-regexp-in-string
                    "-" "_"
                    (upcase name)))))
        (goto-char (point-min))
        (insert "#ifndef " macro "\n")
        (insert "# define " macro "\n\n")
        (insert "\n\n#endif /* !" macro " */\n")))))

(defun insert-shebang (path)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "#! " path  "\n\n")
    (save-buffer)
    (call-process "chmod" nil t nil "+x" (buffer-file-name))))

(defun tab-indent-mode (default)
  (let (setx)
    (if default
        (setq setx 'set-default)
      (setq setx 'set))
    (funcall setx 'indent-tabs-mode t)
    (funcall setx 'c-basic-offset 8)
    (funcall setx 'sh-basic-offset 8)))

(defun space-indent-mode (default)
  (let (setd)
    (if default
        (setq setx 'set-default)
      (setq setx 'set))
    (funcall setx 'indent-tabs-mode nil)
    (funcall setx 'c-basic-offset 4)
    (funcall setx 'sh-basic-offset 4)))

(defun toggle-indent-mode ()
  (interactive)
  (if indent-tabs-mode
      (progn
        (message "Space mode")
        (space-indent-mode nil))
    (progn
      (message "Tab mode")
      (tab-indent-mode nil))))

(defun global-space-indent ()
  (interactive)
  (space-indent-mode t)
  (space-indent-mode nil))

(defun local-space-indent ()
  (interactive)
  (apply-partially 'space-indent-mode nil))

(defun global-tab-indent ()
  (interactive)
  (tab-indent-mode t)
  (tab-indent-mode nil))

(defun local-tab-indent ()
  (interactive)
  (apply-partially 'tab-indent-mode nil))

;; --------------------
;; PARAMETERS
;; --------------------

;; Set 'y or n' instead of 'yes or no'
(fset 'yes-or-no-p 'y-or-n-p)
;; Gdb
(setq-default gdb-many-windows t)
;; Compilation window
(setq compilation-window-height 12)
(setq compilation-scroll-output t)
;; Enable upcase
(put 'upcase-region 'disabled nil)
;; Indentation
(space-indent-mode t)
;; Whitespace only for programming
(add-hook 'prog-mode-hook 'whitespace-mode)
;; Disable ring bell
(setq ring-bell-function 'ignore)

;; --------------------
;; KEYS
;; --------------------

;; Remap escape sequences to correct keybindings
(defun portable-term-setup-hook ()
  (define-key function-key-map "\e[1;3A" [M-up])
  (define-key function-key-map "\e[1;3B" [M-down])
  (define-key function-key-map "\e[1;3C" [M-right])
  (define-key function-key-map "\e[1;3D" [M-left])
  (define-key function-key-map "\e[1;5A" [C-up])
  (define-key function-key-map "\e[1;5B" [C-down])
  (define-key function-key-map "\e[1;5C" [C-right])
  (define-key function-key-map "\e[1;5D" [C-left]))
(add-hook 'term-setup-hook 'portable-term-setup-hook)
;; Replace
(global-set-key (kbd "C-r") 'replace-string)
(global-set-key (kbd "C-c r") 'query-replace-regexp)
;; Search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)
;; Undo
;;(global-set-key (kbd "C-q") 'undo)
;; Move to windows
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-left] 'windmove-left)
;; Goto line
(global-set-key (kbd "M-l") 'goto-line)
;; Split windows
(global-set-key (kbd "<f1>") 'delete-other-windows)
(global-set-key (kbd "<f2>") 'split-window-horizontally)
(global-set-key (kbd "<f3>") 'split-window-vertically)
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x \-") 'split-window-vertically)
;; Style
(global-set-key (kbd "<f4>") 'toggle-indent-mode)
(global-set-key (kbd "C-x <f4>") 'whitespace-mode)
;; Compile
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-x <f5>") 'recompile)
;; Cscope
(global-set-key (kbd "<f9>") 'cscope-set-initial-directory)
(global-set-key (kbd "C-x <f9>") 'cscope-create-list-of-files-to-index)
(global-set-key (kbd "<f10>") 'cscope-find-this-symbol)
(global-set-key (kbd "C-x <f10>") 'cscope-find-global-definition)
(global-set-key (kbd "<f11>") 'cscope-prev-symbol)
(global-set-key (kbd "<f12>") 'cscope-next-symbol)
;; Imenu
(global-set-key (kbd "M-,") 'idomenu)
;; Comments
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
;; C keys
(add-hook 'c-mode-hook
          (lambda ()
            ;; Gdb
            (local-set-key (kbd "<f7>") 'gdb))
          )

;; --------------------
;; CODING
;; --------------------

;; C
(setq c-default-style "linux")
;;(setq c-syntactic-indentation nil)

;; Header guard
(add-hook 'find-file-hooks
          (lambda ()
            (when (and (memq major-mode '(c-mode c++-mode)) (equal (point-min) (point-max)) (string-match ".*\\.hh?" (buffer-file-name)))
              (insert-header-guard)
              (forward-line 4))))

;; Shebangs
(add-hook 'sh-mode-hook
          (lambda ()
            (when (equal (point-min) (point-max))
              (insert-shebang "/bin/sh")
              (goto-char (point-max)))))
(add-hook 'perl-mode-hook
          (lambda ()
            (when (equal (point-min) (point-max))
              (insert "use strict;\nuse warnings;\n\n")
              (insert-shebang "/usr/bin/env perl")
              (goto-char (point-max)))))
(add-hook 'awk-mode-hook
          (lambda ()
            (when (equal (point-min) (point-max))
              (insert-shebang "/usr/bin/awk -f")
              (goto-char (point-max)))))
(add-hook 'python-mode-hook
          (lambda ()
            (when (equal (point-min) (point-max))
              (insert-shebang "/usr/bin/env python")
              (goto-char (point-max)))))

;; Ignore some buffers
(progn
  (defun yic-ignore (str)
    (or
     (string-match "\\*Buffer List\\*" str)
     (string-match "^TAGS" str)
     (string-match "^\\*Messages\\*$" str)
     (string-match "^\\*scratch\\*$" str)
     (string-match "^\\*Completions\\*$" str)
     (string-match "^ " str)

     (memq str
           (mapcar
            (lambda (x)
              (buffer-name
               (window-buffer
                (frame-selected-window x)
                )
               )
              )
            (visible-frame-list)
            )
           )
     )
    )

  (defun yic-next (ls)
    (let* ((ptr ls)
           bf bn go
           )
      (while (and ptr (null go))
        (setq bf (car ptr)  bn (buffer-name bf))
        (if (null (yic-ignore bn))
            (setq go bf)
          (setq ptr (cdr ptr))
          )
        )
      (if go (switch-to-buffer go))
      )
    )

  (defun yic-prev-buffer ()
    (interactive)
    (yic-next (reverse (buffer-list)))
    )

  (defun yic-next-buffer ()
    (interactive)
    (bury-buffer (current-buffer))
    (yic-next (buffer-list))
    )

  (global-set-key [(control x) (left)] 'yic-prev-buffer)
  (global-set-key [(control x) (right)] 'yic-next-buffer)
  (global-set-key [(control x) (control left)] 'yic-prev-buffer)
  (global-set-key [(control x) (control right)] 'yic-next-buffer)
  )

(progn
  (defun kill-region-or-pword (&optional arg)
    (interactive "p")
    (if (use-region-p)
        (kill-region (mark) (point))
      (let (count)
        (dotimes (count arg)
          (if (bolp)
              (delete-char 1)
            (kill-region (max (save-excursion (backward-word)(point))
                              (line-beginning-position))
                         (point)))))))

  (global-set-key "\C-w" 'kill-region-or-pword)
  )

(defun check-large-file-hook ()
  "If a file is over a given size, turn off syntax highlighting"
  (when (> (buffer-size) (* 10240 1024))
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hooks 'check-large-file-hook)

;;-----------------------------------
;; MISC
;;-----------------------------------

;; Lines which contain `FIXME:'
(setq font-lock-fixme1-face (make-face 'font-lock-fixme1-face)
      font-lock-fixme0-face (make-face 'font-lock-fixme0-face))
(set-face-foreground 'font-lock-fixme1-face "yellow")
(set-face-foreground 'font-lock-fixme0-face "blue")

(set-face-bold 'font-lock-fixme0-face t)
(set-face-underline 'font-lock-fixme0-face t)
(font-lock-add-keywords 'c++-mode
                        `(("\\<\\(FIXME:\\) \\(.*$\\)" 1 font-lock-fixme0-face prepend)
                          ("\\<\\(FIXME:\\) \\(.*$\\)" 2 font-lock-fixme1-face t)))
(font-lock-add-keywords 'c-mode
                        `(("\\<\\(FIXME:\\) \\(.*$\\)" 1 font-lock-fixme0-face prepend)
                          ("\\<\\(FIXME:\\) \\(.*$\\)" 2 font-lock-fixme1-face t)))

;; Lines which contain `TODO:'
(setq font-lock-todo0-face (make-face 'font-lock-todo0-face)
      font-lock-todo1-face (make-face 'font-lock-todo1-face))
(set-face-foreground 'font-lock-todo1-face "green")
(set-face-foreground 'font-lock-todo0-face "red")
(set-face-bold 'font-lock-todo0-face t)
(set-face-bold 'font-lock-todo1-face t)
(set-face-underline 'font-lock-todo0-face t)
(font-lock-add-keywords 'c++-mode
                        `(("\\<\\(TODO:\\) \\(.*$\\)" 1 font-lock-todo0-face prepend)
                          ("\\<\\(TODO:\\) \\(.*$\\)" 2 font-lock-todo1-face t)))
(font-lock-add-keywords 'c-mode
                        `(("\\<\\(TODO:\\) \\(.*$\\)" 1 font-lock-todo0-face prepend)
                          ("\\<\\(TODO:\\) \\(.*$\\)" 2 font-lock-todo1-face t)))

;; Additional custom addons
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Color diff
(eval-after-load 'diff-mode
  '(progn
     (set-face-attribute 'diff-added nil :foreground "green4" :background "light")
     (set-face-attribute 'diff-removed nil :foreground "red2" :background "light")
     (set-face-attribute 'diff-changed nil :foreground "purple" :background "light")
     (set-face-attribute 'diff-header nil :foreground "blue" :background "light")))

;; --------------------
;; THEME
;; --------------------

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq nox-theme 'monokai-nox)
(setq x-theme 'monokai-x)

(if (window-system)
    (load-theme x-theme t)
  (load-theme nox-theme t))

;; --------------------
;; EXTENSION MODE
;; --------------------

(add-to-list 'auto-mode-alist '("\\.l$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.y$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ll$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.yy$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.xcc$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.xhh$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.pro$" . sh-mode))
(add-to-list 'auto-mode-alist '("configure$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Drakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ect$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.dust$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("^[^\n]\\{80\\}\\(.*\\)$"
                1 font-lock-warning-face prepend)))))
