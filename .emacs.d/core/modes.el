;; #############################################################################
;; # MODES
;; #############################################################################

;; -----------------------------------------------------------------------------
;; Awk
;; -----------------------------------------------------------------------------

(add-hook 'awk-mode-hook
          (lambda ()
            (when (equal (point-min) (point-max))
              (insert-shebang "/usr/bin/awk -f")
              (goto-char (point-max)))))

;; -----------------------------------------------------------------------------
;; C/C++
;; -----------------------------------------------------------------------------

;; Header guards for c/c++
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

(add-hook 'find-file-hooks
          (lambda ()
            (when (and (memq major-mode '(c-mode c++-mode)) (equal (point-min) (point-max)) (string-match ".*\\.hh?" (buffer-file-name)))
              (insert-header-guard)
              (forward-line 4))))

;; C keys
(add-hook 'c-mode-hook
          (lambda ()
            ;; Gdb
            (local-set-key (kbd "<f7>") 'gdb)
            (local-set-key (kbd "<f9>") 'cscope-set-initial-directory)
            (local-set-key (kbd "C-x <f9>") 'cscope-create-list-of-files-to-index)
            (local-set-key (kbd "<f10>") 'cscope-find-this-symbol)
            (local-set-key (kbd "C-x <f10>") 'cscope-find-global-definition)
            (local-set-key (kbd "<f11>") 'cscope-prev-symbol)
            (local-set-key (kbd "<f12>") 'cscope-next-symbol))
          )

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

;; Additional extensions
(add-to-list 'auto-mode-alist '("\\.l$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.y$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ll$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.yy$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.xcc$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.xhh$" . c++-mode))

;; -----------------------------------------------------------------------------
;; Csharp
;; -----------------------------------------------------------------------------

(use-package csharp-mode
  :config
  (omnisharp-mode)
  (flycheck-mode)
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)
;;   ;csharp-mode README.md recommends this too
;;   ;(electric-pair-mode 1)       ;; Emacs 24
;;   ;(electric-pair-local-mode 1) ;; Emacs 25
  :mode ("\\.cs$" . csharp-mode)
  :bind (("<f6>" . omnisharp-run-code-action-refactoring))
  :after (flycheck omnisharp))

;; -----------------------------------------------------------------------------
;; Git
;; -----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; -----------------------------------------------------------------------------
;; HTML/JS
;; -----------------------------------------------------------------------------

(use-package web-mode
  :init
  (setq company-tooltip-align-annotations t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :bind (("TAB" . company-indent-or-complete-common))
  :mode (("\\.ect$" . web-mode)
         ("\\.html$" . web-mode)
         ("\\.dust$" . web-mode)
         ("\\.jsx?$" . web-mode)))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
(with-eval-after-load 'flycheck
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

;; -----------------------------------------------------------------------------
;; Lua
;; -----------------------------------------------------------------------------

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode))

;; -----------------------------------------------------------------------------
;; Perl
;; -----------------------------------------------------------------------------

(add-hook 'perl-mode-hook
          (lambda ()
            (when (equal (point-min) (point-max))
              (insert "use strict;\nuse warnings;\n\n")
              (insert-shebang "/usr/bin/env perl")
              (goto-char (point-max)))))

;; -----------------------------------------------------------------------------
;; PHP
;; -----------------------------------------------------------------------------

(use-package php-mode
  :config
  (company-mode +1)
  :mode ("\\.php$" . php-mode))

;; -----------------------------------------------------------------------------
;; Python
;; -----------------------------------------------------------------------------

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  ;; Disable elpy indent to not overwrite wind move bindings
  :bind (:map elpy-mode-map
              ("<M-down>" . nil)
              ("<M-up>" . nil)
              ("<M-left>" . nil)
              ("<M-right>" . nil))
  :after (flycheck))

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; -----------------------------------------------------------------------------
;; Ruby
;; -----------------------------------------------------------------------------

(use-package ruby-mode
  :mode (("\\.rb$" . ruby-mode)
         ("Drakefile" . ruby-mode)))

;; -----------------------------------------------------------------------------
;; Rust
;; -----------------------------------------------------------------------------

(use-package flycheck-rust)
(use-package company-racer)
(use-package racer)

(use-package rust-mode
  :init
  (setq company-tooltip-align-annotations t)
  :bind (:map rust-mode-map
              ("<f5>" . rust-run))
  :hook ((rust-mode . racer-mode)
         (racer-mode . company-mode)))

;; -----------------------------------------------------------------------------
;; Shell
;; -----------------------------------------------------------------------------

(add-hook 'sh-mode-hook
          (lambda ()
            (when (equal (point-min) (point-max))
              (insert-shebang "/bin/sh")
              (goto-char (point-max)))))

(add-to-list 'auto-mode-alist '("\\.pro$" . sh-mode))
(add-to-list 'auto-mode-alist '("configure$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

;; -----------------------------------------------------------------------------
;; Typescript
;; -----------------------------------------------------------------------------

(use-package tide
  :init
  (setq company-tooltip-align-annotations t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  :mode ("\\.tsx?$" . typescript-mode))

;; -----------------------------------------------------------------------------
;; YAML
;; -----------------------------------------------------------------------------

(use-package yaml-mode
  :mode ("\\.ya?ml$" . yaml-mode))
