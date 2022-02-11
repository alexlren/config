;;-----------------------------------
;; CPP
;;-----------------------------------

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
