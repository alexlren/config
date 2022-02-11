;; #############################################################################
;; # BASE
;; #############################################################################

;; Hints
(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers)
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-disabled-checkers)
  :config
  (global-flycheck-mode)
  (append flycheck-disabled-checkers
          '(javascript-jshint)
          '(json-jsonlist)
          '(emacs-lisp-checkdoc))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; Line numbers
(use-package nlinum)

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-c m" . mc/edit-lines)
         ("C-c a" . mc/mark-all-like-this)))

;; Auto completion
(use-package company
  :config
  (global-company-mode)
  :bind (("TAB" . company-indent-or-complete-common)))

;; Disable syntax highlighting for files > 10MB
(defun check-large-file-hook ()
  "If a file is over a given size, turn off syntax highlighting"
  (when (> (buffer-size) (* 10240 1024))
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hooks 'check-large-file-hook)
