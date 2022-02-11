;; --------------------
;; UTILS
;; --------------------

;; Auto shebang
(defun insert-shebang (path)
  (save-excursion
    (goto-char (point-min))
    (insert "#! " path  "\n\n")
    (save-buffer)
    (call-process "chmod" nil t nil "+x" (buffer-file-name))))

;; Indentation
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
