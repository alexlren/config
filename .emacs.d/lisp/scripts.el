;; --------------------
;; SCRIPTS
;; --------------------

;; Shebangs

(defun insert-shebang (path)
  (save-excursion
    (goto-char (point-min))
    (insert "#! " path  "\n\n")
    (save-buffer)
    (call-process "chmod" nil t nil "+x" (buffer-file-name))))

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
