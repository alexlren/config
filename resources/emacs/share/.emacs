(require 'bytecomp)
(setq dot-emacs "~/.emacs.config.el")
(setq el-file-list (file-expand-wildcards "~/.emacs.d/*.el"))

(defun byte-compile-el-file (afile)
  (setq elfile afile)
  (setq compiled-elfile (byte-compile-dest-file elfile))
  (and (file-newer-than-file-p elfile compiled-elfile)
                            (byte-compile-file elfile)))

(defun load-byte-compile-el-file (afile)
  (setq elfile afile)
  (setq compiled-elfile (byte-compile-dest-file elfile))
  (if (or (not (file-exists-p compiled-elfile))
	(file-newer-than-file-p elfile compiled-elfile)
        (equal (nth 4 (file-attributes elfile)) (list 0 0)))
      (load elfile)
    (load compiled-elfile))
  (add-hook 'kill-emacs-hook
            '(lambda () (and (file-newer-than-file-p elfile compiled-elfile)
                             (byte-compile-file elfile)))))

(while el-file-list
  (setq elfile (car el-file-list))
  (byte-compile-el-file elfile)
  (setq el-file-list (cdr el-file-list)))

(load-byte-compile-el-file dot-emacs)
