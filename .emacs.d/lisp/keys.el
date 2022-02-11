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

;; Move to windows
(windmove-default-keybindings 'meta)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; (global-set-key [M-up] 'windmove-up)
;; (global-set-key [M-down] 'windmove-down)
;; (global-set-key [M-right] 'windmove-right)
;; (global-set-key [M-left] 'windmove-left)

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
;; Imenu
(global-set-key (kbd "M-,") 'idomenu)
;; Comments
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

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

;; Kill region or backspace
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
