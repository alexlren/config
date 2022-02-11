;; Python
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(eval-after-load "elpy"
 '(progn
    (define-key elpy-mode-map (kbd "<M-down>") nil)
    (define-key elpy-mode-map (kbd "<M-up>") nil)
    (define-key elpy-mode-map (kbd "<M-left>") nil)
    (define-key elpy-mode-map (kbd "<M-right>") nil)))

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
