;; --------------------
;; RUST
;; --------------------

(add-hook 'rust-mode-hook #'racer-mode)
;;(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
;;(setq rust-format-on-save t)

(define-key rust-mode-map (kbd "<f5>") 'rust-run)
