;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq user-full-name "Alex Laurent"
      user-mail-address "laurent@ionq.co")

(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Remove exit confirmation
(setq confirm-kill-emacs nil)

;; enable word wrapping
(setq word-wrap t)
(global-visual-line-mode t)

;; Disable smartparents
;;(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; ---------------------
;; Global Keys
;; ---------------------

;; Windows
(if (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings 'meta)
  (progn
    (global-set-key (kbd "M-up") 'windmove-up)
    (global-set-key (kbd "M-down") 'windmove-down)
    (global-set-key (kbd "M-right") 'windmove-right)
    (global-set-key (kbd "M-left") 'windmove-left)
    ))

(global-set-key (kbd "<f1>") 'delete-other-windows)
(global-set-key (kbd "<f2>") 'split-window-horizontally)
(global-set-key (kbd "<f3>") 'split-window-vertically)
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x \-") 'split-window-vertically)

(map! "C-c C-r" #'doom/reload)
(map! "C-c /" #'comment-or-uncomment-region)

(map! "C-s" #'isearch-forward-regexp)
(map! "C-c C-s" #'+vertico/search-symbol-at-point)
(map! :map isearch-mode-map
      "C-n" #'isearch-repeat-forward
      "C-p" #'isearch-repeat-backward)

(map! "C-r" #'replace-regexp)
(map! "C-c r" #'replace-string)

;; ---------------------
;; Config packages
;; ---------------------

(use-package! company
  :config
  (setq company-idle-delay 0
        company-tooltip-limit 10
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
)

(map!
:after company
:map company-active-map
:desc "<tab>" :nv "<tab>" #'company-complete-selection)

(map!
 :after company
 :map lsp-mode-map
 "<tab>" #'company-indent-or-complete-common)

;; (after! company
;;     ;;; Prevent suggestions from being triggered automatically. In particular,
;;   ;;; this makes it so that:
;;   ;;; - TAB will always complete the current selection.
;;   ;;; - RET will only complete the current selection if the user has explicitly
;;   ;;;   interacted with Company.
;;   ;;; - SPC will never complete the current selection.
;;   ;;;
;;   ;;; Based on:
;;   ;;; - https://github.com/company-mode/company-mode/issues/530#issuecomment-226566961
;;   ;;; - https://emacs.stackexchange.com/a/13290/12534
;;   ;;; - http://stackoverflow.com/a/22863701/3538165
;;   ;;;
;;   ;;; See also:
;;   ;;; - https://emacs.stackexchange.com/a/24800/12534
;;   ;;; - https://emacs.stackexchange.com/q/27459/12534

;;   ;; <return> is for windowed Emacs; RET is for terminal Emacs
;;   (dolist (key '("<return>" "RET"))
;;     ;; Here we are using an advanced feature of define-key that lets
;;     ;; us pass an "extended menu item" instead of an interactive
;;     ;; function. Doing this allows RET to regain its usual
;;     ;; functionality when the user has not explicitly interacted with
;;     ;; Company.
;;     (define-key company-active-map (kbd key)
;;       `(menu-item nil company-complete
;;                   :filter ,(lambda (cmd)
;;                              (when (company-explicit-action-p)
;;                               cmd)))))
;;   ;; (define-key company-active-map (kbd "TAB") #'company-complete-selection)
;;   (map! :map company-active-map "TAB" #'company-complete-selection)
;;   (map! :map company-active-map "<tab>" #'company-complete-selection)
;;   (define-key company-active-map (kbd "SPC") nil)

;;   ;; Company appears to override the above keymap based on company-auto-complete-chars.
;;   ;; Turning it off ensures we have full control.
;;   (setq company-auto-commit-chars nil)
;;   )

(use-package! lsp-mode
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0)
  ;; enable / disable the hints as you prefer:
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;; (setq lsp-enable-xref nil)
  ;; (setq lsp-enable-imenu nil)
  (map! :map lsp-mode-map
        "<f10>" #'lsp-find-definition
        "C-c C-c l" #'flycheck-list-errors
        "C-c C-c a" #'lsp-execute-code-action
        "C-c C-c r" #'lsp-rename
        "C-c C-c q" #'lsp-workspace-restart
        "C-c C-c Q" #'lsp-workspace-shutdown
        "<tab>" #'company-indent-or-complete-common)
  :hook (
         (web-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         )
  )

(use-package! lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t)
  :config
  (map! :map lsp-ui-mode-map
        "M-," #'lsp-ui-imenu
        "<f11>" #'lsp-ui-peek-find-references
        )
)

(use-package! multiple-cursors
  :config
  (map! :map multiple-cursors-mode-map
        "C-x m" #'mc/edit-lines
        "C-x a" #'mc/mark-all-like-this)
)

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(put 'dockerfile-image-name 'safe-local-variable #'stringp)

(use-package! rustic
  :config
  (setq rustic-format-on-save t)
)
;;(+global-word-wrap-mode +1)
