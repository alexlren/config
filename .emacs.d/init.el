;; #############################################################################
;; # EMACS CONFIGURATION
;; #############################################################################

;; Enable byte compilation before loading anything
(custom-set-variables
 '(auto-compile-on-load-mode t)
 '(auto-compile-on-save-mode t)
 )

;; Setup package manager to also use melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
    (package-initialize))

;; Add use-package
(eval-when-compile
  (require 'use-package))

;; Install packages automatically
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Load custom variables
(setq custom-file "~/.emacs.d/core/customize.el")
(load custom-file)

;; Load extra utils functions
(load "~/.emacs.d/core/utils.el")

;; For additional custom addons
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Set 'y or n' instead of 'yes or no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Gdb
(setq-default gdb-many-windows t)
(setq compilation-window-height 12)
(setq compilation-scroll-output t)

;; Default indentation
(space-indent-mode t)

;; Enable upcase/downcase (disabled by default)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Show whitespace (like tabs) in programming mode
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Disable ring bell because it's fucking annoying
(setq ring-bell-function 'ignore)

;; Default formatting style
(setq c-default-style "linux")

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'sha1 t)

;; Global packages
(load "~/.emacs.d/core/base.el")

;; Keys
(load "~/.emacs.d/core/keys.el")

;; Languages config & modes
(load "~/.emacs.d/core/modes.el")
