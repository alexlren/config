;;; sha1-theme.el --- A fruity color theme for Emacs.

;; Copyright (C) 2011-2016

;; Author: Kelvin Smith <oneKelvinSmith@gmail.com>
;; URL: http://github.com/oneKelvinSmith/monokai-emacs
;; Version: 3.5.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of the popular Textmate theme Sha1 for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.
;;
;; Color modified from monokai theme
;;
;;; Credits:
;;
;; Wimer Hazenberg created the original theme.
;; - http://www.sha1.nl/blog/2006/07/15/textmate-color-theme/
;;
;; Bozhidar Batsov created zenburn-theme.el and solarized-theme.el
;;  on which this file is based.
;; - https://github.com/bbatsov/zenburn-emacs
;;
;; Color Scheme Designer 3 for complementary colours.
;; - http://colorschemedesigner.com/
;;
;; Xterm 256 Color Chart
;; - https://upload.wikimedia.org/wikipedia/en/1/15/Xterm_256color_chart.svg
;;
;; K. Adam Christensen for his personal sha1 theme that addresses 256 colours.
;; - https://github.com/pope/personal/blob/master/etc/emacs.d/sha1-theme.el
;;
;; Thomas Frössman for his work on solarized-emacs.
;; - http://github.com/bbatsov/solarized-emacs
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The sha1 theme requires Emacs 24 or later!"))

(deftheme sha1 "The Sha1 colour theme")

(defgroup sha1 nil
  "Sha1 theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom sha1-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'sha1)

(defcustom sha1-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'sha1)

(defcustom sha1-doc-face-as-comment nil
  "Consider `font-lock-doc-face' as comment instead of a string."
  :type 'boolean
  :group 'sha1
  :package-version "3.5.1")

(defcustom sha1-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'sha1)

(defcustom sha1-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'sha1)

(defcustom sha1-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'sha1)

(defcustom sha1-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'sha1)

(defcustom sha1-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'sha1)

;; Primary colors
(defcustom sha1-yellow "#E6DB74"
  "Primary colors - yellow"
  :type 'string
  :group 'sha1)

(defcustom sha1-orange "#FD971F"
  "Primary colors - orange"
  :type 'string
  :group 'sha1)

(defcustom sha1-red "#F92672"
  "Primary colors - red"
  :type 'string
  :group 'sha1)

(defcustom sha1-magenta "#FD5FF0"
  "Primary colors - magenta"
  :type 'string
  :group 'sha1)

(defcustom sha1-blue "#66D9EF"
  "Primary colors - blue"
  :type 'string
  :group 'sha1)

(defcustom sha1-green "#A6E22E"
  "Primary colors - green"
  :type 'string
  :group 'sha1)

(defcustom sha1-cyan "#A1EFE4"
  "Primary colors - cyan"
  :type 'string
  :group 'sha1)

(defcustom sha1-violet "#AE81FF"
  "Primary colors - violet"
  :type 'string
  :group 'sha1)

(defcustom sha1-gray "#64645E"
  "Primary colors - gray"
  :type 'string
  :group 'sha1)

(defcustom sha1-dark-gray "#3D3D3D"
  "Primary colors - dark gray"
  :type 'string
  :group 'sha1)

(defcustom sha1-foreground "#FFFFFF"
  "Adaptive colors - foreground"
  :type 'string
  :group 'sha1)

(defcustom sha1-background "#0a1621"
  "Adaptive colors - background"
  :type 'string
  :group 'sha1)

(defcustom sha1-comments "#75715E"
  "Adaptive colors - comments"
  :type 'string
  :group 'sha1)

(defcustom sha1-emphasis "#F8F8F0"
  "Adaptive colors - emphasis"
  :type 'string
  :group 'sha1)

(defcustom sha1-line-number "#8F908A"
  "Adaptive colors - line number"
  :type 'string
  :group 'sha1)

(defcustom sha1-highlight "#49483E"
  "Adaptive colors - highlight"
  :type 'string
  :group 'sha1)

(defcustom sha1-highlight-alt "#3E3D31"
  "Adaptive colors - highlight"
  :type 'string
  :group 'sha1)

(defcustom sha1-highlight-line "#3C3D37"
  "Adaptive colors - line highlight"
  :type 'string
  :group 'sha1)

(defcustom sha1-mode-line "#104e8b"
  "Adaptive colors - Mode line"
  :type 'string
  :group 'sha1)

(defcustom sha1-mode-line-alt "#0e467d"
  "Adaptive colors - Mode line inactive"
  :type 'string
  :group 'sha1)


(let* (;; Variable pitch
       (sha1-pitch (if sha1-use-variable-pitch
                          'variable-pitch
                        'default))

       ;; Definitions for guis that support 256 colors
       (sha1-class '((class color) (min-colors 257)))

       ;; Functionality specific colors
       (sha1-diff-blue-base      "#232438")
       (sha1-diff-blue-emphasis  "#1F204E")
       (sha1-diff-green-base     "#233E1E")
       (sha1-diff-green-emphasis "#1F541A")
       (sha1-diff-red-base       "#3D241E")
       (sha1-diff-red-emphasis   "#53201A")

       ;; Darker and lighter accented colors
       (sha1-yellow-d       "#BEB244")
       (sha1-yellow-l       "#FFF7A8")
       (sha1-orange-d       "#D47402")
       (sha1-orange-l       "#FFAC4A")
       (sha1-red-d          "#F70057")
       (sha1-red-l          "#FA518D")
       (sha1-magenta-d      "#FB35EA")
       (sha1-magenta-l      "#FE8CF4")
       (sha1-violet-d       "#945AFF")
       (sha1-violet-l       "#C9ACFF")
       (sha1-blue-d         "#40CAE4")
       (sha1-blue-l         "#92E7F7")
       (sha1-cyan-d         "#74DBCD")
       (sha1-cyan-l         "#D3FBF6")
       (sha1-green-d        "#86C30D")
       (sha1-green-l        "#BBEF53")
       (sha1-gray-d         "#35331D")
       (sha1-gray-l         "#7B7962")
       ;; Adaptive higher/lower contrast accented colors
       (sha1-foreground-hc  "#141414")
       (sha1-foreground-lc  "#171A0B")
       ;; High contrast colors
       (sha1-yellow-hc      "#FFFACE")
       (sha1-yellow-hc-alt  "#E7DB74")
       (sha1-yellow-lc      "#9A8F21")
       (sha1-orange-hc      "#FFBE74")
       (sha1-orange-lc      "#A75B00")
       (sha1-red-hc         "#FEB0CC")
       (sha1-red-hc-alt     "#F83535")
       (sha1-red-lc         "#F20055")
       (sha1-magenta-hc     "#FEC6F9")
       (sha1-magenta-lc     "#F309DF")
       (sha1-violet-hc      "#F0E7FF")
       (sha1-violet-lc      "#7830FC")
       (sha1-blue-hc        "#CAF5FD")
       (sha1-blue-lc        "#1DB4D0")
       (sha1-cyan-hc        "#D3FBF6")
       (sha1-cyan-lc        "#4BBEAE")
       (sha1-green-hc       "#CCF47C")
       (sha1-green-hc-alt   "#A6E22C")
       (sha1-green-lc       "#679A01")

       ;; Distinct fringe
       (sha1-fringe-bg (if sha1-distinct-fringe-background
                              sha1-gray
                            sha1-background))

       ;; Definitions for terminals that do not support 256 colors
       (sha1-256-class '((class color) (min-colors 89)))

       ;; Functionality specific colors
       (sha1-256-diff-blue-base      "#00005f")
       (sha1-256-diff-blue-emphasis  "#000087")
       (sha1-256-diff-green-base     "#005800")
       (sha1-256-diff-green-emphasis "#008700")
       (sha1-256-diff-red-base       "#5f0000")
       (sha1-256-diff-red-emphasis   "#870000")

       ;; Primary colors
       (sha1-256-yellow         "#CDC673")
       (sha1-256-orange         "#FF8C00")
       (sha1-256-red            "#F92672")
       (sha1-256-magenta        "#D700D7")
       (sha1-256-violet         "#AF87FF")
       (sha1-256-blue           "#5FD7FF")
       (sha1-256-cyan           "#5FFFFF")
       (sha1-256-green          "#87D700")
       (sha1-256-gray           "#636363")
       (sha1-256-dark-gray      "#3D3D3D")

       ;; Darker and lighter accented colors
       (sha1-256-yellow-d       "#878700")
       (sha1-256-yellow-l       "#FFFF87")
       (sha1-256-orange-d       "#AF5F00")
       (sha1-256-orange-l       "#FFAF5F")
       (sha1-256-red-d          "#870000")
       (sha1-256-red-l          "#FF5F87")
       (sha1-256-magenta-d      "#AF0087")
       (sha1-256-magenta-l      "#FF87DF")
       (sha1-256-violet-d       "#5F00AF")
       (sha1-256-violet-l       "#AF87D7")
       (sha1-256-blue-d         "#008787")
       (sha1-256-blue-l         "#87D7FF")
       (sha1-256-cyan-d         "#5FAFAF")
       (sha1-256-cyan-l         "#AFFFFF")
       (sha1-256-green-d        "#5F8700")
       (sha1-256-green-l        "#AFD700")
       (sha1-256-gray-d         "#333333")
       (sha1-256-gray-l         "#707070")
       ;; Adaptive colors
       (sha1-256-foreground     "#FFFFFF")
       (sha1-256-background     nil)
       (sha1-256-comments       "#8B8878")
       (sha1-256-emphasis       "#FFFAFA")
       (sha1-256-line-number    "#8F908A")
       (sha1-256-highlight      "#474747")
       (sha1-256-highlight-alt  "#607b8b")
       (sha1-256-highlight-line "#000000")
       (sha1-256-mode-line      "#104e8b")
       (sha1-256-mode-line-alt  "#0e467d")
       (sha1-256-vertical-line  "#ebebeb")
       ;; Adaptive higher/lower contrast accented colors
       (sha1-256-foreground-hc  "#171A0B")
       (sha1-256-foreground-lc  "#141414")
       ;; High contrast colors
       (sha1-256-yellow-hc      sha1-256-yellow-d)
       (sha1-256-yellow-lc      sha1-256-yellow-l)
       (sha1-256-orange-hc      sha1-256-orange-d)
       (sha1-256-orange-lc      sha1-256-orange-l)
       (sha1-256-red-hc         sha1-256-red-d)
       (sha1-256-red-lc         sha1-256-red-l)
       (sha1-256-magenta-hc     sha1-256-magenta-d)
       (sha1-256-magenta-lc     sha1-256-magenta-l)
       (sha1-256-violet-hc      sha1-256-violet-d)
       (sha1-256-violet-lc      sha1-256-violet-l)
       (sha1-256-blue-hc        sha1-256-blue-d)
       (sha1-256-blue-lc        sha1-256-blue-l)
       (sha1-256-cyan-hc        sha1-256-cyan-d)
       (sha1-256-cyan-lc        sha1-256-cyan-l)
       (sha1-256-green-hc       sha1-256-green-d)
       (sha1-256-green-lc       sha1-256-green-l)

       ;; Distinct fringe
       (sha1-256-fringe-bg (if sha1-distinct-fringe-background
                                  sha1-256-gray
                                sha1-256-background)))

  ;; Define faces
  (custom-theme-set-faces
   'sha1

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(font-lock-comment-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(font-lock-constant-face
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(font-lock-doc-face
     ((,sha1-class (:foreground ,(if sha1-doc-face-as-comment
                                        sha1-comments
                                      sha1-green)))
      (,sha1-256-class (:foreground ,(if sha1-doc-face-as-comment
                                            sha1-256-comments
                                          sha1-256-green)))))

   `(font-lock-function-name-face
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(font-lock-keyword-face
     ((,sha1-class (:foreground ,sha1-red
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :weight normal))))

   `(font-lock-negation-char-face
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight bold))))

   `(font-lock-preprocessor-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(font-lock-regexp-grouping-construct
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,sha1-class (:foreground ,sha1-violet
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-violet
                                        :weight normal))))

   `(font-lock-string-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(font-lock-type-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :italic nil))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :italic nil))))

   `(font-lock-variable-name-face
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(font-lock-warning-face
     ((,sha1-class (:foreground ,sha1-orange
                                   :weight bold
                                   :italic t
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-orange
                                        :weight bold
                                        :italic t
                                        :underline t))))

   `(c-annotation-face
     ((,sha1-class (:inherit font-lock-constant-face))
      (,sha1-256-class (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
      ((,sha1-class (:foreground ,sha1-foreground
                                    :background ,sha1-background))
       (,sha1-256-class (:foreground ,sha1-256-foreground
                                         :background ,sha1-256-background))))

   `(highlight
     ((,sha1-class (:background ,sha1-highlight))
      (,sha1-256-class (:background ,sha1-256-highlight))))

   `(lazy-highlight
     ((,sha1-class (:inherit highlight
                                :background ,sha1-highlight-alt))
      (,sha1-256-class (:inherit highlight
                                     :background ,sha1-256-highlight-alt))))

   `(region
     ((,sha1-class (:inherit highlight
                                :background ,sha1-highlight))
      (,sha1-256-class (:inherit highlight
                                     :background ,sha1-256-highlight))))

   `(secondary-selection
     ((,sha1-class (:inherit region
                                :background ,sha1-highlight-alt))
      (,sha1-256-class (:inherit region
                                     :background ,sha1-256-highlight-alt))))

   `(shadow
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(match
     ((,sha1-class (:background ,sha1-blue
                                   :foreground ,sha1-background
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-blue
                                        :foreground ,sha1-256-background
                                        :weight bold))))

   `(cursor
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-foreground
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-red
                                        :inverse-video t))))

   `(mouse
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-foreground
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-foreground
                                        :inverse-video t))))

   `(escape-glyph
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(escape-glyph-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(fringe
     ((,sha1-class (:foreground ,sha1-foreground
                                   :background ,sha1-fringe-bg))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :background ,sha1-256-fringe-bg))))

   `(link
     ((,sha1-class (:foreground ,sha1-blue
                                   :underline t
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :underline t
                                        :weight bold))))

   `(link-visited
     ((,sha1-class (:foreground ,sha1-violet
                                   :underline t
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-violet
                                        :underline t
                                        :weight normal))))

   `(success
     ((,sha1-class (:foreground ,sha1-green ))
      (,sha1-256-class (:foreground ,sha1-256-green ))))

   `(warning
     ((,sha1-class (:foreground ,sha1-yellow ))
      (,sha1-256-class (:foreground ,sha1-256-yellow ))))

   `(error
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(eval-sexp-fu-flash
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-green))))

   `(eval-sexp-fu-flash-error
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-red))))

   `(trailing-whitespace
     ((,sha1-class (:background ,sha1-red))
      (,sha1-256-class (:background ,sha1-256-red))))

   `(vertical-border
     ((,sha1-class (:foreground ,sha1-gray))
      (,sha1-256-class (:foreground ,sha1-256-gray))))

   `(menu
     ((,sha1-class (:foreground ,sha1-foreground
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :background ,sha1-256-background))))

   `(minibuffer-prompt
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id
     ((,sha1-class (:weight bold))
      (,sha1-256-class (:weight bold))))

   `(mode-line
     ((,sha1-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,sha1-emphasis
                                      :background ,sha1-highlight
                                      :background ,sha1-mode-line
                                      :box (:line-width 1
                                                        :color ,sha1-gray
                                                        :style unspecified)))
      (,sha1-256-class (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,sha1-256-foreground
                                           :background ,sha1-256-mode-line
                                           :box (:line-width 1
                                                             :color ,sha1-256-highlight
                                                             :style unspecified)))))

   `(powerline-active1
     ((,sha1-class (:background ,sha1-gray-d))
      (,sha1-256-class (:background ,sha1-256-gray-d))))

   `(powerline-active2
     ((,sha1-class (:background ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-background))))


   `(mode-line-inactive
     ((,sha1-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,sha1-comments
                                      :background ,sha1-mode-line-alt
                                      :box (:line-width 1
                                                        :color ,sha1-foreground
                                                        :style unspecified)))
      (,sha1-256-class (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,sha1-256-comments
                                           :background ,sha1-256-mode-line-alt
                                           :box (:line-width 1
                                                             :color ,sha1-256-foreground
                                                             :style unspecified)))))

   `(powerline-inactive1
     ((,sha1-class (:background ,sha1-gray-d))
      (,sha1-256-class (:background ,sha1-256-gray-d))))

   `(powerline-inactive2
     ((,sha1-class (:background ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-background))))

   ;; header-line
   `(header-line
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :background ,sha1-highlight
                                   :box (:color ,sha1-dark-gray
                                                :line-width 1
                                                :style unspecified)))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :background ,sha1-256-highlight
                                        :box (:color ,sha1-256-dark-gray
                                                     :line-width 1
                                                     :style unspecified)))))

   ;; cua
   `(cua-global-mark
     ((,sha1-class (:background ,sha1-yellow
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-yellow
                                        :foreground ,sha1-256-background))))

   `(cua-rectangle
     ((,sha1-class (:inherit region))
      (,sha1-256-class (:inherit region))))

   `(cua-rectangle-noselect
     ((,sha1-class (:inherit secondary-selection))
      (,sha1-256-class (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   ;; dired
   `(dired-directory
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(dired-flagged
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(dired-header
     ((,sha1-class (:foreground ,sha1-blue
                                   :background ,sha1-background
                                   :inherit bold))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :background ,sha1-256-background
                                        :inherit bold))))

   `(dired-ignored
     ((,sha1-class (:inherit shadow))
      (,sha1-256-class (:inherit shadow))))

   `(dired-mark
     ((,sha1-class (:foreground ,sha1-green
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :weight bold))))

   `(dired-marked
     ((,sha1-class (:foreground ,sha1-violet
                                   :inherit bold))
      (,sha1-256-class (:foreground ,sha1-256-violet
                                        :inherit bold))))

   `(dired-perm-write
     ((,sha1-class (:foreground ,sha1-foreground
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :underline t))))

   `(dired-symlink
     ((,sha1-class (:foreground ,sha1-cyan
                                   :slant italic))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :slant italic))))

   `(dired-warning
     ((,sha1-class (:foreground ,sha1-orange
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-orange
                                        :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-blue))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-blue))))

   `(dropdown-list-selection-face
     ((,sha1-class (:background ,sha1-green
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-green
                                        :foreground ,sha1-256-background))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,sha1-class (:background ,sha1-blue
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-blue
                                        :foreground ,sha1-256-background))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,sha1-class (:inherit ecb-history-bucket-node-face
                                :foreground ,sha1-yellow))
      (,sha1-256-class (:inherit ecb-history-bucket-node-face
                                     :foreground ,sha1-256-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,sha1-class (:inherit ecb-directories-general-face
                                :foreground ,sha1-foreground))
      (,sha1-256-class (:inherit ecb-directories-general-face
                                     :foreground ,sha1-256-foreground))))

   `(ecb-history-dead-buffer-face
     ((,sha1-class (:inherit ecb-history-general-face
                                :foreground ,sha1-comments))
      (,sha1-256-class (:inherit ecb-history-general-face
                                     :foreground ,sha1-256-comments))))

   `(ecb-directory-not-accessible-face
     ((,sha1-class (:inherit ecb-directories-general-face
                                :foreground ,sha1-comments))
      (,sha1-256-class (:inherit ecb-directories-general-face
                                     :foreground ,sha1-256-comments))))

   `(ecb-bucket-node-face
     ((,sha1-class (:inherit ecb-default-general-face
                                :weight normal
                                :foreground ,sha1-blue))
      (,sha1-256-class (:inherit ecb-default-general-face
                                     :weight normal
                                     :foreground ,sha1-256-blue))))

   `(ecb-tag-header-face
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,sha1-class (:inherit ecb-analyse-general-face
                                :foreground ,sha1-green))
      (,sha1-256-class (:inherit ecb-analyse-general-face
                                     :foreground ,sha1-256-green))))

   `(ecb-directories-general-face
     ((,sha1-class (:inherit ecb-default-general-face
                                :height 1.0))
      (,sha1-256-class (:inherit ecb-default-general-face
                                     :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,sha1-class (:inherit ecb-methods-general-face
                                :foreground ,sha1-cyan))
      (,sha1-256-class (:inherit ecb-methods-general-face
                                     :foreground ,sha1-256-cyan))))

   `(ecb-mode-line-prefix-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(ecb-tree-guide-line-face
     ((,sha1-class (:inherit ecb-default-general-face
                                :foreground ,sha1-gray
                                :height 1.0))
      (,sha1-256-class (:inherit ecb-default-general-face
                                     :foreground ,sha1-256-gray
                                     :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,sha1-class (:foreground ,sha1-emphasis))
      (,sha1-256-class (:foreground ,sha1-256-emphasis))))

   `(ee-category
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(ee-link
     ((,sha1-class (:inherit link))
      (,sha1-256-class (:inherit link))))

   `(ee-link-visited
     ((,sha1-class (:inherit link-visited))
      (,sha1-256-class (:inherit link-visited))))

   `(ee-marked
     ((,sha1-class (:foreground ,sha1-magenta
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-magenta
                                        :weight bold))))

   `(ee-omitted
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(ee-shadow
     ((,sha1-class (:inherit shadow))
      (,sha1-256-class (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(grep-error-face
     ((,sha1-class (:foreground ,sha1-red
                                   :weight bold
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :weight bold
                                        :underline t))))

   `(grep-hit-face
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(grep-match-face
     ((,sha1-class (:foreground ,sha1-green
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :weight bold))))

   ;; isearch
   `(isearch
     ((,sha1-class (:inherit region
                                :foreground ,sha1-background
                                :background ,sha1-blue))
      (,sha1-256-class (:inherit region
                                     :foreground ,sha1-256-background
                                     :background ,sha1-256-blue))))

   `(isearch-fail
     ((,sha1-class (:inherit isearch
                                :foreground ,sha1-red
                                :background ,sha1-background
                                :bold t))
      (,sha1-256-class (:inherit isearch
                                     :foreground ,sha1-256-red
                                     :background ,sha1-256-background
                                     :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,sha1-class (:foreground ,sha1-comments
                                   :background ,sha1-background
                                   :inverse-video nil))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :background ,sha1-256-background
                                        :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,sha1-class (:foreground ,sha1-yellow
                                   :background ,sha1-background
                                   :inverse-video nil
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :background ,sha1-256-background
                                        :inverse-video nil
                                        :weight bold))))

   ;; auctex
   `(font-latex-bold-face
     ((,sha1-class (:inherit bold
                                :foreground ,sha1-emphasis))
      (,sha1-256-class (:inherit bold
                                     :foreground ,sha1-256-emphasis))))

   `(font-latex-doctex-documentation-face
     ((,sha1-class (:background unspecified))
      (,sha1-256-class (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,sha1-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,sha1-256-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,sha1-class (:inherit italic :foreground ,sha1-emphasis))
      (,sha1-256-class (:inherit italic :foreground ,sha1-256-emphasis))))

   `(font-latex-math-face
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(font-latex-sectioning-0-face
     ((,sha1-class (:inherit font-latex-sectioning-1-face
                                :height ,sha1-height-plus-1))
      (,sha1-256-class (:inherit font-latex-sectioning-1-face
                                     :height ,sha1-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,sha1-class (:inherit font-latex-sectioning-2-face
                                :height ,sha1-height-plus-1))
      (,sha1-256-class (:inherit font-latex-sectioning-2-face
                                     :height ,sha1-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,sha1-class (:inherit font-latex-sectioning-3-face
                                :height ,sha1-height-plus-1))
      (,sha1-256-class (:inherit font-latex-sectioning-3-face
                                     :height ,sha1-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,sha1-class (:inherit font-latex-sectioning-4-face
                                :height ,sha1-height-plus-1))
      (,sha1-256-class (:inherit font-latex-sectioning-4-face
                                     :height ,sha1-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,sha1-class (:inherit font-latex-sectioning-5-face
                                :height ,sha1-height-plus-1))
      (,sha1-256-class (:inherit font-latex-sectioning-5-face
                                     :height ,sha1-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-yellow
                                :weight bold))
      (,sha1-256-class (:inherit ,sha1-pitch :
                                     foreground ,sha1-256-yellow
                                     :weight bold))))

   `(font-latex-sedate-face
     ((,sha1-class (:foreground ,sha1-emphasis))
      (,sha1-256-class (:foreground ,sha1-256-emphasis))))

   `(font-latex-slide-title-face
     ((,sha1-class (:inherit (,sha1-pitch font-lock-type-face)
                                :weight bold
                                :height ,sha1-height-plus-3))
      (,sha1-256-class (:inherit (,sha1-pitch font-lock-type-face)
                                     :weight bold
                                     :height ,sha1-height-plus-3))))

   `(font-latex-string-face
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(font-latex-subscript-face
     ((,sha1-class (:height ,sha1-height-minus-1))
      (,sha1-256-class (:height ,sha1-height-minus-1))))

   `(font-latex-superscript-face
     ((,sha1-class (:height ,sha1-height-minus-1))
      (,sha1-256-class (:height ,sha1-height-minus-1))))

   `(font-latex-verbatim-face
     ((,sha1-class (:inherit fixed-pitch
                                :foreground ,sha1-foreground
                                :slant italic))
      (,sha1-256-class (:inherit fixed-pitch
                                     :foreground ,sha1-256-foreground
                                     :slant italic))))

   `(font-latex-warning-face
     ((,sha1-class (:inherit bold
                                :foreground ,sha1-orange))
      (,sha1-256-class (:inherit bold
                                     :foreground ,sha1-256-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-blue))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-blue))))

   `(ac-selection-face
     ((,sha1-class (:background ,sha1-blue
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-blue
                                        :foreground ,sha1-256-background))))

   `(ac-candidate-mouse-face
     ((,sha1-class (:background ,sha1-blue
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-blue
                                        :foreground ,sha1-256-background))))

   `(ac-completion-face
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :underline t))))

   `(ac-gtags-candidate-face
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-blue))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-blue))))

   `(ac-gtags-selection-face
     ((,sha1-class (:background ,sha1-blue
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-blue
                                        :foreground ,sha1-256-background))))

   `(ac-yasnippet-candidate-face
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-yellow))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-yellow))))

   `(ac-yasnippet-selection-face
     ((,sha1-class (:background ,sha1-yellow
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-yellow
                                        :foreground ,sha1-256-background))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-blue))))

   `(ahs-edit-mode-face
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-highlight))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-highlight))))

   `(ahs-face
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-magenta
                                        :background unspecified))))

   `(ahs-plugin-bod-face
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-violet ))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-cyan ))))

   `(ahs-plugin-defalt-face
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-green))))

   `(ahs-warning-face
     ((,sha1-class (:foreground ,sha1-red
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(android-mode-error-face
     ((,sha1-class (:foreground ,sha1-orange
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-orange
                                        :weight bold))))

   `(android-mode-info-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(android-mode-verbose-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(android-mode-warning-face
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,sha1-class (:foreground ,sha1-violet
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-violet
                                        :weight bold))))

   ;; bm
   `(bm-face
     ((,sha1-class (:background ,sha1-yellow-lc
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-yellow-lc
                                        :foreground ,sha1-256-background))))

   `(bm-fringe-face
     ((,sha1-class (:background ,sha1-yellow-lc
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-yellow-lc
                                        :foreground ,sha1-256-background))))

   `(bm-fringe-persistent-face
     ((,sha1-class (:background ,sha1-green-lc
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-green-lc
                                        :foreground ,sha1-256-background))))

   `(bm-persistent-face
     ((,sha1-class (:background ,sha1-green-lc
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-green-lc
                                        :foreground ,sha1-256-background))))

   ;; calfw
   `(cfw:face-day-title
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   `(cfw:face-annotation
     ((,sha1-class (:inherit cfw:face-day-title
                                :foreground ,sha1-yellow))
      (,sha1-256-class (:inherit cfw:face-day-title
                                     :foreground ,sha1-256-yellow))))

   `(cfw:face-default-content
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(cfw:face-default-day
     ((,sha1-class (:inherit cfw:face-day-title
                                :weight bold))
      (,sha1-256-class (:inherit cfw:face-day-title
                                     :weight bold))))

   `(cfw:face-disable
     ((,sha1-class (:inherit cfw:face-day-title
                                :foreground ,sha1-comments))
      (,sha1-256-class (:inherit cfw:face-day-title
                                     :foreground ,sha1-256-comments))))

   `(cfw:face-grid
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(cfw:face-header
     ((,sha1-class (:foreground ,sha1-blue-hc
                                   :background ,sha1-blue-lc
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-blue-hc
                                        :background ,sha1-256-blue-lc
                                        :weight bold))))

   `(cfw:face-holiday
     ((,sha1-class (:background nil
                                   :foreground ,sha1-red
                                   :weight bold))
      (,sha1-256-class (:background nil
                                        :foreground ,sha1-256-red
                                        :weight bold))))

   `(cfw:face-periods
     ((,sha1-class (:foreground ,sha1-magenta))
      (,sha1-256-class (:foreground ,sha1-256-magenta))))

   `(cfw:face-select
     ((,sha1-class (:background ,sha1-magenta-lc
                                   :foreground ,sha1-magenta-hc))
      (,sha1-256-class (:background ,sha1-256-magenta-lc
                                        :foreground ,sha1-256-magenta-hc))))

   `(cfw:face-saturday
     ((,sha1-class (:foreground ,sha1-cyan-hc
                                   :background ,sha1-cyan-lc))
      (,sha1-256-class (:foreground ,sha1-256-cyan-hc
                                        :background ,sha1-256-cyan-lc))))

   `(cfw:face-sunday
     ((,sha1-class (:foreground ,sha1-red-hc
                                   :background ,sha1-red-lc
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-red-hc
                                        :background ,sha1-256-red-lc
                                        :weight bold))))

   `(cfw:face-title
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-yellow
                                :weight bold
                                :height ,sha1-height-plus-4))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :foreground ,sha1-256-yellow
                                     :weight bold
                                     :height ,sha1-height-plus-4))))

   `(cfw:face-today
     ((,sha1-class (:weight bold
                               :background ,sha1-highlight-line
                               :foreground nil))
      (,sha1-256-class (:weight bold
                                    :background ,sha1-256-highlight-line
                                    :foreground nil))))

   `(cfw:face-today-title
     ((,sha1-class (:background ,sha1-yellow-lc
                                   :foreground ,sha1-yellow-hc
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-yellow-lc
                                        :foreground ,sha1-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-foreground))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-foreground))))

   `(cfw:face-toolbar-button-off
     ((,sha1-class (:background ,sha1-yellow-lc
                                   :foreground ,sha1-yellow-hc
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-yellow-lc
                                        :foreground ,sha1-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,sha1-class (:background ,sha1-yellow-hc
                                   :foreground ,sha1-yellow-lc
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-yellow-hc
                                        :foreground ,sha1-256-yellow-lc
                                        :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,sha1-class (:foreground ,sha1-yellow
                                   :background nil
                                   :box (:color ,sha1-yellow :line-width -1 :style nil)))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :background nil
                                        :box (:color ,sha1-256-yellow :line-width -1 :style nil))) ))

   `(cider-enlightened-local
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(cider-instrumented-face
     ((,sha1-class (:foreground ,sha1-violet
                                   :background nil
                                   :box (:color ,sha1-violet :line-width -1 :style nil)))
      (,sha1-256-class (:foreground ,sha1-256-violet
                                        :background nil
                                        :box (:color ,sha1-256-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :background nil
                                   :box (:color ,sha1-blue :line-width -1 :style nil)))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :background nil
                                        :box (:color ,sha1-256-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-orange))))

   `(cider-test-failure-face
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-red))))

   `(cider-test-success-face
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-green))))

   `(cider-traced-face
     ((,sha1-class :box (:color ,sha1-blue :line-width -1 :style nil))
      (,sha1-256-class  :box (:color ,sha1-256-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,sha1-class (:foreground ,sha1-red
                                   :weight bold
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-error-face
     ((,sha1-class (:foreground ,sha1-orange
                                   :weight bold
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-success-face
     ((,sha1-class (:foreground ,sha1-green
                                   :weight bold
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :weight bold
                                        :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-emphasis))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-emphasis))))

   `(company-tooltip-selection
     ((,sha1-class (:background ,sha1-blue-hc
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-mode-line
                                        :foreground ,sha1-256-background))))

   `(company-tooltip-mouse
     ((,sha1-class (:background ,sha1-blue
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-blue
                                        :foreground ,sha1-256-background))))

   `(company-tooltip-common
     ((,sha1-class (:foreground ,sha1-blue
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :underline t))))

   `(company-tooltip-common-selection
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-blue-hc
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-blue
                                        :underline t))))

   `(company-preview
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-emphasis))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-emphasis))))

   `(company-preview-common
     ((,sha1-class (:foreground ,sha1-blue
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :underline t))))

   `(company-scrollbar-bg
     ((,sha1-class (:background ,sha1-gray))
      (,sha1-256-class (:background ,sha1-256-gray))))

   `(company-scrollbar-fg
     ((,sha1-class (:background ,sha1-comments))
      (,sha1-256-class (:background ,sha1-256-comments))))

   `(company-tooltip-annotation
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-green))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-green))))

   `(company-template-field
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-blue))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-blue))))

   ;; compilation
   `(compilation-column-face
     ((,sha1-class (:foreground ,sha1-cyan
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :underline nil))))

   `(compilation-column-number
     ((,sha1-class (:inherit font-lock-doc-face
                                :foreground ,sha1-cyan
                                :underline nil))
      (,sha1-256-class (:inherit font-lock-doc-face
                                     :foreground ,sha1-256-cyan
                                     :underline nil))))

   `(compilation-enter-directory-face
     ((,sha1-class (:foreground ,sha1-green
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :underline nil))))

   `(compilation-error
     ((,sha1-class (:inherit error
                                :underline nil))
      (,sha1-256-class (:inherit error
                                     :underline nil))))

   `(compilation-error-face
     ((,sha1-class (:foreground ,sha1-red
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :underline nil))))

   `(compilation-face
     ((,sha1-class (:foreground ,sha1-foreground
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :underline nil))))

   `(compilation-info
     ((,sha1-class (:foreground ,sha1-comments
                                   :underline nil
                                   :bold nil))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :underline nil
                                        :bold nil))))

   `(compilation-info-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :underline nil))))

   `(compilation-leave-directory-face
     ((,sha1-class (:foreground ,sha1-green
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :underline nil))))

   `(compilation-line-face
     ((,sha1-class (:foreground ,sha1-green
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :underline nil))))

   `(compilation-line-number
     ((,sha1-class (:foreground ,sha1-green
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :underline nil))))

   `(compilation-warning
     ((,sha1-class (:inherit warning
                                :underline nil))
      (,sha1-256-class (:inherit warning
                                     :underline nil))))

   `(compilation-warning-face
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight normal
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(compilation-mode-line-exit
     ((,sha1-class (:inherit compilation-info
                                :foreground ,sha1-green
                                :weight bold))
      (,sha1-256-class (:inherit compilation-info
                                     :foreground ,sha1-256-green
                                     :weight bold))))

   `(compilation-mode-line-fail
     ((,sha1-class (:inherit compilation-error
                                :foreground ,sha1-red
                                :weight bold))
      (,sha1-256-class (:inherit compilation-error
                                     :foreground ,sha1-256-red
                                     :weight bold))))

   `(compilation-mode-line-run
     ((,sha1-class (:foreground ,sha1-orange
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-orange
                                        :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,sha1-class (:foreground ,sha1-green
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :weight bold))))

   `(cscope-function-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(cscope-line-number-face
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(cscope-line-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(cscope-mouse-face
     ((,sha1-class (:background ,sha1-blue
                                   :foreground ,sha1-foreground))
      (,sha1-256-class (:background ,sha1-256-blue
                                        :foreground ,sha1-256-foreground))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-emphasis
                                   :underline ,sha1-emphasis
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-emphasis
                                        :underline ,sha1-256-emphasis
                                        :weight bold))))

   `(ctbl:face-continue-bar
     ((,sha1-class (:background ,sha1-gray
                                   :foreground ,sha1-yellow))
      (,sha1-256-class (:background ,sha1-256-gray
                                        :foreground ,sha1-256-yellow))))

   `(ctbl:face-row-select
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-foreground
                                   :underline t))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-foreground
                                        :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight bold))))

   `(coffee-mode-function-param
     ((,sha1-class (:foreground ,sha1-violet
                                   :slant italic))
      (,sha1-256-class (:foreground ,sha1-256-violet
                                        :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,sha1-class (:inherit ,sha1-pitch
                                :height ,sha1-height-plus-3
                                :foreground ,sha1-violet
                                :weight bold))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :height ,sha1-height-plus-3
                                     :foreground ,sha1-256-violet
                                     :weight bold))))

   `(custom-variable-tag
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-cyan
                                :height ,sha1-height-plus-3))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :foreground ,sha1-256-cyan
                                     :height ,sha1-height-plus-3))))

   `(custom-comment-tag
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(custom-group-tag
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-blue
                                :height ,sha1-height-plus-3))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :foreground ,sha1-256-blue
                                     :height ,sha1-height-plus-3))))

   `(custom-group-tag-1
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-red
                                :height ,sha1-height-plus-3))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :foreground ,sha1-256-red
                                     :height ,sha1-height-plus-3))))

   `(custom-state
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   ;; diff
   `(diff-added
     ((,sha1-class (:foreground ,sha1-green
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :background ,sha1-256-background))))

   `(diff-changed
     ((,sha1-class (:foreground ,sha1-blue
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :background ,sha1-256-background))))

   `(diff-removed
     ((,sha1-class (:foreground ,sha1-red
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :background ,sha1-256-background))))

   `(diff-header
     ((,sha1-class (:background ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-background))))

   `(diff-file-header
     ((,sha1-class (:background ,sha1-background
                                   :foreground ,sha1-foreground
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-background
                                        :foreground ,sha1-256-foreground
                                        :weight bold))))

   `(diff-refine-added
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-green))))

   `(diff-refine-change
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-blue))))

   `(diff-refine-removed
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,sha1-class (:background ,sha1-yellow-hc-alt
                                   :foreground ,sha1-yellow-hc-alt))
      (,sha1-256-class (:background ,sha1-256-yellow-hc
                                        :foreground ,sha1-256-yellow-hc))))

   `(diff-hl-delete
     ((,sha1-class (:background ,sha1-red-hc-alt
                                   :foreground ,sha1-red-hc-alt))
      (,sha1-256-class (:background ,sha1-256-red-hc
                                        :foreground ,sha1-256-red-hc))))

   `(diff-hl-insert
     ((,sha1-class (:background ,sha1-green-hc-alt
                                   :foreground ,sha1-green-hc-alt))
      (,sha1-256-class (:background ,sha1-256-green-hc
                                        :foreground ,sha1-256-green-hc))))

   `(diff-hl-unknown
     ((,sha1-class (:background ,sha1-violet-hc
                                   :foreground ,sha1-violet-hc))
      (,sha1-256-class (:background ,sha1-256-violet-hc
                                        :foreground ,sha1-256-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,sha1-class (:background ,sha1-diff-red-emphasis))
      (,sha1-256-class (:background ,sha1-256-diff-red-emphasis))))

   `(ediff-fine-diff-B
     ((,sha1-class (:background ,sha1-diff-green-emphasis))
      (,sha1-256-class (:background ,sha1-256-diff-green-emphasis))))

   `(ediff-fine-diff-C
     ((,sha1-class (:background ,sha1-diff-blue-emphasis))
      (,sha1-256-class (:background ,sha1-256-diff-blue-emphasis))))

   `(ediff-current-diff-A
     ((,sha1-class (:background ,sha1-diff-red-base))
      (,sha1-256-class (:background ,sha1-256-diff-red-base))))

   `(ediff-current-diff-B
     ((,sha1-class (:background ,sha1-diff-green-base))
      (,sha1-256-class (:background ,sha1-256-diff-green-base))))

   `(ediff-current-diff-C
     ((,sha1-class (:background ,sha1-diff-blue-base))
      (,sha1-256-class (:background ,sha1-256-diff-blue-base))))

   `(ediff-even-diff-A
     ((,sha1-class (:background ,sha1-comments
                                   :foreground ,sha1-foreground-lc ))
      (,sha1-256-class (:background ,sha1-256-comments
                                        :foreground ,sha1-256-foreground-lc ))))

   `(ediff-odd-diff-A
     ((,sha1-class (:background ,sha1-comments
                                   :foreground ,sha1-foreground-hc ))
      (,sha1-256-class (:background ,sha1-256-comments
                                        :foreground ,sha1-256-foreground-hc ))))

   `(ediff-even-diff-B
     ((,sha1-class (:background ,sha1-comments
                                   :foreground ,sha1-foreground-hc ))
      (,sha1-256-class (:background ,sha1-256-comments
                                        :foreground ,sha1-256-foreground-hc ))))

   `(ediff-odd-diff-B
     ((,sha1-class (:background ,sha1-comments
                                   :foreground ,sha1-foreground-lc ))
      (,sha1-256-class (:background ,sha1-256-comments
                                        :foreground ,sha1-256-foreground-lc ))))

   `(ediff-even-diff-C
     ((,sha1-class (:background ,sha1-comments
                                   :foreground ,sha1-foreground ))
      (,sha1-256-class (:background ,sha1-256-comments
                                        :foreground ,sha1-256-foreground ))))

   `(ediff-odd-diff-C
     ((,sha1-class (:background ,sha1-comments
                                   :foreground ,sha1-background ))
      (,sha1-256-class (:background ,sha1-256-comments
                                        :foreground ,sha1-256-background ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) sha1-class)
       (:underline (:style line :color ,sha1-red)
                   :inherit unspecified))
      (,sha1-class (:foreground ,sha1-red-hc
                                   :background ,sha1-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) sha1-256-class )
       (:underline (:style line :color ,sha1-256-red)
                   :inherit unspecified))
      (,sha1-256-class (:foreground ,sha1-256-red-hc
                                        :background ,sha1-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) sha1-class)
       (:underline (:style line :color ,sha1-yellow)
                   :inherit unspecified))
      (,sha1-class (:foreground ,sha1-yellow-hc
                                   :background ,sha1-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) sha1-256-class )
       (:underline (:style line :color ,sha1-256-yellow)
                   :inherit unspecified))
      (,sha1-256-class (:foreground ,sha1-256-yellow-hc
                                        :background ,sha1-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,sha1-class (:foreground ,sha1-red
                                   :background unspecified
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,sha1-class (:foreground ,sha1-yellow
                                   :background unspecified
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-error-mode-line
     ((,sha1-class (:background ,sha1-red
                                   :foreground unspecified))
      (,sha1-256-class (:background ,sha1-256-red
                                        :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,sha1-class (:background ,sha1-yellow
                                   :foreground unspecified))
      (,sha1-256-class (:background ,sha1-256-yellow
                                        :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(elfeed-search-feed-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(elfeed-search-tag-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(elfeed-search-title-face
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   ;; elixir
   `(elixir-attribute-face
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(elixir-atom-face
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   ;; ein
   `(ein:cell-input-area
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))
   `(ein:cell-input-prompt
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))
   `(ein:cell-output-prompt
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))
   `(ein:notification-tab-normal
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))
   `(ein:notification-tab-selected
     ((,sha1-class (:foreground ,sha1-orange :inherit bold))
      (,sha1-256-class (:foreground ,sha1-256-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,sha1-class (:inherit font-lock-string-face))
      (,sha1-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,sha1-class (:inherit font-lock-string-face))
      (,sha1-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,sha1-class (:inherit font-lock-string-face))
      (,sha1-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,sha1-class (:inherit font-lock-keyword-face))
      (,sha1-256-class (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) sha1-class)
       (:underline (:style wave :color ,sha1-red)
                   :inherit unspecified))
      (,sha1-class (:foreground ,sha1-red-hc
                                   :background ,sha1-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) sha1-256-class )
       (:underline (:style wave :color ,sha1-256-red)
                   :inherit unspecified))
      (,sha1-256-class (:foreground ,sha1-256-red-hc
                                        :background ,sha1-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) sha1-class)
       (:underline (:style wave :color ,sha1-orange)
                   :inherit unspecified))
      (,sha1-class (:foreground ,sha1-orange-hc
                                   :background ,sha1-orange-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) sha1-256-class )
       (:underline (:style wave :color ,sha1-256-orange)
                   :inherit unspecified))
      (,sha1-256-class (:foreground ,sha1-256-orange-hc
                                        :background ,sha1-256-orange-lc
                                        :weight bold
                                        :underline t))))

   ;; epc
   `(epc:face-title
     ((,sha1-class (:foreground ,sha1-blue
                                   :background ,sha1-background
                                   :weight normal
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :background ,sha1-256-background
                                        :weight normal
                                        :underline nil))))

   ;; erc
   `(erc-action-face
     ((,sha1-class (:inherit erc-default-face))
      (,sha1-256-class (:inherit erc-default-face))))

   `(erc-bold-face
     ((,sha1-class (:weight bold))
      (,sha1-256-class (:weight bold))))

   `(erc-current-nick-face
     ((,sha1-class (:foreground ,sha1-blue :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :weight bold))))

   `(erc-dangerous-host-face
     ((,sha1-class (:inherit font-lock-warning-face))
      (,sha1-256-class (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(erc-highlight-face
     ((,sha1-class (:inherit erc-default-face
                                :background ,sha1-highlight))
      (,sha1-256-class (:inherit erc-default-face
                                     :background ,sha1-256-highlight))))

   `(erc-direct-msg-face
     ((,sha1-class (:inherit erc-default-face))
      (,sha1-256-class (:inherit erc-default-face))))

   `(erc-error-face
     ((,sha1-class (:inherit font-lock-warning-face))
      (,sha1-256-class (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,sha1-class (:inherit erc-default-face))
      (,sha1-256-class (:inherit erc-default-face))))

   `(erc-input-face
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(erc-keyword-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :weight bold))))

   `(erc-nick-default-face
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight bold))))

   `(erc-my-nick-face
     ((,sha1-class (:foreground ,sha1-red
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :weight bold))))

   `(erc-nick-msg-face
     ((,sha1-class (:inherit erc-default-face))
      (,sha1-256-class (:inherit erc-default-face))))

   `(erc-notice-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(erc-pal-face
     ((,sha1-class (:foreground ,sha1-orange
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-orange
                                        :weight bold))))

   `(erc-prompt-face
     ((,sha1-class (:foreground ,sha1-orange
                                   :background ,sha1-background
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-orange
                                        :background ,sha1-256-background
                                        :weight bold))))

   `(erc-timestamp-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,sha1-class (:foreground ,sha1-blue
                                   :inherit bold))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :inherit bold))))

   `(eshell-ls-archive
     ((,sha1-class (:foreground ,sha1-red
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :inherit bold))))

   `(eshell-ls-backup
     ((,sha1-class (:inherit font-lock-comment-face))
      (,sha1-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,sha1-class (:inherit font-lock-comment-face))
      (,sha1-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,sha1-class (:foreground ,sha1-blue
                                   :inherit bold))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :inherit bold))))

   `(eshell-ls-executable
     ((,sha1-class (:foreground ,sha1-green
                                   :inherit bold))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :inherit bold))))

   `(eshell-ls-unreadable
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(eshell-ls-missing
     ((,sha1-class (:inherit font-lock-warning-face))
      (,sha1-256-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,sha1-class (:inherit font-lock-doc-face))
      (,sha1-256-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,sha1-class (:foreground ,sha1-yellow
                                   :inherit bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :inherit bold))))

   `(eshell-ls-symlink
     ((,sha1-class (:foreground ,sha1-cyan
                                   :inherit bold))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-red-l
                                   :inherit italic))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-red-l
                                        :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-green-l
                                   :inherit italic))
      (,sha1-256-class (:background ,sha1-256-highlight-line :foreground ,sha1-256-green-l :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,sha1-class (:inherit region))
      (,sha1-256-class (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,sha1-class (:background ,sha1-background
                                   :foreground ,sha1-orange
                                   :underline t
                                   :slant italic))
      (,sha1-256-class (:background ,sha1-256-background
                                        :foreground ,sha1-256-orange
                                        :underline t
                                        :slant italic))))

   `(fic-face
     ((,sha1-class (:background ,sha1-background
                                   :foreground ,sha1-orange
                                   :weight normal
                                   :slant italic))
      (,sha1-256-class (:background ,sha1-256-background
                                        :foreground ,sha1-256-orange
                                        :weight normal
                                        :slant italic))))

   `(font-lock-fic-face
     ((,sha1-class (:background ,sha1-background
                                   :foreground ,sha1-orange
                                   :weight normal
                                   :slant italic))
      (,sha1-256-class (:background ,sha1-256-background
                                        :foreground ,sha1-256-orange
                                        :weight normal
                                        :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :weight normal
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) sha1-class)
       (:underline (:style wave :color ,sha1-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,sha1-class (:foreground ,sha1-red-hc
                                   :background ,sha1-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) sha1-256-class )
       (:underline (:style wave :color ,sha1-256-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,sha1-256-class (:foreground ,sha1-256-red-hc
                                        :background ,sha1-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) sha1-class)
       (:underline (:style wave :color ,sha1-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,sha1-class (:foreground ,sha1-green-hc
                                   :background ,sha1-green-lc))
      (,(append '((supports :underline (:style wave))) sha1-256-class )
       (:underline (:style wave :color ,sha1-256-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,sha1-256-class (:foreground ,sha1-256-green-hc
                                        :background ,sha1-256-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) sha1-class)
       (:underline (:style wave :color ,sha1-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,sha1-class (:foreground ,sha1-yellow-hc
                                   :background ,sha1-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) sha1-256-class )
       (:underline (:style wave :color ,sha1-256-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,sha1-256-class (:foreground ,sha1-256-yellow-hc
                                        :background ,sha1-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style line))) sha1-class)
       (:underline (:style line :color ,sha1-red)))
      (,sha1-class (:foreground ,sha1-red
                                   :background ,sha1-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) sha1-256-class )
       (:underline (:style line :color ,sha1-256-red)))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :background ,sha1-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style line))) sha1-class)
       (:underline (:style line :color ,sha1-orange)))
      (,sha1-class (:foreground ,sha1-orange
                                   :background ,sha1-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) sha1-256-class )
       (:underline (:style line :color ,sha1-256-orange)))
      (,sha1-256-class (:foreground ,sha1-256-orange
                                        :background ,sha1-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style line))) sha1-class)
       (:underline (:style line :color ,sha1-blue)))
      (,sha1-class (:foreground ,sha1-blue
                                   :background ,sha1-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) sha1-256-class )
       (:underline (:style line :color ,sha1-256-blue)))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :background ,sha1-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-fringe-error
     ((,sha1-class (:foreground ,sha1-red-l
                                   :background unspecified
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-red-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-warning
     ((,sha1-class (:foreground ,sha1-orange-l
                                   :background unspecified
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-orange-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-info
     ((,sha1-class (:foreground ,sha1-blue-l
                                   :background unspecified
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-blue-l
                                        :background unspecified
                                        :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) sha1-class)
       (:underline (:style wave :color ,sha1-yellow)
                   :inherit unspecified))
      (,sha1-class (:foreground ,sha1-yellow
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) sha1-256-class )
       (:underline (:style wave :color ,sha1-256-yellow)
                   :inherit unspecified))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight bold
                                        :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) sha1-class)
       (:underline (:style wave :color ,sha1-red)
                   :inherit unspecified))
      (,sha1-class (:foreground ,sha1-red
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) sha1-256-class )
       (:underline (:style wave :color ,sha1-256-red)
                   :inherit unspecified))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :weight bold
                                        :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,sha1-class (:background ,sha1-green
                                   :foreground ,sha1-background
                                   :inherit bold))
      (,sha1-256-class (:background ,sha1-256-green
                                        :foreground ,sha1-256-background
                                        :inherit bold))))

   `(git-gutter:deleted
     ((,sha1-class (:background ,sha1-red
                                   :foreground ,sha1-background
                                   :inherit bold))
      (,sha1-256-class (:background ,sha1-256-red
                                        :foreground ,sha1-256-background
                                        :inherit bold))))

   `(git-gutter:modified
     ((,sha1-class (:background ,sha1-blue
                                   :foreground ,sha1-background
                                   :inherit bold))
      (,sha1-256-class (:background ,sha1-256-blue
                                        :foreground ,sha1-256-background
                                        :inherit bold))))

   `(git-gutter:unchanged
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-background
                                   :inherit bold))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-background
                                        :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,sha1-class (:foreground ,sha1-green
                                   :inherit bold))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :inherit bold))))

   `(git-gutter-fr:deleted
     ((,sha1-class (:foreground ,sha1-red
                                   :inherit bold))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :inherit bold))))

   `(git-gutter-fr:modified
     ((,sha1-class (:foreground ,sha1-blue
                                   :inherit bold))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,sha1-class (:background ,sha1-green
                                   :foreground ,sha1-background
                                   :inherit bold))
      (,sha1-256-class (:background ,sha1-256-green
                                        :foreground ,sha1-256-background
                                        :inherit bold))))

   `(git-gutter+-deleted
     ((,sha1-class (:background ,sha1-red
                                   :foreground ,sha1-background
                                   :inherit bold))
      (,sha1-256-class (:background ,sha1-256-red
                                        :foreground ,sha1-256-background
                                        :inherit bold))))

   `(git-gutter+-modified
     ((,sha1-class (:background ,sha1-blue
                                   :foreground ,sha1-background
                                   :inherit bold))
      (,sha1-256-class (:background ,sha1-256-blue
                                        :foreground ,sha1-256-background
                                        :inherit bold))))

   `(git-gutter+-unchanged
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-background
                                   :inherit bold))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-background
                                        :inherit bold))))

   `(git-gutter-fr+-added
     ((,sha1-class (:foreground ,sha1-green
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :weight bold))))

   `(git-gutter-fr+-deleted
     ((,sha1-class (:foreground ,sha1-red
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :weight bold))))

   `(git-gutter-fr+-modified
     ((,sha1-class (:foreground ,sha1-blue
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :background ,sha1-highlight-line
                                   :inherit bold))
      (,sha1-256-class (:foreground ,sha1-blue
                                        :background ,sha1-256-highlight-line
                                        :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(guide-key/key-face
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(guide-key/prefix-command-face
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,sha1-class (:weight bold
                               :inherit gnus-group-mail-1-empty))
      (,sha1-256-class (:weight bold
                                    :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,sha1-class (:inherit gnus-group-news-1-empty))
      (,sha1-256-class (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,sha1-class (:weight bold
                               :inherit gnus-group-mail-2-empty))
      (,sha1-256-class (:weight bold
                                    :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,sha1-class (:inherit gnus-group-news-2-empty))
      (,sha1-256-class (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,sha1-class (:weight bold
                               :inherit gnus-group-mail-3-empty))
      (,sha1-256-class (:weight bold
                                    :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,sha1-class (:inherit gnus-group-news-3-empty))
      (,sha1-256-class (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,sha1-class (:weight bold
                               :inherit gnus-group-mail-low-empty))
      (,sha1-256-class (:weight bold
                                    :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,sha1-class (:inherit gnus-group-news-low-empty))
      (,sha1-256-class (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,sha1-class (:weight bold
                               :inherit gnus-group-news-1-empty))
      (,sha1-256-class (:weight bold
                                    :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,sha1-class (:weight bold
                               :inherit gnus-group-news-2-empty))
      (,sha1-256-class (:weight bold
                                    :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,sha1-class (:weight bold
                               :inherit gnus-group-news-3-empty))
      (,sha1-256-class (:weight bold
                                    :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,sha1-class (:weight bold
                               :inherit gnus-group-news-4-empty))
      (,sha1-256-class (:weight bold
                                    :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,sha1-class (:weight bold
                               :inherit gnus-group-news-5-empty))
      (,sha1-256-class (:weight bold
                                    :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,sha1-class (:weight bold
                               :inherit gnus-group-news-6-empty))
      (,sha1-256-class (:weight bold
                                    :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,sha1-class (:weight bold
                               :inherit gnus-group-news-low-empty))
      (,sha1-256-class (:weight bold
                                    :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,sha1-class (:inherit message-header-other))
      (,sha1-256-class (:inherit message-header-other))))

   `(gnus-header-from
     ((,sha1-class (:inherit message-header-other))
      (,sha1-256-class (:inherit message-header-other))))

   `(gnus-header-name
     ((,sha1-class (:inherit message-header-name))
      (,sha1-256-class (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,sha1-class (:inherit message-header-other))
      (,sha1-256-class (:inherit message-header-other))))

   `(gnus-header-subject
     ((,sha1-class (:inherit message-header-subject))
      (,sha1-256-class (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(gnus-summary-high-ancient
     ((,sha1-class (:foreground ,sha1-blue
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :weight bold))))

   `(gnus-summary-high-read
     ((,sha1-class (:foreground ,sha1-green
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :weight bold))))

   `(gnus-summary-high-ticked
     ((,sha1-class (:foreground ,sha1-orange
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-orange
                                        :weight bold))))

   `(gnus-summary-high-unread
     ((,sha1-class (:foreground ,sha1-foreground
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :weight bold))))

   `(gnus-summary-low-ancient
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(gnus-summary-low-read
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(gnus-summary-low-ticked
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(gnus-summary-low-unread
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(gnus-summary-normal-ancient
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(gnus-summary-normal-read
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(gnus-summary-normal-ticked
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(gnus-summary-normal-unread
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(gnus-summary-selected
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight bold))))

   `(gnus-cite-1
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(gnus-cite-2
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(gnus-cite-3
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(gnus-cite-4
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(gnus-cite-5
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(gnus-cite-6
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(gnus-cite-7
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(gnus-cite-8
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(gnus-cite-9
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(gnus-cite-10
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(gnus-cite-11
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(gnus-group-news-1-empty
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(gnus-group-news-2-empty
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(gnus-group-news-3-empty
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(gnus-group-news-4-empty
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(gnus-group-news-5-empty
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(gnus-group-news-6-empty
     ((,sha1-class (:foreground ,sha1-blue-lc))
      (,sha1-256-class (:foreground ,sha1-256-blue-lc))))

   `(gnus-group-news-low-empty
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(gnus-signature
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(gnus-x-face
     ((,sha1-class (:background ,sha1-foreground
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-foreground
                                        :foreground ,sha1-256-background))))


   ;; helm
   `(helm-apt-deinstalled
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(helm-apt-installed
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(helm-bookmark-directory
     ((,sha1-class (:inherit helm-ff-directory))
      (,sha1-256-class (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(helm-bookmark-gnus
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(helm-bookmark-info
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(helm-bookmark-man
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(helm-bookmark-w3m
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(helm-bookmarks-su
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(helm-buffer-file
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(helm-buffer-directory
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(helm-buffer-process
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(helm-buffer-saved-out
     ((,sha1-class (:foreground ,sha1-red
                                   :background ,sha1-background
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :background ,sha1-256-background
                                        :inverse-video t))))

   `(helm-buffer-size
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(helm-candidate-number
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-emphasis
                                   :bold t))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-emphasis
                                        :bold t))))

   `(helm-ff-directory
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(helm-ff-executable
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(helm-ff-file
     ((,sha1-class (:background ,sha1-background
                                   :foreground ,sha1-foreground))
      (,sha1-256-class (:background ,sha1-256-background
                                        :foreground ,sha1-256-foreground))))

   `(helm-ff-invalid-symlink
     ((,sha1-class (:background ,sha1-background
                                   :foreground ,sha1-orange
                                   :slant italic))
      (,sha1-256-class (:background ,sha1-256-background
                                        :foreground ,sha1-256-orange
                                        :slant italic))))

   `(helm-ff-prefix
     ((,sha1-class (:background ,sha1-green
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-green
                                        :foreground ,sha1-256-background))))

   `(helm-ff-symlink
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(helm-grep-file
     ((,sha1-class (:foreground ,sha1-cyan
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :underline t))))

   `(helm-grep-finish
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(helm-grep-lineno
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(helm-grep-match
     ((,sha1-class (:inherit helm-match))
      (,sha1-256-class (:inherit helm-match))))

   `(helm-grep-running
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(helm-header
     ((,sha1-class (:inherit header-line))
      (,sha1-256-class (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(helm-lisp-show-completion
     ((,sha1-class (:foreground ,sha1-yellow
                                   :background ,sha1-highlight-line
                                   :bold t))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :background ,sha1-256-highlight-line
                                        :bold t))))

   `(helm-M-x-key
     ((,sha1-class (:foreground ,sha1-orange
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-orange
                                        :underline t))))

   `(helm-moccur-buffer
     ((,sha1-class (:foreground ,sha1-cyan
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :underline t))))

   `(helm-match
     ((,sha1-class (:foreground ,sha1-green :inherit bold))
      (,sha1-256-class (:foreground ,sha1-256-green :inherit bold))))

   `(helm-match-item
     ((,sha1-class (:inherit helm-match))
      (,sha1-256-class (:inherit helm-match))))

   `(helm-selection
     ((,sha1-class (:background ,sha1-highlight
                                   :inherit bold
                                   :underline nil))
      (,sha1-256-class (:background ,sha1-256-highlight
                                        :inherit bold
                                        :underline nil))))

   `(helm-selection-line
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-emphasis
                                   :underline nil))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-emphasis
                                        :underline nil))))

   `(helm-separator
     ((,sha1-class (:foreground ,sha1-gray))
      (,sha1-256-class (:foreground ,sha1-256-gray))))

   `(helm-source-header
     ((,sha1-class (:background ,sha1-violet-l
                                   :foreground ,sha1-background
                                   :underline nil))
      (,sha1-256-class (:background ,sha1-256-violet-l
                                        :foreground ,sha1-256-background
                                        :underline nil))))

   `(helm-swoop-target-line-face
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   `(helm-swoop-target-word-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(helm-time-zone-current
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(helm-time-zone-home
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(helm-visible-mark
     ((,sha1-class (:background ,sha1-background
                                   :foreground ,sha1-magenta :bold t))
      (,sha1-256-class (:background ,sha1-256-background
                                        :foreground ,sha1-256-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,sha1-class :foreground ,sha1-blue)
      (,sha1-256-class  :foreground ,sha1-256-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,sha1-class :foreground ,sha1-blue-l)
      (,sha1-256-class  :foreground ,sha1-256-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,sha1-class :foreground ,sha1-blue-l)
      (,sha1-256-class  :foreground ,sha1-256-blue-l)))

   `(helm-ls-git-untracked-face
     ((,sha1-class :foreground ,sha1-orange)
      (,sha1-256-class  :foreground ,sha1-256-orange)))

   `(helm-ls-git-added-copied-face
     ((,sha1-class :foreground ,sha1-green)
      (,sha1-256-class  :foreground ,sha1-256-green)))

   `(helm-ls-git-added-modified-face
     ((,sha1-class :foreground ,sha1-green-l)
      (,sha1-256-class  :foreground ,sha1-256-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,sha1-class :foreground ,sha1-red)
      (,sha1-256-class  :foreground ,sha1-256-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,sha1-class :foreground ,sha1-red-l)
      (,sha1-256-class  :foreground ,sha1-256-red-l)))

   `(helm-ls-git-conflict-face
     ((,sha1-class :foreground ,sha1-yellow)
      (,sha1-256-class  :foreground ,sha1-256-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,sha1-class (:foreground ,sha1-yellow-lc
                                   :background ,sha1-yellow-hc))
      (,sha1-256-class (:foreground ,sha1-256-yellow-lc
                                        :background ,sha1-256-yellow-hc))))

   `(hi-pink
     ((,sha1-class (:foreground ,sha1-magenta-lc
                                   :background ,sha1-magenta-hc))
      (,sha1-256-class (:foreground ,sha1-256-magenta-lc
                                        :background ,sha1-256-magenta-hc))))

   `(hi-green
     ((,sha1-class (:foreground ,sha1-green-lc
                                   :background ,sha1-green-hc))
      (,sha1-256-class (:foreground ,sha1-256-green-lc
                                        :background ,sha1-256-green-hc))))

   `(hi-blue
     ((,sha1-class (:foreground ,sha1-blue-lc
                                   :background ,sha1-blue-hc))
      (,sha1-256-class (:foreground ,sha1-256-blue-lc
                                        :background ,sha1-256-blue-hc))))

   `(hi-black-b
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :background ,sha1-background
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :background ,sha1-256-background
                                        :weight bold))))

   `(hi-blue-b
     ((,sha1-class (:foreground ,sha1-blue-lc
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-blue-lc
                                        :weight bold))))

   `(hi-green-b
     ((,sha1-class (:foreground ,sha1-green-lc
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-green-lc
                                        :weight bold))))

   `(hi-red-b
     ((,sha1-class (:foreground ,sha1-red
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-red
                                   :weight bold))))

   `(hi-black-hb
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :background ,sha1-background
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :background ,sha1-256-background
                                        :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(highlight-changes-delete
     ((,sha1-class (:foreground ,sha1-red
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,sha1-class (:background ,sha1-dark-gray))
      (,sha1-256-class (:background ,sha1-256-dark-gray))))

   `(highlight-indentation-current-column-face
     ((,sha1-class (:background ,sha1-dark-gray))
      (,sha1-256-class (:background ,sha1-256-dark-gray))))

   ;; highlight-symbol
   `(highlight-symbol-face
     ((,sha1-class (:background ,sha1-highlight))
      (,sha1-256-class (:background ,sha1-256-highlight))))

   ;; hl-line-mode
   `(hl-line
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   `(hl-line-face
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,sha1-class (:foreground ,sha1-blue-hc
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-blue-hc
                                        :weight normal))))

   `(ido-only-match
     ((,sha1-class (:foreground ,sha1-blue-hc
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-blue-hc
                                        :weight normal))))

   `(ido-subdir
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(ido-incomplete-regexp
     ((,sha1-class (:foreground ,sha1-red
                                   :weight bold ))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :weight bold ))))

   `(ido-indicator
     ((,sha1-class (:background ,sha1-red
                                   :foreground ,sha1-background
                                   :width condensed))
      (,sha1-256-class (:background ,sha1-256-red
                                        :foreground ,sha1-256-background
                                        :width condensed))))

   `(ido-virtual
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   ;; info
   `(info-header-xref
     ((,sha1-class (:foreground ,sha1-green
                                   :inherit bold
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :inherit bold
                                        :underline t))))

   `(info-menu
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(info-node
     ((,sha1-class (:foreground ,sha1-violet
                                   :inherit bold))
      (,sha1-256-class (:foreground ,sha1-256-violet
                                        :inherit bold))))

   `(info-quoted-name
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(info-reference-item
     ((,sha1-class (:background nil
                                   :underline t
                                   :inherit bold))
      (,sha1-256-class (:background nil
                                        :underline t
                                        :inherit bold))))

   `(info-string
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(info-title-1
     ((,sha1-class (:height ,sha1-height-plus-4))
      (,sha1-256-class (:height ,sha1-height-plus-4))))

   `(info-title-2
     ((,sha1-class (:height ,sha1-height-plus-3))
      (,sha1-256-class (:height ,sha1-height-plus-3))))

   `(info-title-3
     ((,sha1-class (:height ,sha1-height-plus-2))
      (,sha1-256-class (:height ,sha1-height-plus-2))))

   `(info-title-4
     ((,sha1-class (:height ,sha1-height-plus-1))
      (,sha1-256-class (:height ,sha1-height-plus-1))))

   ;; ivy
   `(ivy-current-match
     ((,sha1-class (:background ,sha1-gray :inherit bold))
      (,sha1-256-class (:background ,sha1-gray-l :inherit bold))))

   `(ivy-minibuffer-match-face-1
     ((,sha1-class (:inherit bold))
      (,sha1-256-class (:inherit bold))))

   `(ivy-minibuffer-match-face-2
     ((,sha1-class (:foreground ,sha1-violet
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-violet
                                        :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,sha1-class (:foreground ,sha1-green
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,sha1-class (:foreground ,sha1-yellow
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :underline t))))

   `(ivy-remote
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(swiper-line-face
     ((,sha1-class (:background ,sha1-highlight-line))))

   `(swiper-match-face-1
     ((,sha1-class (:background ,sha1-gray-d))))

   `(swiper-match-face-2
     ((,sha1-class (:background ,sha1-green))))

   `(swiper-match-face-3
     ((,sha1-class (:background ,sha1-orange))))

   `(swiper-match-face-4
     ((,sha1-class (:background ,sha1-magenta))))

   ;; jabber
   `(jabber-activity-face
     ((,sha1-class (:weight bold
                               :foreground ,sha1-red))
      (,sha1-256-class (:weight bold
                                    :foreground ,sha1-256-red))))

   `(jabber-activity-personal-face
     ((,sha1-class (:weight bold
                               :foreground ,sha1-blue))
      (,sha1-256-class (:weight bold
                                    :foreground ,sha1-256-blue))))

   `(jabber-chat-error
     ((,sha1-class (:weight bold
                               :foreground ,sha1-red))
      (,sha1-256-class (:weight bold
                                    :foreground ,sha1-256-red))))

   `(jabber-chat-prompt-foreign
     ((,sha1-class (:weight bold
                               :foreground ,sha1-red))
      (,sha1-256-class (:weight bold
                                    :foreground ,sha1-256-red))))

   `(jabber-chat-prompt-local
     ((,sha1-class (:weight bold
                               :foreground ,sha1-blue))
      (,sha1-256-class (:weight bold
                                    :foreground ,sha1-256-blue))))

   `(jabber-chat-prompt-system
     ((,sha1-class (:weight bold
                               :foreground ,sha1-green))
      (,sha1-256-class (:weight bold
                                    :foreground ,sha1-256-green))))

   `(jabber-chat-text-foreign
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(jabber-chat-text-local
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(jabber-chat-rare-time-face
     ((,sha1-class (:underline t
                                  :foreground ,sha1-green))
      (,sha1-256-class (:underline t
                                       :foreground ,sha1-256-green))))

   `(jabber-roster-user-away
     ((,sha1-class (:slant italic
                              :foreground ,sha1-green))
      (,sha1-256-class (:slant italic
                                   :foreground ,sha1-256-green))))

   `(jabber-roster-user-chatty
     ((,sha1-class (:weight bold
                               :foreground ,sha1-orange))
      (,sha1-256-class (:weight bold
                                    :foreground ,sha1-256-orange))))

   `(jabber-roster-user-dnd
     ((,sha1-class (:slant italic
                              :foreground ,sha1-red))
      (,sha1-256-class (:slant italic
                                   :foreground ,sha1-256-red))))

   `(jabber-roster-user-error
     ((,sha1-class (:weight light
                               :slant italic
                               :foreground ,sha1-red))
      (,sha1-256-class (:weight light
                                    :slant italic
                                    :foreground ,sha1-256-red))))

   `(jabber-roster-user-offline
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(jabber-roster-user-online
     ((,sha1-class (:weight bold
                               :foreground ,sha1-blue))
      (,sha1-256-class (:weight bold
                                    :foreground ,sha1-256-blue))))

   `(jabber-roster-user-xa
     ((,sha1-class (:slant italic
                              :foreground ,sha1-magenta))
      (,sha1-256-class (:slant italic
                                   :foreground ,sha1-256-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(js2-external-variable
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(js2-function-call
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(js2-function-param
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(js2-instance-member
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(js2-jsdoc-html-tag-delimiter
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(js2-jsdoc-html-tag-name
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(js2-jsdoc-tag
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(js2-jsdoc-type
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(js2-jsdoc-value
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(js2-magic-paren
     ((,sha1-class (:underline t))
      (,sha1-256-class (:underline t))))

   `(js2-object-property
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(js2-private-function-call
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(js2-private-member
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(js2-warning
     ((,sha1-class (:underline ,sha1-orange))
      (,sha1-256-class (:underline ,sha1-256-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,sha1-class (:inherit bold))
      (,sha1-256-class (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,sha1-class (:foreground ,sha1-line-number
                                   :background ,sha1-fringe-bg
                                   :inherit default
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-line-number
                                        :background ,sha1-256-fringe-bg
                                        :inherit default
                                        :underline nil))))

   ;; line-number (>= Emacs26)
   `(line-number
     ((,sha1-class (:foreground ,sha1-line-number
                                   :background ,sha1-fringe-bg
                                   :inherit default
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-line-number
                                        :background ,sha1-256-fringe-bg
                                        :inherit default
                                        :underline nil))))
   `(line-number-current-line
     ((,sha1-class (:foreground ,sha1-foreground
                                   :background ,sha1-fringe-bg
                                   :inherit default
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :background ,sha1-256-fringe-bg
                                        :inherit default
                                        :underline nil))))

   ;; linum-relative-current-face
   `(linum-relative-current-face
     ((,sha1-class (:foreground ,sha1-line-number
                                   :background ,sha1-highlight-line
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-line-number
                                        :background ,sha1-256-highlight-line
                                        :underline nil))))

   ;; lsp-mode
   `(lsp-ui-doc-header
     ((,sha1-class (:inherit org-document-title))
      (,sha1-256-class (:inherit org-document-title))))

   `(lsp-ui-doc-background
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-highlight-line))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,sha1-class (:inherit disha1-red-directory))
      (,sha1-256-class (:inherit disha1-red-directory))))

   `(lusty-file-face
     ((,sha1-class nil)
      (,sha1-256-class  nil)))

   `(lusty-match-face
     ((,sha1-class (:inherit ido-first-match))
      (,sha1-256-class (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,sha1-class (:foreground ,sha1-cyan
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :weight bold))))

   ;; magit
   `(magit-bisect-good
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(magit-bisect-skip
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(magit-bisect-bad
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(magit-blame-highlight
     ((,sha1-class (:foreground ,sha1-foreground
                                   :background ,sha1-highlight-alt))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                       :background ,sha1-256-highlight-alt))))

   `(magit-diff-file-heading-selection
     ((,sha1-class (:inherit magit-diff-file-heading-highlight
                                :foreground ,sha1-orange-d))
      (,sha1-256-class (:inherit magit-diff-file-heading-highlight
                                   :foreground ,sha1-256-orange-d))))

   `(magit-diff-hunk-heading
     ((,sha1-class (:foreground ,sha1-gray-d
                                   :background ,sha1-gray-l))
      (,sha1-256-class (:foreground ,sha1-256-gray-d
                                       :background ,sha1-256-gray-l))))

   `(magit-diff-hunk-heading-highlight
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-background
                                       :background ,sha1-256-foreground))))

   `(magit-diff-hunk-heading-selection
     ((,sha1-class (:inherit magit-diff-hunk-heading-highlight
                                :foreground ,sha1-orange))
      (,sha1-256-class (:inherit magit-diff-hunk-heading-highlight
                                    :foreground ,sha1-256-orange))))

   `(magit-diff-lines-heading
     ((,sha1-class (:inherit magit-diff-hunk-heading-highlight
                                :foreground ,sha1-background
                                :background ,sha1-orange-l))
      (,sha1-256-class (:inherit magit-diff-hunk-heading-highlight
                                    :foreground ,sha1-256-background
                                    :background ,sha1-256-orange-l))))

   `(magit-diff-added
     ((,sha1-class (:foreground ,sha1-green
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :background ,sha1-256-background))))

   `(magit-diff-removed
     ((,sha1-class (:foreground ,sha1-red
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-red
                                       :background ,sha1-256-background))))

   `(magit-diff-base
     ((,sha1-class (:foreground ,sha1-yellow
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                       :background ,sha1-256-background))))

   `(magit-diff-context
     ((,sha1-class (:foreground ,sha1-gray-l))
      (,sha1-256-class (:foreground ,sha1-256-gray-l))))

   `(magit-diff-added-highlight
     ((,sha1-class (:foreground ,sha1-green
                                   :background ,sha1-highlight-alt))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :background ,sha1-256-highlight-alt))))

   `(magit-diff-removed-highlight
     ((,sha1-class (:foreground ,sha1-red
                                   :background ,sha1-highlight-alt))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :background ,sha1-256-highlight-alt))))

   `(magit-diff-base-highlight
     ((,sha1-class (:foreground ,sha1-yellow
                                   :background ,sha1-highlight-alt))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                       :background ,sha1-256-highlight-alt))))

   `(magit-diff-context-highlight
     ((,sha1-class (:foreground ,sha1-foreground
                                   :background ,sha1-highlight-alt))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                       :background ,sha1-256-highlight-alt))))

   `(magit-diffstat-added
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(magit-diffstat-removed
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(magit-log-graph
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(magit-log-author
     ((,sha1-class (:foreground ,sha1-red-d
                                   :slant normal
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-red-d
                                       :slant normal
                                       :weight normal))))

   `(magit-log-date
     ((,sha1-class (:foreground ,sha1-gray
                                   :slant normal
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-gray
                                       :slant normal
                                       :weight normal))))

   `(magit-process-ok
     ((,sha1-class (:inherit magit-section-heading
                                :foreground ,sha1-green))
      (,sha1-256-class (:inherit magit-section-heading
                                    :foreground ,sha1-256-green))))

   `(magit-process-ng
     ((,sha1-class (:inherit magit-section-heading
                                :foreground ,sha1-red))
      (,sha1-256-class (:inherit magit-section-heading
                                    :foreground ,sha1-256-red))))

   `(magit-reflog-commit
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(magit-reflog-amend
     ((,sha1-class (:foreground ,sha1-magenta))
      (,sha1-256-class (:foreground ,sha1-256-magenta))))

   `(magit-reflog-merge
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(magit-reflog-checkout
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(magit-reflog-reset
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(magit-reflog-rebase
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(magit-reflog-cherry-pick
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(magit-reflog-remote
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(magit-reflog-other
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(magit-section-highlight
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   `(magit-section-heading
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                       :weight bold))))

   `(magit-section-heading-selection
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(magit-sequence-stop
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(magit-sequence-part
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(magit-sequence-head
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(magit-sequence-drop
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(magit-dimmed
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(magit-hash
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(magit-tag
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(magit-branch-remote
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(magit-branch-local
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(magit-refname
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(magit-signature-good
     ((,sha1-class (:foreground ,sha1-green-d))
      (,sha1-256-class (:foreground ,sha1-256-green-d))))

   `(magit-signature-bad
     ((,sha1-class (:foreground ,sha1-red-d
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-red-d
                                       :weight bold))))

   `(magit-signature-untrusted
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(magit-signature-expired
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(magit-signature-revoked
     ((,sha1-class (:foreground ,sha1-magenta))
      (,sha1-256-class (:foreground ,sha1-256-magenta))))

   `(magit-signature-error
     ((,sha1-class (:foreground ,sha1-red-l))
      (,sha1-256-class (:foreground ,sha1-256-red-l))))

   `(magit-cherry-unmatched
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(magit-cherry-equivalent
     ((,sha1-class (:foreground ,sha1-magenta))
      (,sha1-256-class (:foreground ,sha1-256-magenta))))

   ;; man
   `(Man-overstrike
     ((,sha1-class (:foreground ,sha1-blue
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :weight bold))))

   `(Man-reverse
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(Man-underline
     ((,sha1-class (:foreground ,sha1-green :underline t))
      (,sha1-256-class (:foreground ,sha1-256-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight bold))))

   `(monky-diff-add
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(monky-diff-del
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(markdown-header-face-1
     ((,sha1-class (:inherit markdown-header-face
                                :height ,sha1-height-plus-4))
      (,sha1-256-class (:inherit markdown-header-face
                                     :height ,sha1-height-plus-4))))

   `(markdown-header-face-2
     ((,sha1-class (:inherit markdown-header-face
                                :height ,sha1-height-plus-3))
      (,sha1-256-class (:inherit markdown-header-face
                                     :height ,sha1-height-plus-3))))

   `(markdown-header-face-3
     ((,sha1-class (:inherit markdown-header-face
                                :height ,sha1-height-plus-2))
      (,sha1-256-class (:inherit markdown-header-face
                                     :height ,sha1-height-plus-2))))

   `(markdown-header-face-4
     ((,sha1-class (:inherit markdown-header-face
                                :height ,sha1-height-plus-1))
      (,sha1-256-class (:inherit markdown-header-face
                                     :height ,sha1-height-plus-1))))

   `(markdown-header-face-5
     ((,sha1-class (:inherit markdown-header-face))
      (,sha1-256-class (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,sha1-class (:inherit markdown-header-face))
      (,sha1-256-class (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(message-header-name
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(message-header-other
     ((,sha1-class (:foreground ,sha1-foreground
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :weight normal))))

   `(message-header-to
     ((,sha1-class (:foreground ,sha1-foreground
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :weight normal))))

   `(message-header-cc
     ((,sha1-class (:foreground ,sha1-foreground
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :weight normal))))

   `(message-header-newsgroups
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight bold))))

   `(message-header-subject
     ((,sha1-class (:foreground ,sha1-cyan
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :weight normal))))

   `(message-header-xheader
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(message-mml
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight bold))))

   `(message-separator
     ((,sha1-class (:foreground ,sha1-comments
                                   :slant italic))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(mew-face-header-from
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(mew-face-header-date
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(mew-face-header-to
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(mew-face-header-key
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(mew-face-header-private
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(mew-face-header-important
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(mew-face-header-marginal
     ((,sha1-class (:foreground ,sha1-foreground
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :weight bold))))

   `(mew-face-header-warning
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(mew-face-header-xmew
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(mew-face-header-xmew-bad
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(mew-face-body-url
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(mew-face-body-comment
     ((,sha1-class (:foreground ,sha1-foreground
                                   :slant italic))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :slant italic))))

   `(mew-face-body-cite1
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(mew-face-body-cite2
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(mew-face-body-cite3
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(mew-face-body-cite4
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(mew-face-body-cite5
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(mew-face-mark-review
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(mew-face-mark-escape
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(mew-face-mark-delete
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(mew-face-mark-unlink
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(mew-face-mark-refile
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(mew-face-mark-unread
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(mew-face-eof-message
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(mew-face-eof-part
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(mingus-pausing-face
     ((,sha1-class (:foreground ,sha1-magenta))
      (,sha1-256-class (:foreground ,sha1-256-magenta))))

   `(mingus-playing-face
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(mingus-playlist-face
     ((,sha1-class (:foreground ,sha1-cyan ))
      (,sha1-256-class (:foreground ,sha1-256-cyan ))))

   `(mingus-song-file-face
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(mingus-stopped-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,sha1-class (:background ,sha1-violet-d))
      (,sha1-256-class (:background ,sha1-256-violet-d))))

   `(mmm-cleanup-submode-face
     ((,sha1-class (:background ,sha1-orange-d))
      (,sha1-256-class (:background ,sha1-256-orange-d))))

   `(mmm-declaration-submode-face
     ((,sha1-class (:background ,sha1-cyan-d))
      (,sha1-256-class (:background ,sha1-256-cyan-d))))

   `(mmm-comment-submode-face
     ((,sha1-class (:background ,sha1-blue-d))
      (,sha1-256-class (:background ,sha1-256-blue-d))))

   `(mmm-output-submode-face
     ((,sha1-class (:background ,sha1-red-d))
      (,sha1-256-class (:background ,sha1-256-red-d))))

   `(mmm-special-submode-face
     ((,sha1-class (:background ,sha1-green-d))
      (,sha1-256-class (:background ,sha1-256-green-d))))

   `(mmm-code-submode-face
     ((,sha1-class (:background ,sha1-gray))
      (,sha1-256-class (:background ,sha1-256-gray))))

   `(mmm-default-submode-face
     ((,sha1-class (:background ,sha1-gray-d))
      (,sha1-256-class (:background ,sha1-256-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,sha1-class (:underline t))
      (,sha1-256-class (:underline t))))

   `(moccur-edit-done-face
     ((,sha1-class (:foreground ,sha1-comments
                                   :background ,sha1-background
                                   :slant italic))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :background ,sha1-256-background
                                        :slant italic))))

   `(moccur-edit-face
     ((,sha1-class (:background ,sha1-yellow
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-yellow
                                        :foreground ,sha1-256-background))))

   `(moccur-edit-file-face
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   `(moccur-edit-reject-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(moccur-face
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-emphasis
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-emphasis
                                        :weight bold))))

   `(search-buffers-face
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-emphasis
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-emphasis
                                        :weight bold))))

   `(search-buffers-header-face
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-yellow
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-yellow
                                        :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,sha1-class (:foreground ,sha1-green
                                   :slant italic
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-2-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :slant italic
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-3-face
     ((,sha1-class (:foreground ,sha1-orange
                                   :slant italic
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-orange
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-4-face
     ((,sha1-class (:foreground ,sha1-yellow
                                   :slant italic
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-5-face
     ((,sha1-class (:foreground ,sha1-cyan
                                   :slant italic
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-6-face
     ((,sha1-class (:foreground ,sha1-green
                                   :slant italic
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-7-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :slant italic
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-flagged-face
     ((,sha1-class (:foreground ,sha1-magenta
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-magenta
                                        :weight bold))))

   `(mu4e-view-url-number-face
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight normal))))

   `(mu4e-warning-face
     ((,sha1-class (:foreground ,sha1-red
                                   :slant normal
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :slant normal
                                        :weight bold))))

   `(mu4e-header-highlight-face
     ((,sha1-class (:inherit unspecified
                                :foreground unspecified
                                :background ,sha1-highlight-line
                                :underline ,sha1-emphasis
                                :weight normal))
      (,sha1-256-class (:inherit unspecified
                                     :foreground unspecified
                                     :background ,sha1-256-highlight-line
                                     :underline ,sha1-256-emphasis
                                     :weight normal))))


   `(mu4e-draft-face
     ((,sha1-class (:inherit font-lock-string-face))
      (,sha1-256-class (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,sha1-class (:inherit font-lock-comment-face))
      (,sha1-256-class (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,sha1-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,sha1-256-class (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-header-face
     ((,sha1-class (:inherit default))
      (,sha1-256-class (:inherit default))))

   `(mu4e-header-marks-face
     ((,sha1-class (:inherit font-lock-preprocessor-face))
      (,sha1-256-class (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,sha1-class (:inherit font-lock-type-face))
      (,sha1-256-class (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,sha1-class (:inherit font-lock-pseudo-keyword-face
                                :weight bold))
      (,sha1-256-class (:inherit font-lock-pseudo-keyword-face
                                     :weight bold))))

   `(mu4e-moved-face
     ((,sha1-class (:inherit font-lock-comment-face
                                :slant italic))
      (,sha1-256-class (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-ok-face
     ((,sha1-class (:inherit font-lock-comment-face
                                :slant normal
                                :weight bold))
      (,sha1-256-class (:inherit font-lock-comment-face
                                     :slant normal
                                     :weight bold))))

   `(mu4e-replied-face
     ((,sha1-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,sha1-256-class (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-system-face
     ((,sha1-class (:inherit font-lock-comment-face
                                :slant italic))
      (,sha1-256-class (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-title-face
     ((,sha1-class (:inherit font-lock-type-face
                                :weight bold))
      (,sha1-256-class (:inherit font-lock-type-face
                                     :weight bold))))

   `(mu4e-trashed-face
     ((,sha1-class (:inherit font-lock-comment-face
                                :strike-through t))
      (,sha1-256-class (:inherit font-lock-comment-face
                                     :strike-through t))))

   `(mu4e-unread-face
     ((,sha1-class (:inherit font-lock-keyword-face
                                :weight bold))
      (,sha1-256-class (:inherit font-lock-keyword-face
                                     :weight bold))))

   `(mu4e-view-attach-number-face
     ((,sha1-class (:inherit font-lock-variable-name-face
                                :weight bold))
      (,sha1-256-class (:inherit font-lock-variable-name-face
                                     :weight bold))))

   `(mu4e-view-contact-face
     ((,sha1-class (:foreground ,sha1-foreground
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :weight normal))))

   `(mu4e-view-header-key-face
     ((,sha1-class (:inherit message-header-name
                                :weight normal))
      (,sha1-256-class (:inherit message-header-name
                                     :weight normal))))

   `(mu4e-view-header-value-face
     ((,sha1-class (:foreground ,sha1-cyan
                                   :weight normal
                                   :slant normal))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :weight normal
                                        :slant normal))))

   `(mu4e-view-link-face
     ((,sha1-class (:inherit link))
      (,sha1-256-class (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :weight normal
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(nav-face-button-num
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(nav-face-dir
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(nav-face-hdir
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(nav-face-file
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(nav-face-hfile
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :background ,sha1-background
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :background ,sha1-256-background
                                        :weight bold))))


   `(neo-header-face
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :background ,sha1-256-background))))

   `(neo-root-dir-face
     ((,sha1-class (:foreground ,sha1-green
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :background ,sha1-256-background))))

   `(neo-dir-link-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :background ,sha1-256-background))))

   `(neo-file-link-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(neo-button-face
     ((,sha1-class (:underline nil))
      (,sha1-256-class (:underline nil))))

   `(neo-expand-btn-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(neo-vc-default-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(neo-vc-user-face
     ((,sha1-class (:foreground ,sha1-red
                                   :slant italic))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :slant italic))))

   `(neo-vc-up-to-date-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(neo-vc-edited-face
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(neo-vc-needs-update-face
     ((,sha1-class (:underline t))
      (,sha1-256-class (:underline t))))

   `(neo-vc-needs-merge-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(neo-vc-unlocked-changes-face
     ((,sha1-class (:foreground ,sha1-red
                                   :background ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :background ,sha1-256-comments))))

   `(neo-vc-added-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(neo-vc-removed-face
     ((,sha1-class (:strike-through t))
      (,sha1-256-class (:strike-through t))))

   `(neo-vc-conflict-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(neo-vc-missing-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(neo-vc-ignored-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   ;; adoc-mode / markup
   `(markup-meta-face
     ((,sha1-class (:foreground ,sha1-gray-l))
      (,sha1-256-class (:foreground ,sha1-256-gray-l))))

   `(markup-table-face
     ((,sha1-class (:foreground ,sha1-blue-hc
                                   :background ,sha1-blue-lc))
      (,sha1-256-class (:foreground ,sha1-256-blue-hc
                                        :background ,sha1-256-blue-lc))))

   `(markup-verbatim-face
     ((,sha1-class (:background ,sha1-orange-lc))
      (,sha1-256-class (:background ,sha1-256-orange-lc))))

   `(markup-list-face
     ((,sha1-class (:foreground ,sha1-violet-hc
                                   :background ,sha1-violet-lc))
      (,sha1-256-class (:foreground ,sha1-256-violet-hc
                                        :background ,sha1-256-violet-lc))))

   `(markup-replacement-face
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(markup-complex-replacement-face
     ((,sha1-class (:foreground ,sha1-violet-hc
                                   :background ,sha1-violet-lc))
      (,sha1-256-class (:foreground ,sha1-256-violet-hc
                                        :background ,sha1-256-violet-lc))))

   `(markup-gen-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(markup-secondary-text-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   ;; org-mode
   `(org-agenda-structure
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :background ,sha1-highlight-line
                                   :weight bold
                                   :slant normal
                                   :inverse-video nil
                                   :height ,sha1-height-plus-1
                                   :underline nil
                                   :box (:line-width 2 :color ,sha1-background)))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :background ,sha1-256-highlight-line
                                        :weight bold
                                        :slant normal
                                        :inverse-video nil
                                        :height ,sha1-height-plus-1
                                        :underline nil
                                        :box (:line-width 2 :color ,sha1-256-background)))))

   `(org-agenda-calendar-event
     ((,sha1-class (:foreground ,sha1-emphasis))
      (,sha1-256-class (:foreground ,sha1-256-emphasis))))

   `(org-agenda-calendar-sexp
     ((,sha1-class (:foreground ,sha1-foreground
                                   :slant italic))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :slant italic))))

   `(org-agenda-date
     ((,sha1-class (:foreground ,sha1-comments
                                   :background ,sha1-background
                                   :weight normal
                                   :inverse-video nil
                                   :overline nil
                                   :slant normal
                                   :height 1.0
                                   :box (:line-width 2 :color ,sha1-background)))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :background ,sha1-256-background
                                        :weight normal
                                        :inverse-video nil
                                        :overline nil
                                        :slant normal
                                        :height 1.0
                                        :box (:line-width 2 :color ,sha1-256-background)))) t)

   `(org-agenda-date-weekend
     ((,sha1-class (:inherit org-agenda-date
                                :inverse-video nil
                                :background unspecified
                                :foreground ,sha1-comments
                                :weight unspecified
                                :underline t
                                :overline nil
                                :box unspecified))
      (,sha1-256-class (:inherit org-agenda-date
                                     :inverse-video nil
                                     :background unspecified
                                     :foreground ,sha1-256-comments
                                     :weight unspecified
                                     :underline t
                                     :overline nil
                                     :box unspecified))) t)

   `(org-agenda-date-today
     ((,sha1-class (:inherit org-agenda-date
                                :inverse-video t
                                :weight bold
                                :underline unspecified
                                :overline nil
                                :box unspecified
                                :foreground ,sha1-blue
                                :background ,sha1-background))
      (,sha1-256-class (:inherit org-agenda-date
                                     :inverse-video t
                                     :weight bold
                                     :underline unspecified
                                     :overline nil
                                     :box unspecified
                                     :foreground ,sha1-256-blue
                                     :background ,sha1-256-background))) t)

   `(org-agenda-done
     ((,sha1-class (:foreground ,sha1-comments
                                   :slant italic))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :slant italic))) t)

   `(org-archived
     ((,sha1-class (:foreground ,sha1-comments
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :weight normal))))

   `(org-block
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :background ,sha1-highlight-alt))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :background ,sha1-256-highlight-alt))))

   `(org-block-background
     ((,sha1-class (:background ,sha1-highlight-alt))
      (,sha1-256-class (:background ,sha1-256-highlight-alt))))

   `(org-block-begin-line
     ((,sha1-class (:foreground ,sha1-comments
                                   :background ,sha1-gray-d
                                   :slant italic))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :background ,sha1-256-gray-d
                                        :slant italic))))

   `(org-block-end-line
     ((,sha1-class (:foreground ,sha1-comments
                                   :background ,sha1-gray-d
                                   :slant italic))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :background ,sha1-256-gray-d
                                        :slant italic))))

   `(org-checkbox
     ((,sha1-class (:background ,sha1-background
                                   :foreground ,sha1-foreground
                                   :box (:line-width 1 :style released-button)))
      (,sha1-256-class (:background ,sha1-256-background
                                        :foreground ,sha1-256-foreground
                                        :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(org-date
     ((,sha1-class (:foreground ,sha1-blue
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :underline t))))

   `(org-done
     ((,sha1-class (:weight bold
                               :foreground ,sha1-green))
      (,sha1-256-class (:weight bold
                                    :foreground ,sha1-256-green))))

   `(org-ellipsis
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(org-formula
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(org-headline-done
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(org-hide
     ((,sha1-class (:foreground ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-background))))

   `(org-level-1
     ((,sha1-class (:inherit ,sha1-pitch
                                :height ,sha1-height-plus-4
                                :foreground ,sha1-orange))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :height ,sha1-height-plus-4
                                     :foreground ,sha1-256-orange))))

   `(org-level-2
     ((,sha1-class (:inherit ,sha1-pitch
                                :height ,sha1-height-plus-3
                                :foreground ,sha1-green))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :height ,sha1-height-plus-3
                                     :foreground ,sha1-256-green))))

   `(org-level-3
     ((,sha1-class (:inherit ,sha1-pitch
                                :height ,sha1-height-plus-2
                                :foreground ,sha1-blue))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :height ,sha1-height-plus-2
                                     :foreground ,sha1-256-blue))))

   `(org-level-4
     ((,sha1-class (:inherit ,sha1-pitch
                                :height ,sha1-height-plus-1
                                :foreground ,sha1-yellow))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :height ,sha1-height-plus-1
                                     :foreground ,sha1-256-yellow))))

   `(org-level-5
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-cyan))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :foreground ,sha1-256-cyan))))

   `(org-level-6
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-green))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :foreground ,sha1-256-green))))

   `(org-level-7
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-red))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :foreground ,sha1-256-red))))

   `(org-level-8
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-blue))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :foreground ,sha1-256-blue))))

   `(org-link
     ((,sha1-class (:foreground ,sha1-blue
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :underline t))))

   `(org-sexp-date
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(org-scheduled
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(org-scheduled-previously
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(org-scheduled-today
     ((,sha1-class (:foreground ,sha1-blue
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :weight normal))))

   `(org-special-keyword
     ((,sha1-class (:foreground ,sha1-comments
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :weight bold))))

   `(org-table
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(org-tag
     ((,sha1-class (:weight bold))
      (,sha1-256-class (:weight bold))))

   `(org-time-grid
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(org-todo
     ((,sha1-class (:foreground ,sha1-red
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :weight bold))))

   `(org-upcoming-deadline
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight normal
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(org-warning
     ((,sha1-class (:foreground ,sha1-orange
                                   :weight normal
                                   :underline nil))
      (,sha1-256-class (:foreground ,sha1-256-orange
                                        :weight normal
                                        :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,sha1-class (:background ,sha1-blue-lc
                                   :foreground ,sha1-blue-hc))
      (,sha1-256-class (:background ,sha1-256-blue-lc
                                        :foreground ,sha1-256-blue-hc))))

   `(org-habit-clear-future-face
     ((,sha1-class (:background ,sha1-blue-lc))
      (,sha1-256-class (:background ,sha1-256-blue-lc))))

   `(org-habit-ready-face
     ((,sha1-class (:background ,sha1-green-lc
                                   :foreground ,sha1-green))
      (,sha1-256-class (:background ,sha1-256-green-lc
                                        :foreground ,sha1-256-green))))

   `(org-habit-ready-future-face
     ((,sha1-class (:background ,sha1-green-lc))
      (,sha1-256-class (:background ,sha1-256-green-lc))))

   `(org-habit-alert-face
     ((,sha1-class (:background ,sha1-yellow
                                   :foreground ,sha1-yellow-lc))
      (,sha1-256-class (:background ,sha1-256-yellow
                                        :foreground ,sha1-256-yellow-lc))))

   `(org-habit-alert-future-face
     ((,sha1-class (:background ,sha1-yellow-lc))
      (,sha1-256-class (:background ,sha1-256-yellow-lc))))

   `(org-habit-overdue-face
     ((,sha1-class (:background ,sha1-red
                                   :foreground ,sha1-red-lc))
      (,sha1-256-class (:background ,sha1-256-red
                                        :foreground ,sha1-256-red-lc))))

   `(org-habit-overdue-future-face
     ((,sha1-class (:background ,sha1-red-lc))
      (,sha1-256-class (:background ,sha1-256-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(org-agenda-restriction-lock
     ((,sha1-class (:background ,sha1-yellow))
      (,sha1-256-class (:background ,sha1-256-yellow))))

   `(org-clock-overlay
     ((,sha1-class (:background ,sha1-yellow))
      (,sha1-256-class (:background ,sha1-256-yellow))))

   `(org-column
     ((,sha1-class (:background ,sha1-highlight-line
                                   :strike-through nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :inherit default))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :strike-through nil
                                        :underline nil
                                        :slant normal
                                        :weight normal
                                        :inherit default))))

   `(org-column-title
     ((,sha1-class (:background ,sha1-highlight-line
                                   :underline t
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :underline t
                                        :weight bold))))

   `(org-date-selected
     ((,sha1-class (:foreground ,sha1-red
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :inverse-video t))))

   `(org-document-info
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(org-document-title
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :weight bold
                                   :height ,sha1-height-plus-4))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :weight bold
                                        :height ,sha1-height-plus-4))))

   `(org-drawer
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(org-footnote
     ((,sha1-class (:foreground ,sha1-magenta
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-magenta
                                        :underline t))))

   `(org-latex-and-export-specials
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(org-mode-line-clock-overrun
     ((,sha1-class (:inherit mode-line))
      (,sha1-256-class (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,sha1-class (:inherit org-level-1))
      (,sha1-256-class (:inherit org-level-1))))

   `(outline-2
     ((,sha1-class (:inherit org-level-2))
      (,sha1-256-class (:inherit org-level-2))))

   `(outline-3
     ((,sha1-class (:inherit org-level-3))
      (,sha1-256-class (:inherit org-level-3))))

   `(outline-4
     ((,sha1-class (:inherit org-level-4))
      (,sha1-256-class (:inherit org-level-4))))

   `(outline-5
     ((,sha1-class (:inherit org-level-5))
      (,sha1-256-class (:inherit org-level-5))))

   `(outline-6
     ((,sha1-class (:inherit org-level-6))
      (,sha1-256-class (:inherit org-level-6))))

   `(outline-7
     ((,sha1-class (:inherit org-level-7))
      (,sha1-256-class (:inherit org-level-7))))

   `(outline-8
     ((,sha1-class (:inherit org-level-8))
      (,sha1-256-class (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,sha1-256-class (:foreground ,sha1-comments))))

   ;; perspective
   `(persp-selected-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight normal))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight normal))))

   ;; popup
   `(popup-face
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-foreground))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-foreground))))

   `(popup-isearch-match
     ((,sha1-class (:background ,sha1-green))
      (,sha1-256-class (:background ,sha1-256-green))))

   `(popup-menu-face
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-foreground))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-foreground))))

   `(popup-menu-mouse-face
     ((,sha1-class (:background ,sha1-blue
                                   :foreground ,sha1-foreground))
      (,sha1-256-class (:background ,sha1-256-blue
                                        :foreground ,sha1-256-foreground))))

   `(popup-menu-selection-face
     ((,sha1-class (:background ,sha1-magenta
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-magenta
                                        :foreground ,sha1-256-background))))

   `(popup-scroll-bar-background-face
     ((,sha1-class (:background ,sha1-comments))
      (,sha1-256-class (:background ,sha1-256-comments))))

   `(popup-scroll-bar-foreground-face
     ((,sha1-class (:background ,sha1-emphasis))
      (,sha1-256-class (:background ,sha1-256-emphasis))))

   `(popup-tip-face
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-foreground))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-foreground))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(rainbow-delimiters-depth-4-face
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(rainbow-delimiters-depth-5-face
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(rainbow-delimiters-depth-6-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(rainbow-delimiters-depth-7-face
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(rainbow-delimiters-depth-10-face
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(rainbow-delimiters-depth-12-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(rainbow-delimiters-unmatched-face
     ((,sha1-class (:foreground ,sha1-foreground
                                   :background ,sha1-background
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :background ,sha1-256-background
                                        :inverse-video t))))

   ;; realgud
   `(realgud-overlay-arrow1
     ((,sha1-class (:foreground ,sha1-green-d))
      (,sha1-256-class (:foreground ,sha1-256-green-d))))

   `(realgud-overlay-arrow2
     ((,sha1-class (:foreground ,sha1-yellow-d))
      (,sha1-256-class (:foreground ,sha1-256-yellow-d))))

   `(realgud-overlay-arrow3
     ((,sha1-class (:foreground ,sha1-orange-d))
      (,sha1-256-class (:foreground ,sha1-256-orange-d))))

   `(realgud-bp-enabled-face
     ((,sha1-class (:inherit error))
      (,sha1-256-class (:inherit error))))

   `(realgud-bp-disabled-face
     ((,sha1-class (:inherit secondary-selection))
      (,sha1-256-class (:inherit secondary-selection))))

   `(realgud-bp-line-enabled-face
     ((,sha1-class (:foreground ,sha1-red-d))
      (,sha1-256-class (:foreground ,sha1-256-red-d))))

   `(realgud-bp-line-disabled-face
     ((,sha1-class (:inherit secondary-selection))
      (,sha1-256-class (:inherit secondary-selection))))

   `(realgud-line-number
     ((,sha1-class (:inerhit sha1-line-number))
      (,sha1-256-class (:inerhit sha1-line-number))))

   `(realgud-backtrace-number
     ((,sha1-class (:foreground ,sha1-yellow-d
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                       :weight bold))))

   ;; rhtm-mode
   `(erb-face
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :background ,sha1-256-background))))

   `(erb-delim-face
     ((,sha1-class (:foreground ,sha1-cyan
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :background ,sha1-256-background))))

   `(erb-exec-face
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :background ,sha1-256-background))))

   `(erb-exec-delim-face
     ((,sha1-class (:foreground ,sha1-cyan
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :background ,sha1-256-background))))

   `(erb-out-face
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :background ,sha1-256-background))))

   `(erb-out-delim-face
     ((,sha1-class (:foreground ,sha1-cyan
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :background ,sha1-256-background))))

   `(erb-comment-face
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :background ,sha1-256-background))))

   `(erb-comment-delim-face
     ((,sha1-class (:foreground ,sha1-cyan
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :background ,sha1-256-background))))

   ;; rst-mode
   `(rst-level-1-face
     ((,sha1-class (:background ,sha1-yellow
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-yellow
                                        :foreground ,sha1-256-background))))

   `(rst-level-2-face
     ((,sha1-class (:background ,sha1-cyan
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-cyan
                                        :foreground ,sha1-256-background))))

   `(rst-level-3-face
     ((,sha1-class (:background ,sha1-blue
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-blue
                                        :foreground ,sha1-256-background))))

   `(rst-level-4-face
     ((,sha1-class (:background ,sha1-violet
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-violet
                                        :foreground ,sha1-256-background))))

   `(rst-level-5-face
     ((,sha1-class (:background ,sha1-magenta
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-magenta
                                        :foreground ,sha1-256-background))))

   `(rst-level-6-face
     ((,sha1-class (:background ,sha1-red
                                   :foreground ,sha1-background))
      (,sha1-256-class (:background ,sha1-256-red
                                        :foreground ,sha1-256-background))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(rpm-spec-doc-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(rpm-spec-ghost-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(rpm-spec-macro-face
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(rpm-spec-package-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(rpm-spec-section-face
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(rpm-spec-tag-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(rpm-spec-var-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,sha1-class (:foreground ,sha1-violet
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-violet
                                        :weight bold))))

   `(sh-escaped-newline
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight bold))))

   `(sh-heredoc
     ((,sha1-class (:foreground ,sha1-yellow
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   `(sp-wrap-overlay-face
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   `(sp-show-pair-enclosing
     ((,sha1-class (:inherit highlight))
      (,sha1-256-class (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,sha1-class (:foreground ,sha1-green
                                   :background ,sha1-background
                                   :weight normal
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :background ,sha1-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,sha1-class (:foreground ,sha1-red
                                   :background ,sha1-background
                                   :weight normal
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :background ,sha1-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,sha1-class (:foreground ,sha1-blue
                                   :background ,sha1-background
                                   :weight normal
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :background ,sha1-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(show-paren-mismatch
     ((,sha1-class (:foreground ,sha1-red
                                   :background ,sha1-background
                                   :weight normal
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :background ,sha1-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,sha1-class (:foreground ,sha1-blue
                                   :background ,sha1-background
                                   :weight normal
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :background ,sha1-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-mismatch
     ((,sha1-class (:foreground ,sha1-red
                                   :background ,sha1-background
                                   :weight normal
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :background ,sha1-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-no-match
     ((,sha1-class (:foreground ,sha1-red
                                   :background ,sha1-background
                                   :weight normal
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :background ,sha1-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   ;; smerge
   `(smerge-base
      ((,sha1-class (:background ,sha1-diff-blue-base))
        (,sha1-256-class (:background ,sha1-256-diff-blue-base))))
   `(smerge-upper
      ((,sha1-class (:background ,sha1-diff-red-base))
        (,sha1-256-class (:background ,sha1-256-diff-red-base))))
   `(smerge-lower
      ((,sha1-class (:background ,sha1-diff-green-base))
        (,sha1-256-class (:background ,sha1-256-diff-green-base))))
   ;; WARNING: defining this face will overwrite the next two when displaying a
   ;; smerge diff in a file.
   ;; `(smerge-refined-changed
   ;;    ((,sha1-class (:background ,sha1-diff-blue-emphasis))
   ;;      (,sha1-256-class (:background ,sha1-256-diff-blue-emphasis))))
   `(smerge-refined-added
      ((,sha1-class (:background ,sha1-diff-green-emphasis))
        (,sha1-256-class (:background ,sha1-256-diff-green-emphasis))))
   `(smerge-refined-removed
      ((,sha1-class (:background ,sha1-diff-red-emphasis))
        (,sha1-256-class (:background ,sha1-256-diff-red-emphasis))))

   ;; speedbar
   `(speedbar-button-face
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-comments))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :foreground ,sha1-256-comments))))

   `(speedbar-directory-face
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-blue))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :foreground ,sha1-256-blue))))

   `(speedbar-file-face
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-foreground))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :foreground ,sha1-256-foreground))))

   `(speedbar-highlight-face
     ((,sha1-class (:inherit ,sha1-pitch
                                :background ,sha1-highlight-line))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :background ,sha1-256-highlight-line))))

   `(speedbar-selected-face
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-yellow
                                :underline t))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :foreground ,sha1-256-yellow
                                     :underline t))))

   `(speedbar-separator-face
     ((,sha1-class (:inherit ,sha1-pitch
                                :background ,sha1-blue
                                :foreground ,sha1-background
                                :overline ,sha1-cyan-lc))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :background ,sha1-256-blue
                                     :foreground ,sha1-256-background
                                     :overline ,sha1-256-cyan-lc))))

   `(speedbar-tag-face
     ((,sha1-class (:inherit ,sha1-pitch
                                :foreground ,sha1-green))
      (,sha1-256-class (:inherit ,sha1-pitch
                                     :foreground ,sha1-256-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,sha1-class (:background ,sha1-blue
                                   :foreground ,sha1-background
                                   :height ,sha1-height-plus-1
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-blue
                                        :foreground ,sha1-256-background
                                        :height ,sha1-height-plus-1
                                        :weight bold))))

   `(sr-editing-path-face
     ((,sha1-class (:background ,sha1-yellow
                                   :foreground ,sha1-background
                                   :weight bold
                                   :height ,sha1-height-plus-1))
      (,sha1-256-class (:background ,sha1-256-yellow
                                        :foreground ,sha1-256-background
                                        :weight bold
                                        :height ,sha1-height-plus-1))))

   `(sr-highlight-path-face
     ((,sha1-class (:background ,sha1-green
                                   :foreground ,sha1-background
                                   :weight bold
                                   :height ,sha1-height-plus-1))
      (,sha1-256-class (:background ,sha1-256-green
                                        :foreground ,sha1-256-background
                                        :weight bold
                                        :height ,sha1-height-plus-1))))

   `(sr-passive-path-face
     ((,sha1-class (:background ,sha1-comments
                                   :foreground ,sha1-background
                                   :weight bold
                                   :height ,sha1-height-plus-1))
      (,sha1-256-class (:background ,sha1-256-comments
                                        :foreground ,sha1-256-background
                                        :weight bold
                                        :height ,sha1-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,sha1-class (:inherit disha1-red-marked))
      (,sha1-256-class (:inherit disha1-red-marked))))

   `(sr-marked-file-face
     ((,sha1-class (:inherit disha1-red-marked))
      (,sha1-256-class (:inherit disha1-red-marked))))

   `(sr-alt-marked-dir-face
     ((,sha1-class (:background ,sha1-magenta
                                   :foreground ,sha1-background
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-magenta
                                        :foreground ,sha1-256-background
                                        :weight bold))))

   `(sr-alt-marked-file-face
     ((,sha1-class (:background ,sha1-magenta
                                   :foreground ,sha1-background
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-magenta
                                        :foreground ,sha1-256-background
                                        :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,sha1-class (:inherit disha1-red-directory
                                :weight normal))
      (,sha1-256-class (:inherit disha1-red-directory
                                     :weight normal))))

   `(sr-symlink-directory-face
     ((,sha1-class (:inherit disha1-red-directory
                                :slant italic
                                :weight normal))
      (,sha1-256-class (:inherit disha1-red-directory
                                     :slant italic
                                     :weight normal))))

   `(sr-symlink-face
     ((,sha1-class (:inherit disha1-red-symlink
                                :slant italic
                                :weight normal))
      (,sha1-256-class (:inherit disha1-red-symlink
                                     :slant italic
                                     :weight normal))))

   `(sr-broken-link-face
     ((,sha1-class (:inherit disha1-red-warning
                                :slant italic
                                :weight normal))
      (,sha1-256-class (:inherit disha1-red-warning
                                     :slant italic
                                     :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(sr-encrypted-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(sr-log-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(sr-packaged-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(sr-html-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(sr-xml-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,sha1-class (:background ,sha1-red
                                   :foreground ,sha1-background
                                   :weight bold))
      (,sha1-256-class (:background ,sha1-256-red
                                        :foreground ,sha1-256-background
                                        :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-yellow))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-yellow))))

   `(syslog-hour-face
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-green))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-green))))

   `(syslog-error-face
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-red
                                   :weight bold))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-red
                                        :weight bold))))

   `(syslog-warn-face
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-orange
                                   :weight bold))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-orange
                                        :weight bold))))

   `(syslog-info-face
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-blue
                                   :weight bold))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-blue
                                        :weight bold))))

   `(syslog-debug-face
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-cyan
                                   :weight bold))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-cyan
                                        :weight bold))))

   `(syslog-su-face
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-magenta))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-magenta))))

   ;; table
   `(table-cell
     ((,sha1-class (:foreground ,sha1-foreground
                                   :background ,sha1-highlight-line))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :background ,sha1-256-highlight-line))))

   ;; term
   `(term-color-black
     ((,sha1-class (:foreground ,sha1-background
                                   :background ,sha1-highlight-line))
      (,sha1-256-class (:foreground ,sha1-256-background
                                        :background ,sha1-256-highlight-line))))

   `(term-color-red
     ((,sha1-class (:foreground ,sha1-red
                                   :background ,sha1-red-d))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :background ,sha1-256-red-d))))

   `(term-color-green
     ((,sha1-class (:foreground ,sha1-green
                                   :background ,sha1-green-d))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :background ,sha1-256-green-d))))

   `(term-color-yellow
     ((,sha1-class (:foreground ,sha1-yellow
                                   :background ,sha1-yellow-d))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :background ,sha1-256-yellow-d))))

   `(term-color-blue
     ((,sha1-class (:foreground ,sha1-blue
                                   :background ,sha1-blue-d))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :background ,sha1-256-blue-d))))

   `(term-color-magenta
     ((,sha1-class (:foreground ,sha1-magenta
                                   :background ,sha1-magenta-d))
      (,sha1-256-class (:foreground ,sha1-256-magenta
                                        :background ,sha1-256-magenta-d))))

   `(term-color-cyan
     ((,sha1-class (:foreground ,sha1-cyan
                                   :background ,sha1-cyan-d))
      (,sha1-256-class (:foreground ,sha1-256-cyan
                                        :background ,sha1-256-cyan-d))))

   `(term-color-white
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :background ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :background ,sha1-256-foreground))))

   `(term-default-fg-color
     ((,sha1-class (:inherit term-color-white))
      (,sha1-256-class (:inherit term-color-white))))

   `(term-default-bg-color
     ((,sha1-class (:inherit term-color-black))
      (,sha1-256-class (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,sha1-class (:background ,sha1-yellow-hc
                                   :foreground ,sha1-background
                                   :inherit ,sha1-pitch))))

   ;; treemacs
   `(treemacs-directory-face
      ((,sha1-class (:foreground ,sha1-violet
                         :background ,sha1-background
                         :weight bold))
        (,sha1-256-class (:foreground ,sha1-256-violet
                              :background ,sha1-256-background
                              :weight bold))))

   `(treemacs-header-face
      ((,sha1-class (:foreground ,sha1-yellow
                         :background ,sha1-background
                         :underline t
                         :weight bold))
        (,sha1-256-class (:foreground ,sha1-256-yellow
                              :background ,sha1-256-background
                              :underline t
                              :weight bold))))

   `(treemacs-git-modified-face
      ((,sha1-class (:foreground ,sha1-green
                         :background ,sha1-background))
        (,sha1-256-class (:foreground ,sha1-256-green
                              :background ,sha1-256-background))))

   `(treemacs-git-renamed-face
      ((,sha1-class (:foreground ,sha1-red
                         :background ,sha1-background))
        (,sha1-256-class (:foreground ,sha1-256-red
                              :background ,sha1-256-background))))

   `(treemacs-git-ignored-face
      ((,sha1-class (:foreground ,sha1-gray-l
                         :background ,sha1-background))
        (,sha1-256-class (:foreground ,sha1-256-gray-l
                              :background ,sha1-256-background))))

   `(treemacs-git-untracked-face
      ((,sha1-class (:foreground ,sha1-red
                         :background ,sha1-background))
        (,sha1-256-class (:foreground ,sha1-256-red
                              :background ,sha1-256-background))))

   `(treemacs-git-added-face
      ((,sha1-class (:foreground ,sha1-green
                         :background ,sha1-background))
        (,sha1-256-class (:foreground ,sha1-256-green
                              :background ,sha1-256-background))))

   `(treemacs-git-conflict-face
      ((,sha1-class (:foreground ,sha1-orange
                         :background ,sha1-background))
        (,sha1-256-class (:foreground ,sha1-256-orange
                              :background ,sha1-256-background))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,sha1-class (:foreground ,sha1-magenta
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-magenta
                                        :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :background ,sha1-highlight-line
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :background ,sha1-256-highlight-line
                                        :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,sha1-class (:foreground ,sha1-emphasis))
      (,sha1-256-class (:foreground ,sha1-256-emphasis))))

   `(tuareg-font-lock-error-face
     ((,sha1-class (:foreground ,sha1-yellow
                                   :background ,sha1-red
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :background ,sha1-256-red
                                        :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,sha1-class (:foreground ,sha1-cyan))
      (,sha1-256-class (:foreground ,sha1-256-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,sha1-class (:foreground ,sha1-comments
                                   :background ,sha1-background))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :background ,sha1-256-background))))

   `(undo-tree-visualizer-unmodified-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(undo-tree-visualizer-current-face
     ((,sha1-class (:foreground ,sha1-blue
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-blue
                                        :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :background ,sha1-background
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :background ,sha1-256-background
                                        :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   ;; volatile highlights
   `(vhl/default-face
      ((,sha1-class (:background ,sha1-highlight-alt))
        (,sha1-256-class (:background ,sha1-256-highlight-alt))))

   ;; w3m
   `(w3m-anchor
     ((,sha1-class (:inherit link))
      (,sha1-256-class (:inherit link))))

   `(w3m-arrived-anchor
     ((,sha1-class (:inherit link-visited))
      (,sha1-256-class (:inherit link-visited))))

   `(w3m-form
     ((,sha1-class (:background ,sha1-background
                                   :foreground ,sha1-foreground))
      (,sha1-256-class (:background ,sha1-256-background
                                        :foreground ,sha1-256-foreground))))

   `(w3m-header-line-location-title
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-yellow))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-yellow))))

   `(w3m-header-line-location-content

     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-foreground))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-foreground))))

   `(w3m-bold
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :weight bold))))

   `(w3m-image-anchor
     ((,sha1-class (:background ,sha1-background
                                   :foreground ,sha1-cyan
                                   :inherit link))
      (,sha1-256-class (:background ,sha1-256-background
                                        :foreground ,sha1-256-cyan
                                        :inherit link))))

   `(w3m-image
     ((,sha1-class (:background ,sha1-background
                                   :foreground ,sha1-cyan))
      (,sha1-256-class (:background ,sha1-256-background
                                        :foreground ,sha1-256-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,sha1-class (:foreground ,sha1-emphasis))
      (,sha1-256-class (:foreground ,sha1-256-emphasis))))

   `(w3m-lnum-match
     ((,sha1-class (:background ,sha1-highlight-line))
      (,sha1-256-class (:background ,sha1-256-highlight-line))))

   `(w3m-lnum
     ((,sha1-class (:underline nil
                                  :bold nil
                                  :foreground ,sha1-red))
      (,sha1-256-class (:underline nil
                                       :bold nil
                                       :foreground ,sha1-256-red))))

   `(w3m-session-select
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(w3m-session-selected
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :bold t
                                   :underline t))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :bold t
                                        :underline t))))

   `(w3m-tab-background
     ((,sha1-class (:background ,sha1-background
                                   :foreground ,sha1-foreground))
      (,sha1-256-class (:background ,sha1-256-background
                                        :foreground ,sha1-256-foreground))))

   `(w3m-tab-selected-background
     ((,sha1-class (:background ,sha1-background
                                   :foreground ,sha1-foreground))
      (,sha1-256-class (:background ,sha1-256-background
                                        :foreground ,sha1-256-foreground))))

   `(w3m-tab-mouse
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-yellow))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-yellow))))

   `(w3m-tab-selected
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-emphasis
                                   :bold t))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-emphasis
                                        :bold t))))

   `(w3m-tab-unselected
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-foreground))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-foreground))))

   `(w3m-tab-selected-retrieving
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-red))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-red))))

   `(w3m-tab-unselected-retrieving
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-orange))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-orange))))

   `(w3m-tab-unselected-unseen
     ((,sha1-class (:background ,sha1-highlight-line
                                   :foreground ,sha1-violet))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :foreground ,sha1-256-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(web-mode-comment-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(web-mode-constant-face
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(web-mode-current-element-highlight-face
     ((,sha1-class (:underline unspecified
                                  :weight unspecified
                                  :background ,sha1-highlight-line))
      (,sha1-256-class (:underline unspecified
                                       :weight unspecified
                                       :background ,sha1-256-highlight-line))))

   `(web-mode-doctype-face
     ((,sha1-class (:foreground ,sha1-comments
                                   :slant italic
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :slant italic
                                        :weight bold))))

   `(web-mode-folded-face
     ((,sha1-class (:underline t))
      (,sha1-256-class (:underline t))))

   `(web-mode-function-name-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(web-mode-html-attr-name-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(web-mode-html-attr-custom-face
     ((,sha1-class (:inherit web-mode-html-attr-name-face))
      (,sha1-256-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-engine-face
     ((,sha1-class (:inherit web-mode-block-delimiter-face))
      (,sha1-256-class (:inherit web-mode-block-delimiter-face))))

   `(web-mode-html-attr-equal-face
     ((,sha1-class (:inherit web-mode-html-attr-name-face))
      (,sha1-256-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-value-face
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(web-mode-html-tag-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(web-mode-html-tag-bracket-face
     ((,sha1-class (:foreground ,sha1-gray))
      (,sha1-256-class (:foreground ,sha1-256-gray))))

   `(web-mode-keyword-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(web-mode-preprocessor-face
     ((,sha1-class (:foreground ,sha1-yellow
                                   :slant normal
                                   :weight unspecified))
      (,sha1-256-class (:foreground ,sha1-256-yellow
                                        :slant normal
                                        :weight unspecified))))

   `(web-mode-string-face
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(web-mode-type-face
     ((,sha1-class (:inherit font-lock-type-face))
      (,sha1-256-class (:inherit font-lock-type-face))))

   `(web-mode-variable-name-face
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(web-mode-warning-face
     ((,sha1-class (:inherit font-lock-warning-face))
      (,sha1-256-class (:inherit font-lock-warning-face))))

   `(web-mode-block-face
     ((,sha1-class (:background unspecified))
      (,sha1-256-class (:background unspecified))))

   `(web-mode-block-delimiter-face
     ((,sha1-class (:inherit font-lock-preprocessor-face))
      (,sha1-256-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-comment-face
     ((,sha1-class (:inherit web-mode-comment-face))
      (,sha1-256-class (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,sha1-class (:inherit font-lock-preprocessor-face))
      (,sha1-256-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-string-face
     ((,sha1-class (:inherit web-mode-string-face))
      (,sha1-256-class (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,sha1-class (:box 1 :weight bold))
      (,sha1-256-class (:box 1 :weight bold))))

   `(web-mode-css-at-rule-face
     ((,sha1-class (:inherit font-lock-constant-face))
      (,sha1-256-class (:inherit font-lock-constant-face))))

   `(web-mode-css-pseudo-class-face
     ((,sha1-class (:inherit font-lock-builtin-face))
      (,sha1-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-color-face
     ((,sha1-class (:inherit font-lock-builtin-face))
      (,sha1-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-filter-face
     ((,sha1-class (:inherit font-lock-function-name-face))
      (,sha1-256-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-function-face
     ((,sha1-class (:inherit font-lock-builtin-face))
      (,sha1-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-call-face
     ((,sha1-class (:inherit font-lock-function-name-face))
      (,sha1-256-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-priority-face
     ((,sha1-class (:inherit font-lock-builtin-face))
      (,sha1-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,sha1-class (:inherit font-lock-variable-name-face))
      (,sha1-256-class (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,sha1-class (:inherit font-lock-keyword-face))
      (,sha1-256-class (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,sha1-class (:inherit web-mode-string-face))
      (,sha1-256-class (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,sha1-class (:inherit web-mode-string-face))
      (,sha1-256-class (:inherit web-mode-string-face))))

   `(web-mode-json-comment-face
     ((,sha1-class (:inherit web-mode-comment-face))
      (,sha1-256-class (:inherit web-mode-comment-face))))

   `(web-mode-json-context-face
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(web-mode-json-key-face
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(web-mode-json-string-face
     ((,sha1-class (:inherit web-mode-string-face))
      (,sha1-256-class (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(web-mode-part-comment-face
     ((,sha1-class (:inherit web-mode-comment-face))
      (,sha1-256-class (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,sha1-class (:inherit web-mode-block-face))
      (,sha1-256-class (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,sha1-class (:inherit web-mode-string-face))
      (,sha1-256-class (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,sha1-class (:foreground ,sha1-violet))
      (,sha1-256-class (:foreground ,sha1-256-violet))))

   `(web-mode-whitespace-face
     ((,sha1-class (:background ,sha1-red))
      (,sha1-256-class (:background ,sha1-256-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-comments
                                   :inverse-video unspecified
                                   :slant italic))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-comments
                                        :inverse-video unspecified
                                        :slant italic))))

   `(whitespace-hspace
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-emphasis
                                   :inverse-video unspecified))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-emphasis
                                        :inverse-video unspecified))))

   `(whitespace-tab
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-red
                                   :inverse-video unspecified
                                   :weight bold))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-red
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-newline
     ((,sha1-class(:background unspecified
                                  :foreground ,sha1-comments
                                  :inverse-video unspecified))
      (,sha1-256-class (:background unspecified
                                       :foreground ,sha1-256-comments
                                       :inverse-video unspecified))))

   `(whitespace-trailing
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-orange-lc
                                   :inverse-video t))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-orange-lc
                                        :inverse-video t))))

   `(whitespace-line
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-magenta
                                   :inverse-video unspecified))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-magenta
                                        :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,sha1-class (:background ,sha1-red-lc
                                   :foreground unspecified
                                   :inverse-video unspecified))
      (,sha1-256-class (:background ,sha1-256-red-lc
                                        :foreground unspecified
                                        :inverse-video unspecified))))

   `(whitespace-indentation
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-yellow
                                   :inverse-video unspecified
                                   :weight bold))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-yellow
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-empty
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-red-lc
                                   :inverse-video t))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-red-lc
                                        :inverse-video t))))

   `(whitespace-space-after-tab
     ((,sha1-class (:background unspecified
                                   :foreground ,sha1-orange
                                   :inverse-video t
                                   :weight bold))
      (,sha1-256-class (:background unspecified
                                        :foreground ,sha1-256-orange
                                        :inverse-video t
                                        :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(wl-highlight-folder-many-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(wl-highlight-folder-path-face
     ((,sha1-class (:foreground ,sha1-orange))
      (,sha1-256-class (:foreground ,sha1-256-orange))))

   `(wl-highlight-folder-unread-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(wl-highlight-folder-zero-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(wl-highlight-folder-unknown-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(wl-highlight-message-citation-header
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(wl-highlight-message-cited-text-1
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(wl-highlight-message-cited-text-2
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(wl-highlight-message-cited-text-3
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(wl-highlight-message-cited-text-4
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(wl-highlight-message-header-contents-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(wl-highlight-message-headers-face
     ((,sha1-class (:foreground ,sha1-red))
      (,sha1-256-class (:foreground ,sha1-256-red))))

   `(wl-highlight-message-important-header-contents
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(wl-highlight-message-header-contents
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(wl-highlight-message-important-header-contents2
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(wl-highlight-message-signature
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(wl-highlight-summary-answesha1-red-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(wl-highlight-summary-disposed-face
     ((,sha1-class (:foreground ,sha1-foreground
                                   :slant italic))
      (,sha1-256-class (:foreground ,sha1-256-foreground
                                        :slant italic))))

   `(wl-highlight-summary-new-face
     ((,sha1-class (:foreground ,sha1-blue))
      (,sha1-256-class (:foreground ,sha1-256-blue))))

   `(wl-highlight-summary-normal-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(wl-highlight-summary-thread-top-face
     ((,sha1-class (:foreground ,sha1-yellow))
      (,sha1-256-class (:foreground ,sha1-256-yellow))))

   `(wl-highlight-thread-indent-face
     ((,sha1-class (:foreground ,sha1-magenta))
      (,sha1-256-class (:foreground ,sha1-256-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(wl-highlight-summary-displaying-face
     ((,sha1-class (:underline t
                                  :weight bold))
      (,sha1-256-class (:underline t
                                       :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,sha1-class (:inherit error))
      (,sha1-256-class (:inherit error))))

   `(weechat-highlight-face
     ((,sha1-class (:foreground ,sha1-emphasis
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-emphasis
                                        :weight bold))))

   `(weechat-nick-self-face
     ((,sha1-class (:foreground ,sha1-green
                                   :weight unspecified
                                   :inverse-video t))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :weight unspecified
                                        :inverse-video t))))

   `(weechat-prompt-face
     ((,sha1-class (:inherit minibuffer-prompt))
      (,sha1-256-class (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   ;; which-func-mode
   `(which-func
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   ;; which-key
   `(which-key-key-face
     ((,sha1-class (:foreground ,sha1-green
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-green
                                        :weight bold))))

   `(which-key-separator-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(which-key-note-face
     ((,sha1-class (:foreground ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments))))

   `(which-key-command-description-face
     ((,sha1-class (:foreground ,sha1-foreground))
      (,sha1-256-class (:foreground ,sha1-256-foreground))))

   `(which-key-local-map-description-face
     ((,sha1-class (:foreground ,sha1-yellow-hc))
      (,sha1-256-class (:foreground ,sha1-256-yellow-hc))))

   `(which-key-group-description-face
     ((,sha1-class (:foreground ,sha1-red
                                   :weight bold))
      (,sha1-256-class (:foreground ,sha1-256-red
                                        :weight bold))))

   ;; window-divider-mode
   `(window-divider
     ((,sha1-class (:foreground ,sha1-highlight))
      (,sha1-256-class (:foreground ,sha1-highlight))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel ((t (:inherit window-divider))))

   ;; window-number-mode
   `(window-number-face
     ((,sha1-class (:foreground ,sha1-green))
      (,sha1-256-class (:foreground ,sha1-256-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,sha1-class (:foreground ,sha1-comments
                                   :background ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :background ,sha1-256-comments))))

   `(yascroll:thumb-fringe
     ((,sha1-class (:foreground ,sha1-comments
                                   :background ,sha1-comments))
      (,sha1-256-class (:foreground ,sha1-256-comments
                                        :background ,sha1-256-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,sha1-class (:background ,sha1-highlight-line
                                   :box ,sha1-emphasis))
      (,sha1-256-class (:background ,sha1-256-highlight-line
                                        :box ,sha1-256-emphasis)))))

  (custom-theme-set-variables
   'sha1
   `(ansi-color-names-vector [,sha1-background ,sha1-red ,sha1-green ,sha1-yellow
                                                  ,sha1-blue ,sha1-magenta ,sha1-cyan ,sha1-foreground])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,sha1-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,sha1-magenta ,sha1-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,sha1-highlight-line . 0)
       (,sha1-green-lc . 20)
       (,sha1-cyan-lc . 30)
       (,sha1-blue-lc . 50)
       (,sha1-yellow-lc . 60)
       (,sha1-orange-lc . 70)
       (,sha1-magenta-lc . 85)
       (,sha1-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,sha1-background)
   `(pos-tip-background-color ,sha1-yellow-hc)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,sha1-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,sha1-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,sha1-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,sha1-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,sha1-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     '(unspecified ,sha1-background ,sha1-highlight-line
                  ,sha1-red-d ,sha1-red
                  ,sha1-green-d ,sha1-green
                  ,sha1-yellow-d ,sha1-yellow
                  ,sha1-blue-d ,sha1-blue
                  ,sha1-magenta-d ,sha1-magenta
                  ,sha1-cyan-d ,sha1-cyan
                  ,sha1-foreground ,sha1-emphasis))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'sha1)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; sha1-theme.el ends here
