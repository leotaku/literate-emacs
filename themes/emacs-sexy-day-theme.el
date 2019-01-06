;;; emacs-sexy-day-theme.el --- inspired by the emacs.sexy website
(require 'doom-themes)

;;
(defgroup emacs-sexy-day-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom emacs-sexy-day-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'emacs-sexy-day-theme
  :type 'boolean)

(defcustom emacs-sexy-day-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'emacs-sexy-day-theme
  :type 'boolean)

(defcustom emacs-sexy-day-comment-bg emacs-sexy-day-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'emacs-sexy-day-theme
  :type 'boolean)

(defcustom emacs-sexy-day-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'emacs-sexy-day-theme
  :type '(or integer boolean))

;;
(def-doom-theme emacs-sexy-day
  "A light theme inspired by Atom One"

  ;; name        default   256       16
  ((bg         '("#fdfdfd" nil       nil))
   (bg-alt     '("#f6f6f6" nil       nil))
   (base0      '("#f6f6f6" "#f6f6f6" "white"))
   (base1      '("#f1f1f1" "#f1f1f1" "brightblack"))
   (base2      '("#e0e0e0" "#e0e0e0" "brightblack"))
   (base3      '("#d3d5d4" "#d3d5d4" "brightblack"))
   (base4      '("#9ca0a4" "#9ca0a4" "brightblack"))
   (base5      '("#383a42" "#424242" "brightblack"))
   (base6      '("#202328" "#2e2e2e" "brightblack"))
   (base7      '("#1c1f24" "#1e1e1e" "brightblack"))
   (base8      '("#1b2229" "black"   "black"))
   (fg         '("#383a42" "#424242" "black"))
   (fg-alt     '("#c6c7c7" "#c7c7c7" "brightblack"))

   (grey       base4)
   (red        '("#B61F1F" "#B61F1F" "red"))
   (orange     '("#FFB81E" "#FFB81E" "brightred"))
   (green      '("#18874C" "#18874C" "green"))
   (teal       '("#D7F8F7" "#D7F8F7" "brightgreen"))
   (yellow     '("#FFD11E" "#FFD11E" "yellow"))
   (blue       '("#000080" "#000080" "brightblue"))
   (dark-blue  '("#0E1D34" "#0E1D34" "blue"))
   (magenta    '("#3A377D" "#3A377D" "magenta"))
   (violet     '("#5E5BA8" "#5E5BA8" "brightmagenta"))
   (cyan       '("#1CBBB4" "#1CBBB4" "brightcyan"))
   (dark-cyan  '("#172D4F" "#172D4F" "cyan"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base2 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if emacs-sexy-day-brighter-comments cyan base4))
   (doc-comments   (doom-darken comments 0.15))
   (constants      violet)
   (functions      magenta)
   (keywords       red)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-darken magenta 0.36))
   (numbers        orange)
   (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.3)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright emacs-sexy-day-brighter-modeline)
   (-modeline-pad
    (when emacs-sexy-day-padded-modeline
      (if (integerp emacs-sexy-day-padded-modeline) emacs-sexy-day-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt (doom-blend violet base4 (if -modeline-bright 0.5 0.2)))

   (modeline-bg
    (if -modeline-bright
        (doom-darken base2 0.05)
      base1))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken base2 0.1)
      base2))
   (modeline-bg-inactive (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1))))

  ;; --- extra faces ------------------------
  ((font-lock-comment-face
    :foreground comments
    :background (if emacs-sexy-day-comment-bg base0))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   
   ((line-number &override) :foreground (doom-lighten base4 0.15))
   ((line-number-current-line &override) :foreground base8)

   (solaire-hl-line-face :inherit 'hl-line :background base0)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; magit
   (magit-blame-heading     :foreground orange :background bg-alt)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :bold bold)

   ;; smartparens
   (sp-pair-overlay-face  :background base1)
   (shadow :background base1 :foreground "#505050")

   ;; misc

   (secondary-selection    :background bg)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face     :foreground base5)
   (markdown-header-face     :inherit 'bold :foreground red)
   (markdown-code-face       :background base1)
   (mmm-default-submode-face :background base1)

   ;; org-mode
   (org-level-1          :foreground red    :weight 'bold :height 1.3)
   (org-level-2          :foreground orange :weight 'bold :height 1.2)
   (org-level-3          :foreground green  :weight 'bold :height 1.1)
   (org-ellipsis         :foreground red  :background bg  :underline nil)
   (org-quote            :background base1)
   (org-todo             :foreground green   :weight 'bold :box '(:line-width 1 :color green))
   (org-done             :foreground orange  :weight 'bold :box '(:line-width 1 :color orange))
   (org-maybe            :foreground magenta :weight 'bold :box '(:line-width 1 :color magenta))
   (org-canceled         :foreground red     :weight 'bold :box '(:line-width 1 :color red))

   (org-document-title   :foreground magenta :weight 'bold :height 1.5)
   (org-document-info    :foreground magenta :weight 'bold)
   
   (org-block            :inherit 'shadow)
   (org-block-begin-line :inherit 'shadow)
   (org-block-end-line   :inherit 'shadow)
   (org-meta-line        :inherit 'shadow)

   ;; helm
   (helm-candidate-number :background blue :foreground bg)

   ;; web-mode
   (web-mode-current-element-highlight-face :background dark-blue :foreground bg)

   ;; wgrep
   (wgrep-face :background base1)

   ;; ediff
   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))

   ;; tooltip
   (tooltip :background base1 :foreground fg)

   ;; posframe
   (ivy-posframe               :background base0)

   ;; lsp
   (lsp-ui-doc-background      :background base0)
   (lsp-face-highlight-read    :background (doom-blend red bg 0.3))
   (lsp-face-highlight-textual :inherit 'lsp-face-highlight-read)
   (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read))
   

  ;; --- extra variables ---------------------
  ;; ()
  ((fancy-splash-image-file "~/.emacs.d/title.png")))

;;; emacs-sexy-day-theme.el ends here
