;; all visual configuration

(if (boundp 'FONT) () (setq FONT "Fira Mono"))
(if (boundp 'FONT-SIZE) () (setq FONT-SIZE 100))

(scroll-bar-mode 0)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(defun set-font (font size)
  (interactive)
  (get-frame-to-variables)
  (setq FONT font)
  (setq FONT-SIZE size)
  (set-visual-conf))

(defun set-visual-conf (&optional ignore)
  (interactive)
  (set-frame-font (format "%s-%d" FONT (/ FONT-SIZE 10)) nil t)
  (set-face-attribute 'default nil :height FONT-SIZE)
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))
  ;; (set-face-attribute 'org-meta-line nil :background (doom-color 'base1))
  ;; (set-face-attribute 'org-block-begin-line nil :inherit 'org-meta-line)
  ;; (set-face-attribute 'org-block-end-line nil :inherit 'org-meta-line)
  ;; (set-face-attribute 'org-document-info-keyword nil :inherit 'org-meta-line)

  (set-frame-size (selected-frame) FRAME-WIDTH FRAME-HEIGHT t))

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun get-frame-to-variables ()
  (setq FRAME-WIDTH (- (frame-inner-width) 16))
  (setq FRAME-HEIGHT (- (frame-native-height) 2)))

(defadvice disable-theme (after disable-theme-after activate) (set-visual-conf))
(defadvice disable-theme (before disable-theme-before activate) (get-frame-to-variables))
(defadvice load-theme (after load-theme-after activate) (set-visual-conf))
(defadvice load-theme (before load-theme-before activate) (get-frame-to-variables))
(add-hook 'after-make-frame-functions 'set-visual-conf)

(get-frame-to-variables)

(use-package all-the-icons
  :straight t
  :defer t)

(use-package doom-themes
  :straight t
  :defer t)

(use-package doom-modeline
  :straight t
  :defer t
  :after all-the-icons
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 5)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-major-mode-color-icon nil)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-github-interval (* 30 60))
  (setq doom-modeline-version t))

(doom-modeline-init)

(use-package yascroll
  :straight t
  :defer t
  :init
  (require 'cl)
  :config
  (global-yascroll-bar-mode 1)
  (scroll-bar-mode 0)
  (setq yascroll:delay-to-hide 0.5))

(if (not custom-enabled-themes)
    (load-theme 'emacs-sexy-day t))
