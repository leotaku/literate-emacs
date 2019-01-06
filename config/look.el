;; all visual configuration

(if (boundp 'FONT) () (setq FONT "Fira Mono"))
(if (boundp 'FONT-SIZE) () (setq FONT-SIZE 100))

(scroll-bar-mode 0)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(defun set-font (font size)
  (interactive)
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
  (set-face-attribute 'org-meta-line nil :background (doom-color 'base1))
  (set-face-attribute 'org-block-begin-line nil :inherit 'org-meta-line)
  (set-face-attribute 'org-block-end-line nil :inherit 'org-meta-line)
  (set-face-attribute 'org-document-info-keyword nil :inherit 'org-meta-line))


(defadvice load-theme (after load-theme-after activate) (set-visual-conf))
(add-hook 'after-make-frame-functions 'set-visual-conf)

(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t)

(use-package doom-modeline
  :ensure t
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
  :ensure t
  :init
  (require 'cl)
  :config
  (global-yascroll-bar-mode 1)
  (scroll-bar-mode 0)
  (setq yascroll:delay-to-hide 0.5))
