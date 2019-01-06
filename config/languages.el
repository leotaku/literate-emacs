;; lsp setup

(use-package lsp-mode
  :commands lsp
  :ensure t
  :config
  (set-face-attribute 'lsp-face-highlight-textual nil :background (doom-color 'base1)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package flycheck
  :ensure t)

(use-package flycheck-rust
  :ensure t)

(use-package yasnippet
  :ensure t)

(add-hook 'rust-mode-hook #'lsp)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; (use-package company-lsp
;;   :ensure t
;;   :after (lsp-mode)
;;   :config
;;   (push 'company-lsp company-backends))
;; 
;;(add-hook 'rust-mode-hook #'lsp-rust-enable)

;; (add-hook 'rust-mode-hook #'flycheck-mode)
(add-hook 'lsp-mode-hook #'yas-minor-mode)

;; (use-package lsp-ui
;;   :ensure t
;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (la)tex

(use-package tex
  :defer t
  :ensure auctex
  :hook
  (TeX-mode . visual-line-mode)
  (LaTeX-mode . visual-line-mode)
  :config
  (setq TeX-auto-save t)
  (setq preview-default-option-list 
    '("titlesec" "pagestyles" "displaymath" "floats" "graphics" "textmath" "sections" "footnotes"))
  (TeX-source-correlate-mode)
  (TeX-PDF-mode)
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura"))
  (add-to-list 'TeX-expand-list
             '("%sn" (lambda () server-name)))
  (add-to-list 'TeX-view-program-list
             '("Zathura"
               ("zathura %o"
                (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=%sn --no-wait +%{line} %{input}\""))
               "zathura")))

(use-package magic-latex-buffer
  :ensure t
  :hook TeX-mode-hook
  :config
  (setq magic-latex-enable-block-highlight t
      magic-latex-enable-suscript        t
      magic-latex-enable-pretty-symbols  t
      magic-latex-enable-block-align     nil
      magic-latex-enable-inline-image    nil
      magic-latex-enable-minibuffer-echo t))

(use-package auctex-latexmk
  :ensure t
  :after tex
  :init
  (auctex-latexmk-setup))

(use-package company-auctex
  :ensure t
  :after tex
  :init
  (company-auctex-init))

;; markdown

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . visual-line-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

;; nix

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"                                                                                                                                                                                                
  :config
  (setf nix-indent-function #'nix-indent-line))

;; (use-package company-nixos-options
;;   :ensure t
;;   :after company
;;   :config
;;   (add-to-list 'company-backends 'company-nixos-options))

(use-package helm-nixos-options
  :ensure t
  :defer t)

;; rust

(defun configure-rust/setup-env ()
  "RLS requires some environment variables to be setup. We use rustup to get the values."
  
  (when (executable-find "rustup")
    (require 's)
    (require 'dash)
    (setq rust-default-toolchain
          (car (s-split " " (-first
                             (lambda (line) (s-match "default" line)) 
                             (s-lines (shell-command-to-string "rustup toolchain list"))))))
    ;; tell racer to use the rustup-managed rust-src
    ;; rustup component add rust-src
    (setq rust-src-path (concat (getenv "HOME") "/.multirust/toolchains/" rust-default-toolchain "/lib/rustlib/src/rust/src"))
    (setq rust-bin-path (concat (getenv "HOME") "/.multirust/toolchains/" rust-default-toolchain "/bin"))
    (setq racer-rust-src-path rust-src-path)
    (setenv "RUST_SRC_PATH" rust-src-path)
    (setenv "RUSTC" rust-bin-path)))

;; (add-hook 'rust-mode-hook #'lsp-rust-enable)

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'") ;; this is already done by rust-mode


(defun rust-mode-newline-and-indent ()
  (interactive)
  (cond
   ((or (and (looking-at "}") (looking-back "{"))
        (and (looking-at ")") (looking-back "(")))
    (progn
      (newline 2)
      (rust-mode-indent-line)
      (previous-line)
      (rust-mode-indent-line)))
   ((newline-and-indent))))

(general-define-key
 :keymap 'rust-mode-map
 "RET" 'rust-mode-newline-and-indent)

(use-package cargo
  :ensure t
  :after (rust-mode)
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (setq rust-format-on-save t))

;; (use-package company-racer
;;   :ensure t
;;   :after (rust-mode racer company)
;;   :config
;;   (add-to-list 'company-backends 'company-racer))

;; (use-package racer
;;   :ensure t
;;   :after (rust-mode)
;;   :config
;;   (setq-mode-local racer-mode
;;                    company-minimum-prefix-length 100)
;;   (general-define-key
;;    :keymaps 'racer-mode-map
;;    "TAB" #'company-indent-or-complete-common)
;;   (add-hook 'rust-mode-hook #'racer-mode))

;; lisp

;; (require 'evil)
;; (use-package parinfer
;;   :ensure t
;;   :defer t
;;   :bind
;;   (:map
;;    parinfer-mode-map
;;    ("<tab>" . parinfer-smart-tab:dwim-right)
;;    ("<backtab>" . parinfer-smart-tab:dwim-left)
;;    ("C-i" . parinfer--reindent-sexp)
;;    ("C-," . parinfer-toggle-mode))
;;   :hook
;;   (emacs-lisp-mode . parinfer-mode)
;;   :config
;;   (parinfer-strategy-add 'default 'newline-and-indent)
;;   :init
;;   (setq parinfer-auto-switch-indent-mode nil)
;;   (setq parinfer-extensions
;;         '(defaults       ;; should be included.
;;            pretty-parens  ;; different paren styles for different modes.
;;            evil           ;; If you use Evil
;;            ;;lispy          ;; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly
;;            ;;paredit      ;; Introduce some paredit commands.
;;            smart-tab      ;; C-b & C-f jump positions and smart shift with tab & S-tab.
;;            smart-yank)))  ;; Yank behavior depend on mode.

;; (use-package parinfer
;;   :ensure t
;;   :bind
;;   (("C-," . parinfer-toggle-mode))
;;   :init
;;   (progn
;;     (setq parinfer-extensions
;;           '(defaults       ; should be included.
;;             pretty-parens  ; different paren styles for different modes.
;;             evil           ; If you use Evil.
;;             lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
;;             paredit        ; Introduce some paredit commands.
;;             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;;             smart-yank))   ; Yank behavior depend on mode.
;;     (add-hook 'clojure-mode-hook #'parinfer-mode)
;;     (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'scheme-mode-hook #'parinfer-mode)
;;     (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package lispy
  :ensure t
  :hook (emacs-lisp-mode . lispy-mode))

(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (lispy-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)

(use-package lispyville
  :ensure t
  :defer t
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(additional-movement
     wrap
     additional
     additional-insert
     slurp/barf-cp
     operators)))
