;; company configuration

;; (define-globalized-minor-mode global-my-company-mode company-mode my-company-mode-on)

;; (defun my-company-mode-on ()
;;   (when (and (not (or noninteractive (eq (aref (buffer-name) 0) ?\s))))
;;        (buffer-file-name)
;;        (company-mode 1)))

;; (use-package company
;;   :straight t
;;   :defer t
;;   :config
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-require-match nil)
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-frontends '(company-tng-frontend
;;                             company-pseudo-tooltip-frontend
;;                             company-echo-metadata-frontend))

;;   (let ((keymap company-active-map))
;;     (define-key keymap [return] nil)
;;     (define-key keymap (kbd "RET") 'company-complete)
;;     (define-key keymap [tab] 'company-select-next)
;;     (define-key keymap (kbd "TAB") 'company-select-next)
;;     (define-key keymap [backtab] 'company-select-previous)
;;     (define-key keymap (kbd "S-TAB") 'company-select-previous))
  
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-dabbrev-downcase 0)
;;   (setq company-idle-delay 0.2))

;; (use-package company-quickhelp
;;   :straight t
;;   :after company
;;   :hook
;;   (company-mode . company-quickhelp-mode))

;; (global-my-company-mode 1)
