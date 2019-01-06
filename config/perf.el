;; packages for better emacs performance

(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup)
  (setq vlf-application 'dont-ask))
