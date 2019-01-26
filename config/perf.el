;; packages for better emacs performance

(use-package vlf
  :straight t
  :defer t
  :config
  (require 'vlf-setup)
  (setq vlf-application 'dont-ask))
