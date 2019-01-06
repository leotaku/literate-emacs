;; packages for use with things other than text files

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . (lambda () (face-remap-add-relative 'variable-pitch
                                :family "Liberation Serif"
                                :height 1.5))))
