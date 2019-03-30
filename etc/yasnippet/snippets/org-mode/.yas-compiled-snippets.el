;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("<t" "test" "test" nil nil nil "/home/leo/.emacs.d/etc/yasnippet/snippets/org-mode/test" nil nil)
                       ("<s" "#+begin_src $1\n$0\n#+end_src" "source"
                        (looking-back "^<s" nil)
                        nil nil "/home/leo/.emacs.d/etc/yasnippet/snippets/org-mode/source" nil nil)
                       ("<l" "#+begin_latex\n$0\n#+end_latex" "latex"
                        (looking-back "^<l" nil)
                        nil nil "/home/leo/.emacs.d/etc/yasnippet/snippets/org-mode/latex" nil nil)
                       ("<e" "#+begin_src elisp\n$0\n#+end_src" "elisp"
                        (looking-back "^<e" nil)
                        nil nil "/home/leo/.emacs.d/etc/yasnippet/snippets/org-mode/elisp" nil nil)))


;;; Do not edit! File generated at Sat Mar 30 10:07:44 2019
