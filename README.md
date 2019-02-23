<div align="center">
    <h1><i>Literate Emacs</i></h1>
    a literate Emacs configuration using <a href="https://orgmode.org/">org</a>/<a href="https://github.com/OrgTangle/ntangle">ntangle</a>
</div>

This repository contains several important files:
- init.org :: the main configuration file. tangled to init.el via org/ntangle
- snippets.org :: the file where my [[https://github.com/joaotavora/yasnippet][yasnippet]] snippets are stored. 
- unmanaged :: lisp code or other resources that are not substantial enough to be packaged
  - themes :: my personal emacs theme(s)
  - icons :: png icons taken from [[http://emacs.sexy][emacs.sexy]]
- straight :: directory where [[https://github.com/raxod502/straight.el][straight.el]] stores its files.
  - versions/defaut.el :: lockfile for straight.el
- var :: managed by [[https://github.com/emacscollective/no-littering][no-littering.el]]
- etc :: managed by no-littering.el

In the future this repo will also contain a valid [[https://nixos.org/nix/][nix]] expression for the ntangle tool, which I use to tangle my org-mode configuration files.

** Current look
[[file:scrot.png]]
