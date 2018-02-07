(package-initialize)
(load "~/.emacs.d/packages")
(load "~/.emacs.d/indent")
(load "~/.emacs.d/keymap")

;; Site-specific
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "git-modes")
(load "git-commit")

;; Locale
(set-language-environment "UTF-8")
(setenv "LANG" "en_AU.UTF-8")
(setenv "LC_COLLATE" "C")

;; Filetype mappings
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.m?js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.tmac$" . nroff-mode))
(add-to-list 'auto-mode-alist '("\\.roff$" . nroff-mode))

;; Disable various annoyances
(tty-suppress-bold-inverse-default-colors t)
(setq auto-save-default nil
      create-lockfiles nil
      disabled-command-function nil
      js2-strict-trailing-comma-warning nil)

;; Use a single directory for storing backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/auto-save-list"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Load customisation data
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
