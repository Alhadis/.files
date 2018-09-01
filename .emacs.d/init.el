(package-initialize)
(load "~/.emacs.d/packages")
(load "~/.emacs.d/whitespace")
(load "~/.emacs.d/keymap")
(load "~/.emacs.d/hooks")

;; Site-specific
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "git-modes")
(load "git-commit")

;; Locale
(set-language-environment "UTF-8")
(setenv "LANG" "en_AU.UTF-8")
(setenv "LC_COLLATE" "C")

;; Unicode support for graphical displays
(if (getenv "DISPLAY")
    (progn (setq locale-coding-system 'utf-8)
           (set-terminal-coding-system 'utf-8)
           (set-keyboard-coding-system 'utf-8)
           (set-selection-coding-system 'utf-8)
           (prefer-coding-system 'utf-8)
           (define-coding-system-alias 'UTF-8 'utf-8)))

;; Filetype mappings
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.m?js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.tmac$" . nroff-mode))
(add-to-list 'auto-mode-alist '("\\.roff$" . nroff-mode))

;; Configure `js2-mode' for nicer JavaScript editing
(setq js2-highlight-level 3
      js2-include-node-externs t
      js2-strict-trailing-comma-warning nil
      js2-strict-cond-assign-warning nil
      js2-strict-inconsistent-return-warning nil)

;; Disable various annoyances
(tty-suppress-bold-inverse-default-colors t)
(setq auto-save-default nil
      create-lockfiles nil
      disabled-command-function nil)

;; Use a single directory for storing backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/auto-save-list"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Snippets
(load "yasnippet")
(setq yas-indent-line "fixed")
(yas-global-mode 1)

;; Everything else
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(load "~/.emacs.d/site.el" t)
