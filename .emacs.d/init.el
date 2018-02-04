;; Packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available p))))
       (url (concat (if no-ssl "http" "https")
                    "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Locale
(set-language-environment "UTF-8")
(setenv "LANG" "en_AU.UTF-8")
(setenv "LC_ALL" "en_AU.UTF-8")

;; Indentation
(setq default-tab-width 4)
(setq indent-tabs-mode t)
(setq indent-line-function 'insert-tab)
(setq nxml-child-indent 4)
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-file-style "K&R")
            (setq c-basic-offset 4)
            (setq c-tab-always-indent t)
            (setq c-syntactic-indentation nil)))
(add-hook 'cperl-mode-hook
          (lambda ()
            (setq tab-width 8)
            (setq cperl-indent-level 8)
            (setq cperl-extra-newline-before-brace nil)
            (setq cperl-merge-trailing-else nil)))
(add-hook 'git-commit-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
            (setq indent-tabs-mode nil)
            (setq fill-column 72)))
(add-hook 'html-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode t)
            (define-key html-mode-map (kbd "TAB") 'self-insert-command)
            (set (make-local-variable 'sgml-basic-offset) 4)))
(add-hook 'js-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode t)
            (setq indent-line-function 'insert-tab)))
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 4)
            (setq sh-indentation 4)
            (setq sh-use-smie nil)
            (setq indent-tabs-mode t)
            (setq tab-width 4)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (aggressive-indent-mode)))

;; Improved JavaScript editing
(setq js2-strict-trailing-comma-warning nil)
(add-to-list 'auto-mode-alist '("\\.m?js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; Filetype mappings
(add-to-list 'auto-mode-alist '("\\.tmac$" . nroff-mode))
(add-to-list 'auto-mode-alist '("\\.roff$" . nroff-mode))

;; Disable newline auto-indentation
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; Load Git-related syntax highlighting
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "git-modes")
(load "git-commit")

;; Keybindings
(cua-mode)
(load "~/.emacs.d/keymap")

;; Show cursor's current column number
(setq column-number-mode t)

;; Desktop-only
(when (display-graphic-p)
  (setq ring-bell-function (lambda () ()))
  (setq visible-bell 1))

;; Non-desktop
(unless (display-graphic-p)
  (tty-suppress-bold-inverse-default-colors t))

;; Disable autosave, interlocking, and annoying crap
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq disabled-command-function nil)

;; Use a single directory for storing backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/auto-save-list")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Load customisation data
(setq custom-file "~/.emacs.d/custom")
(load custom-file)
