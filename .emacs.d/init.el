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

;; No pointless distractions, please
(setq inhibit-startup-screen t)
(blink-cursor-mode nil)

;; Modifying this file, could you not
(setq custom-file "~/.emacs.d/custom.el")

;; Don't leave clutter around while editing
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

;; Keep a familiar workflow
(cua-mode t)
(setq help-window-select t
      backward-delete-char-untabify-method nil)

;; Configure indentation
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
(setq default-tab-width 4
      indent-tabs-mode t
      indent-line-function 'insert-tab
      coffee-indent-tabs-mode t
      nxml-child-indent 4)

;; Prevent tabs from creeping into Lisp code
(dolist (hook
  (list 'emacs-lisp-mode-hook
        'lisp-interaction-mode-hook
        'lisp-mode-hook
        'scheme-mode-hook))
  (add-hook hook
     (lambda ()
       (setq indent-tabs-mode nil)
       (setq tab-width 8)
       (add-hook 'before-save-hook
         (lambda ()
           (untabify (point-min) (point-max))
           (delete-trailing-whitespace))
         nil t))))

;; Load Git-related major modes
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-hook 'git-commit-mode-hook
  (lambda ()
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
    (setq indent-tabs-mode nil)
    (setq fill-column 72)))
(load "git-modes")
(load "git-commit")

;; Load everything else
(load "~/.emacs.d/keybindings")
(load "~/.emacs.d/theme")
(load "~/.emacs.d/packages")
