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

;;; Function to load all ".el" files in ~/.emacs.d/config
(defun load-directory (directory)
  "Recursively load all Emacs Lisp files in a directory."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

;; Execute something in shell and inject results
(defun inject (command)
  "Insert output of COMMAND into current buffer"
  (interactive "sShell command: ")
  (insert-before-markers (shell-command-to-string command)))

;; Use Hunspell for spellchecking
(setq ispell-program-name (executable-find "hunspell"))

;; Load Homebrew/MELPA-installed packages
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(load "aggressive-indent")
(load "move-text")
(require 'ascii-art-to-unicode)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(autoload 'rust-mode "rust-mode" nil t)
(autoload 'coffee-mode "coffee-mode" nil t)
(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'nroff-mode "nroff-mode" nil t)

;; Filetype mappings
(add-to-list 'auto-mode-alist '("\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.mjs$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tmac$" . nroff-mode))
(add-to-list 'auto-mode-alist '("\\.roff$" . nroff-mode))

;; Disable newline auto-indentation
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; Load Git-related syntax highlighting
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "git-modes")
(load "git-commit")

;; Keybindings
(cua-mode)
(load "~/.emacs.d/keymap")

;; Show cursor's current column number
(setq column-number-mode t)

;; Disable bell while on desktop
(when (display-graphic-p)
  (setq ring-bell-function (lambda () ()))
  (setq visible-bell 1))

;; Disable autosave
(setq auto-save-default nil)

;; Use a single directory for storing backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/auto-save-list")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (slime haskell-tab-indent haskell-mode move-text ## ascii-art-to-unicode aggressive-indent)))
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
