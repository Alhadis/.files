;; Indentation
(setq default-tab-width 4
      indent-tabs-mode t
      indent-line-function 'insert-tab
      coffee-indent-tabs-mode t
      nxml-child-indent 4)

;; Disable newline auto-indentation
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

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

(load "aggressive-indent" t)
(when (boundp 'aggressive-indent-mode)
      (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))
