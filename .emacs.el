;; UTF-8 support
;; (set-language-environment "UTF-8")
(setenv "LANG" "en_AU.UTF-8")
(setenv "LC_ALL" "en_AU.UTF-8")


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

;; Load Git-related syntax highlighting
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "git-modes")

;; Show cursor's current column number
(setq column-number-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
