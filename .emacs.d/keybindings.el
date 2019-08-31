(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])
(global-unset-key [(control h)(control c)])
(global-unset-key (kbd "M-<"))
(global-unset-key (kbd "M->"))

(global-set-key (kbd "C-h C-f") 'describe-function)
(global-set-key (kbd "C-h C-c") 'describe-key-briefly)
(global-set-key (kbd "<backtab>") 'decrease-left-margin)
(global-set-key (kbd "<f5>") 'toggle-truncate-lines)
(global-set-key (kbd "<f7>") 'ispell-buffer)
(global-set-key (kbd "C-M-]") 'next-buffer)
(global-set-key (kbd "C-n") 'find-file)
(global-set-key (kbd "C-o") 'find-file-at-point)
(global-set-key (kbd "C-<prior>") 'beginning-of-buffer)
(global-set-key (kbd "C-<next>") 'end-of-buffer)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(define-key lisp-interaction-mode-map (kbd "C-e") 'eval-defun)
(add-hook 'dired-load-hook (lambda() (define-key dired-mode-map (kbd "C-o") nil)))


;; Custom commands

(global-set-key
 (kbd "C-w")
 (lambda () "Close the currently active buffer."
   (interactive)
   (if (minibufferp)
       (keyboard-quit)               ;; Quit minibuffer if focussed
     (if (and (< 1 (count-windows))  ;; Otherwise, kill current buffer
              (eq major-mode 'help-mode))
         (progn (kill-buffer) (delete-window))
       (kill-buffer)))))

(global-set-key
 (kbd "C-u")
 (lambda () "Erase text between point and start-of-line."
   (interactive)
   (kill-line 0)))

(global-set-key
 (kbd "C-g")
 (lambda () "Quit minibuffer if active; otherwise, call `goto-line'."
   (interactive)
   (if (minibufferp)
       (keyboard-quit)
     (call-interactively 'goto-line))))

(global-set-key
 (kbd "<f1>")
 (lambda () "Look up documentation for the term at point."
   (interactive)
   (if (and (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
            (not (nth 4 (syntax-ppss))))
       (describe-symbol (symbol-at-point)) ;; Test: intro(3)
       (call-interactively 'man-follow))))

;;
;; Angrier buffer switching; ignores `*Messages*' and other bullcrap.
;;
(global-set-key
 (kbd "C-]")
 (lambda ()
  "Cycle through buffers without stopping on unwanted buffers."
  (interactive)
  (let (list (visible-buffer-list))
       (setq list (remove (current-buffer) list))
       (if (> 1 (length list))
            (switch-to-buffer (nth 1 list))
            (next-buffer)))))

(defun visible-buffer-list ()
  "Retrieves the contents of `buffer-list' without internal-only buffers."
  (remove nil (mapcar (lambda (buf &optional name)
                        (setq name (buffer-name buf))
                        (if (or (string= " " (substring name 0 1))
                                (string= name "*Messages*")
                                (string= name "*Completions*"))
                            nil buf))
                      (buffer-list))))
