(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])
(global-unset-key [(control h)(control c)])

(global-set-key (kbd "C-h C-f") 'describe-function)
(global-set-key (kbd "C-h C-c") 'describe-key-briefly)
(global-set-key (kbd "<backtab>") 'decrease-left-margin)
(global-set-key (kbd "<f7>") 'ispell-buffer)
(global-set-key (kbd "C-<up>") `move-text-up)
(global-set-key (kbd "C-<down>") `move-text-down)
(global-set-key (kbd "C-]") 'next-buffer)
(global-set-key (kbd "C-n") 'find-file)
(global-set-key (kbd "C-o") 'find-file-at-point)

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
