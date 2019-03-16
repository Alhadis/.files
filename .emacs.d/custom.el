(custom-set-variables
 '(backward-delete-char-untabify-method nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(display-battery-mode t)
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(help-window-select t))

(custom-set-faces
 '(font-lock-function-name-face ((t (:weight bold))))
 '(match ((t (:background "brightcyan" :foreground "black")))))

;; Remove :inverse-video effect when resolving conflicts
(unless (getenv "DISPLAY")
  (progn (custom-set-faces
          '(smerge-refined-added   ((t (:inherit smerge-refined-change))))
          '(smerge-refined-removed ((t (:inherit smerge-refined-change)))))))
