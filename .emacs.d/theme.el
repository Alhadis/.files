;; Disable that nasty background applied to “bold” TTY text
(tty-suppress-bold-inverse-default-colors t)

;; Remove :inverse-video effect when resolving conflicts
(unless (getenv "DISPLAY")
  (progn (custom-set-faces
          '(smerge-refined-added   ((t (:inherit smerge-refined-change))))
          '(smerge-refined-removed ((t (:inherit smerge-refined-change)))))))

;; Modeline
(column-number-mode t)
(display-battery-mode t)
(show-paren-mode t)

;; Text colours
(custom-set-faces
  '(font-lock-function-name-face ((t (:weight bold))))
  '(match ((t (:background "brightcyan" :foreground "black")))))
