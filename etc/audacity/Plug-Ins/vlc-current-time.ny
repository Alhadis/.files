;nyquist plug-in
;name "Cursor to Current VLC Time"
;type tool
;codetype lisp
;author "John Gardner"
;copyright "Unlicense"
;release 0.1

;; Sync the cursor to the current playback position of a video playing in VLC.
;;
;; FIXME: For some reason, the `system' function doesn't work on macOS.
;; Its return value suggests successful execution, but the absence of a
;; redirection target says otherwise.

(load "nyinit.lsp"         :verbose nil)
(load "aud-do-support.lsp" :verbose nil)

(let* ((sep (string *file-separator*))
       (tmpdir (string-right-trim sep (get-temp-path)))
       (tmpout (format nil "~a~a~a" tmpdir sep "audacity-temp.out"))
       (cmd (format nil "osascript -e '~A' | tr -d '\\n' >~A"
             "tell application \"VLC\" to return the current time"
             tmpout)))
  (when (system cmd)
      ((setq fp (open tmpout :direction :input))
       (setq to (read-line fp))
       (aud-do "Select:"))))
