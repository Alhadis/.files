(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available p))))
       (url (concat (if no-ssl "http" "https")
                    "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               '("gnu" . "https://elpa.gnu.org/packages/")))

(custom-set-variables
 '(package-selected-packages
   '(adoc-mode
     aggressive-indent
     apache-mode
     ascii-art-to-unicode
     bison-mode
     brainfuck-mode
     cmake-mode
     coffee-mode
     csv-mode
     cuda-mode
     dockerfile-mode
     dyalog-mode
     enh-ruby-mode
     erlang
     fic-mode
     forth-mode
     glsl-mode
     go-mode
     haskell-mode
     haskell-tab-indent
     ini-mode
     js2-mode
     lfe-mode
     markdown-mode
     move-text
     nasm-mode
     newlisp-mode
     ninja-mode
     picolisp-mode
     pov-mode
     powershell
     rust-mode
     sed-mode
     slime
     sml-mode
     spice-mode
     ssh-config-mode
     toml-mode
     typescript-mode
     wavefront-obj-mode
     yaml-mode)))
