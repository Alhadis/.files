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
     dna-mode
     dockerfile-mode
     dotenv-mode
     dyalog-mode
     editorconfig
     enh-ruby-mode
     erlang
     fic-mode
     form-feed
     forth-mode
     glsl-mode
     go-mode
     haskell-mode
     haskell-tab-indent
     ini-mode
     js2-mode
     less-css-mode
     lfe-mode
     markdown-mode
     mocha
     move-text
     multiple-cursors
     nasm-mode
     newlisp-mode
     ninja-mode
     picolisp-mode
     pov-mode
     powershell
     rust-mode
     scad-mode
     sed-mode
     shift-number
     slime
     sml-mode
     spice-mode
     ssh-config-mode
     toml-mode
     typescript-mode
     use-package
     vimrc-mode
     wavefront-obj-mode
     xterm-color
     yaml-mode
     yasnippet)))
