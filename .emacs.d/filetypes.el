;; JavaScript/JSX
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(dolist (name '("chakra" "d8" "js" "node" "rhino" "v8" "v8-shell"))
        (add-to-list 'interpreter-mode-alist (cons name 'js2-mode)))
(dolist (patt '("\\.[cmsp]?js\\'"
                "\\.es[0-9]?\\'"
                "\\.eslintrc\\'"
                "\\.jscript\\'"
                "\\._?js[bmse]?\\'"
                "\\.snap\\'"))
        (add-to-list 'auto-mode-alist (cons patt 'js2-mode)))


;; TypeScript
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(dolist (name '("deno" "tsc" "ts-node"))
        (add-to-list 'interpreter-mode-alist (cons name 'typescript-mode)))


;; Roff
(dolist (patt '("^\\.TH[ \t]+\\(?:\\S+\\)"
                "^'\\\\\" [tre]"))
        (add-to-list 'magic-mode-alist (cons patt 'nroff-mode)))
(dolist (patt '("\\.\\(?:[1-9]\\(?:[a-z_][a-z_0-9]*\\)?\\|0p\\|n\\|man\\|mdoc\\)\\(?:\\.in\\)?\\'"
                "\\.[gn]?roff\\'"
                "\\.mandoc\\'"
                "\\.mm[nt]\\'"
                "\\.mom\\'"
                "\\.nr\\'"
                "\\.rof\\'"
                "\\.tmac\\(-u\\|\\.in\\)?\\'"
                "\\.tr\\'"
                "/toc\\.entries\\'"))
        (add-to-list 'auto-mode-alist (cons patt 'nroff-mode)))
