;;
;; hooks.el -- Various hooks that don't belong anywhere else.
;;

(defcustom mocha-test-directory-regexp
  "/\\(test\\|spec\\)s?/?$"
  "Regular expression for identifying test directories."
  :type 'string
  :group 'mocha)

(defconst mocha-bdd-globals
  '("after"
    "afterEach"
    "before"
    "beforeEach"
    "context"
    "describe"
    "it"
    "specify")
  "Functions globalised by Mocha's `BDD' interface.")

(defconst mocha-tdd-globals
  '("setup"
    "suite"
    "suiteSetup"
    "suiteTeardown"
    "test"
    "teardown")
  "Functions globalised by Mocha's `TDD' interface.")

(defconst mocha-qunit-globals
  '("after"
    "afterEach"
    "before"
    "beforeEach"
    "test"
    "suite")
  "Functions globalised by Mocha's `QUnit' interface.")

(add-hook 'js2-init-hook
          (lambda()
            (when (string-match mocha-test-directory-regexp
                                (file-name-directory (buffer-file-name)))
              (setq js2-additional-externs mocha-bdd-globals))))
