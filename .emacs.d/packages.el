;; Source packages from MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 27) (package-initialize))
(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Initialise `use-package.el' and auto-install missing packages
(eval-when-compile (require 'use-package nil t))
(if (fboundp 'use-package)
    ((require 'use-package-ensure)
     (setq use-package-always-ensure t))

    ;; Hack to make first-time installation easier
    (progn (setq package-selected-packages ())
           (defmacro use-package (name &rest args)
             `(push (quote ,name) package-selected-packages))))

;; Start MELPIN’
(use-package adoc-mode)
(use-package aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))
(use-package apache-mode)
(use-package bison-mode)
(use-package bnf-mode)
(use-package bnfc)
(use-package brainfuck-mode)
(use-package cmake-mode)
(use-package cc-mode
  :config
  (setq c-file-style "K&R")
  (setq c-basic-offset 4)
  (setq c-tab-always-indent t)
  (setq c-syntactic-indentation nil))
(use-package cperl-mode
  :config
  (setq tab-width 8)
  (setq cperl-indent-level 8)
  (setq cperl-extra-newline-before-brace nil)
  (setq cperl-merge-trailing-else nil))
(use-package coffee-mode)
(use-package csv-mode)
(use-package cuda-mode)
(use-package dashboard)
(use-package deadgrep)
(use-package dna-mode)
(use-package dockerfile-mode)
(use-package dotenv-mode)
(use-package dyalog-mode)
(use-package editorconfig)
(use-package enh-ruby-mode)
(use-package erlang)
(use-package fic-mode)
(use-package form-feed)
(use-package forth-mode)
(use-package glsl-mode)
(use-package go-mode)
(use-package haskell-mode)
(use-package haskell-tab-indent)
(use-package ini-mode)
(use-package js2-mode
  :config (setq js2-highlight-level 3)
          (setq js2-include-node-externs t)
          (setq js2-strict-trailing-comma-warning nil)
          (setq js2-strict-cond-assign-warning nil)
          (setq js2-strict-inconsistent-return-warning nil)
          (setq indent-line-function 'insert-tab)
          (setq indent-tabs-mode t)
          (setq tab-width 4)
  :interpreter ("chakra" "d8" "js" "node" "qjs" "rhino" "v8" "v8-shell")
  :mode (("\\.es[0-9]?\\'\\|\\.[cmsp]?js\\'\\|\\.eslintrc\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode)))
(use-package less-css-mode)
(use-package lfe-mode)
(use-package markdown-mode)
(use-package mocha
  :config
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
     (lambda ()
        (when (string-match mocha-test-directory-regexp
              (file-name-directory (buffer-file-name)))
        (setq js2-additional-externs mocha-bdd-globals)))))
(use-package move-text
  :bind ("C-<up>"   . move-text-up)
  :bind ("C-<down>" . move-text-down))
(use-package multiple-cursors)
(use-package nasm-mode)
(use-package newlisp-mode)
(use-package ninja-mode)
(use-package nroff-mode)
(use-package plisp-mode
  :config
  (setq plisp-syntax-highlighting-p t))
(use-package pov-mode)
(use-package powershell)
(use-package rust-mode)
(use-package scad-mode)
(use-package sed-mode)
(use-package sgml-mode
  :bind   ([tab] . self-insert-command)
  :config (setq tab-width 4)
          (setq indent-tabs-mode t)
          (set (make-local-variable 'sgml-basic-offset) 4))
(use-package sh-mode
  :config
  (setq sh-basic-offset 4)
  (setq sh-indentation 4)
  (setq sh-use-smie nil)
  (setq indent-tabs-mode t)
  (setq tab-width 4))
(use-package shift-number)
(use-package slime)
(use-package sml-mode)
(use-package spice-mode)
(use-package ssh-config-mode)
(use-package toml-mode)
(use-package typescript-mode
  :mode "\\.tsx\\'"
  :interpreter ("deno" "tsc" "ts-node"))
(use-package vimrc-mode)
(use-package wavefront-obj-mode)
(use-package xterm-color)
(use-package yaml-mode)
(use-package yasnippet
  :config
  (setq yas-indent-line "fixed")
  (when (boundp 'yas-global-mode)
        (yas-global-mode 1)))
