;;; typescript-ts-mode.el --- tree sitter support for TypeScript  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Created    : October 2022
;; Keywords   : typescript tsx languages tree-sitter

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'treesit)
(require 'js)
(eval-when-compile (require 'rx))
(require 'c-ts-mode) ; For comment indent and filling.

(declare-function treesit-parser-create "treesit.c")

(defcustom typescript-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `typescript-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'typescript)

(defvar typescript-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Taken from the cc-langs version
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?%  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?\' "\""    table)
    (modify-syntax-entry ?\240 "."   table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?\^m "> b" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for `typescript-ts-mode'.")

(defun typescript-ts-mode--indent-rules (language)
  "Rules used for indentation.
Argument LANGUAGE is either `typescript' or `tsx'."
  `((,language
     ((parent-is "program") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((and (parent-is "comment") c-ts-mode--looking-at-star)
      c-ts-mode--comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "ternary_expression") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "member_expression") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "named_imports") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "statement_block") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "type_arguments") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "variable_declarator") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "arguments") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "array") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "formal_parameters") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "template_substitution") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "object_pattern") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "object") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "object_type") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "enum_body") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "class_body") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "arrow_function") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "binary_expression") parent-bol typescript-ts-mode-indent-offset)

     ,@(when (eq language 'tsx)
         `(((match "<" "jsx_fragment") parent 0)
           ((parent-is "jsx_fragment") parent typescript-ts-mode-indent-offset)
           ((node-is "jsx_closing_element") parent 0)
           ((node-is "jsx_element") parent typescript-ts-mode-indent-offset)
           ((parent-is "jsx_element") parent typescript-ts-mode-indent-offset)
           ((parent-is "jsx_opening_element") parent typescript-ts-mode-indent-offset)
           ((parent-is "jsx_expression") parent-bol typescript-ts-mode-indent-offset)
           ((match "/" "jsx_self_closing_element") parent 0)
           ((parent-is "jsx_self_closing_element") parent typescript-ts-mode-indent-offset)))
     (no-node parent-bol 0))))

(defvar typescript-ts-mode--keywords
  '("!" "abstract" "as" "async" "await" "break"
    "case" "catch" "class" "const" "continue" "debugger"
    "declare" "default" "delete" "do" "else" "enum"
    "export" "extends" "finally" "for" "from" "function"
    "get" "if" "implements" "import" "in" "instanceof" "interface"
    "keyof" "let" "namespace" "new" "of" "private" "protected"
    "public" "readonly" "return" "set" "static" "switch"
    "target" "throw" "try" "type" "typeof" "var" "void"
    "while" "with" "yield")
  "TypeScript keywords for tree-sitter font-locking.")

(defvar typescript-ts-mode--operators
  '("=" "+=" "-=" "*=" "/=" "%=" "**=" "<<=" ">>=" ">>>=" "&=" "^="
    "|=" "&&=" "||=" "??=" "==" "!=" "===" "!==" ">" ">=" "<" "<=" "+"
    "-" "*" "/" "%" "++" "--" "**" "&" "|" "^" "~" "<<" ">>" ">>>"
    "&&" "||" "!" "?.")
  "TypeScript operators for tree-sitter font-locking.")

(defun typescript-ts-mode--font-lock-settings (language)
  "Tree-sitter font-lock settings.
Argument LANGUAGE is either `typescript' or `tsx'."
  (treesit-font-lock-rules
   :language language
   :override t
   :feature 'comment
   `((comment) @font-lock-comment-face)

   :language language
   :override t
   :feature 'constant
   `(((identifier) @font-lock-constant-face
      (:match "^[A-Z_][A-Z_\\d]*$" @font-lock-constant-face))

     [(true) (false) (null)] @font-lock-constant-face)

   :language language
   :override t
   :feature 'keyword
   `([,@typescript-ts-mode--keywords] @font-lock-keyword-face
     [(this) (super)] @font-lock-keyword-face)

   :language language
   :override t
   :feature 'string
   `((regex pattern: (regex_pattern)) @font-lock-regexp-face
     (string) @font-lock-string-face
     (template_string) @js--fontify-template-string
     (template_substitution ["${" "}"] @font-lock-misc-punctuation-face))

   :language language
   :override t
   :feature 'declaration
   `((function
      name: (identifier) @font-lock-function-name-face)

     (function_declaration
      name: (identifier) @font-lock-function-name-face)

     (method_definition
      name: (property_identifier) @font-lock-function-name-face)
     (method_signature
      name: (property_identifier) @font-lock-function-name-face)
     (required_parameter (identifier) @font-lock-variable-name-face)
     (optional_parameter (identifier) @font-lock-variable-name-face)

     (variable_declarator
      name: (identifier) @font-lock-variable-name-face)

     (enum_declaration (identifier) @font-lock-type-face)

     (extends_clause value: (identifier) @font-lock-type-face)
     ;; extends React.Component<T>
     (extends_clause value: (member_expression
                             object: (identifier) @font-lock-type-face
                             property: (property_identifier) @font-lock-type-face))

     (arrow_function
      parameter: (identifier) @font-lock-variable-name-face)

     (variable_declarator
      name: (identifier) @font-lock-function-name-face
      value: [(function) (arrow_function)])

     (variable_declarator
      name: (array_pattern
             (identifier)
             (identifier) @font-lock-function-name-face)
      value: (array (number) (function)))

     (catch_clause
      parameter: (identifier) @font-lock-variable-name-face)

     (import_clause (identifier) @font-lock-variable-name-face)
     (import_clause (named_imports (import_specifier (identifier)) @font-lock-variable-name-face)))

   :language language
   :override t
   :feature 'identifier
   `((nested_type_identifier
      module: (identifier) @font-lock-type-face)

     (type_identifier) @font-lock-type-face

     (predefined_type) @font-lock-type-face

     (new_expression
      constructor: (identifier) @font-lock-type-face)

     (enum_body (property_identifier) @font-lock-type-face)

     (enum_assignment name: (property_identifier) @font-lock-type-face)

     (variable_declarator
      name: (identifier) @font-lock-variable-name-face)

     (for_in_statement
      left: (identifier) @font-lock-variable-name-face)

     (arrow_function
      parameters:
      [(_ (identifier) @font-lock-variable-name-face)
       (_ (_ (identifier) @font-lock-variable-name-face))
       (_ (_ (_ (identifier) @font-lock-variable-name-face)))]))

   :language language
   :override t
   :feature 'property
   `((property_signature
      name: (property_identifier) @font-lock-property-face)
     (public_field_definition
      name: (property_identifier) @font-lock-property-face)

     (pair key: (property_identifier) @font-lock-variable-name-face)

     ((shorthand_property_identifier) @font-lock-property-face)

     ((shorthand_property_identifier_pattern)
      @font-lock-property-face))

   :language language
   :override t
   :feature 'expression
   '((assignment_expression
      left: [(identifier) @font-lock-function-name-face
             (member_expression
              property: (property_identifier) @font-lock-function-name-face)]
      right: [(function) (arrow_function)]))

   :language language
   :feature 'function
   '((call_expression
      function:
      [(identifier) @font-lock-function-name-face
       (member_expression
        property: (property_identifier) @font-lock-function-name-face)]))

   :language language
   :override t
   :feature 'pattern
   `((pair_pattern
      key: (property_identifier) @font-lock-property-face)

     (array_pattern (identifier) @font-lock-variable-name-face))

   :language language
   :override t
   :feature 'jsx
   `((jsx_opening_element
      [(nested_identifier (identifier)) (identifier)]
      @font-lock-function-name-face)

     (jsx_closing_element
      [(nested_identifier (identifier)) (identifier)]
      @font-lock-function-name-face)

     (jsx_self_closing_element
      [(nested_identifier (identifier)) (identifier)]
      @font-lock-function-name-face)

     (jsx_attribute (property_identifier) @font-lock-constant-face))

   :language language
   :feature 'number
   `((number) @font-lock-number-face
     ((identifier) @font-lock-number-face
      (:match "^\\(:?NaN\\|Infinity\\)$" @font-lock-number-face)))

   :language language
   :feature 'operator
   `([,@typescript-ts-mode--operators] @font-lock-operator-face
     (ternary_expression ["?" ":"] @font-lock-operator-face))

   :language language
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language language
   :feature 'delimiter
   '((["," "." ";" ":"]) @font-lock-delimiter-face)

   :language language
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)))

(defvar typescript-ts-mode--sentence-nodes
  '("import_statement"
    "debugger_statement"
    "expression_statement"
    "if_statement"
    "switch_statement"
    "for_statement"
    "for_in_statement"
    "while_statement"
    "do_statement"
    "try_statement"
    "with_statement"
    "break_statement"
    "continue_statement"
    "return_statement"
    "throw_statement"
    "empty_statement"
    "labeled_statement"
    "variable_declaration"
    "lexical_declaration"
    "property_signature")
  "Nodes that designate sentences in TypeScript.
See `treesit-sentence-type-regexp' for more information.")

(defvar typescript-ts-mode--sexp-nodes
  '("expression"
    "pattern"
    "array"
    "function"
    "string"
    "escape"
    "template"
    "regex"
    "number"
    "identifier"
    "this"
    "super"
    "true"
    "false"
    "null"
    "undefined"
    "arguments"
    "pair")
  "Nodes that designate sexps in TypeScript.
See `treesit-sexp-type-regexp' for more information.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;;;###autoload
(define-derived-mode typescript-ts-base-mode prog-mode "TypeScript"
  "Major mode for editing TypeScript."
  :group 'typescript
  :syntax-table typescript-ts-mode--syntax-table

  ;; Comments.
  (c-ts-mode-comment-setup)
  (setq-local treesit-defun-prefer-top-level t)

  (setq-local treesit-text-type-regexp
              (regexp-opt '("comment"
                            "template_string")))

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (regexp-opt '("class_declaration"
                            "method_definition"
                            "function_declaration"
                            "lexical_declaration")))
  (setq-local treesit-defun-name-function #'js--treesit-defun-name)

  (setq-local treesit-sentence-type-regexp
              (regexp-opt typescript-ts-mode--sentence-nodes))

  (setq-local treesit-sexp-type-regexp
              (regexp-opt typescript-ts-mode--sexp-nodes))

  ;; Imenu (same as in `js-ts-mode').
  (setq-local treesit-simple-imenu-settings
              `(("Function" "\\`function_declaration\\'" nil nil)
                ("Variable" "\\`lexical_declaration\\'"
                 js--treesit-valid-imenu-entry nil)
                ("Class" ,(rx bos (or "class_declaration"
                                      "method_definition")
                              eos)
                 nil nil))))

;;;###autoload
(define-derived-mode typescript-ts-mode typescript-ts-base-mode "TypeScript"
  "Major mode for editing TypeScript."
  :group 'typescript
  :syntax-table typescript-ts-mode--syntax-table

  (when (treesit-ready-p 'typescript)
    (treesit-parser-create 'typescript)

    ;; Indent.
    (setq-local treesit-simple-indent-rules
                (typescript-ts-mode--indent-rules 'typescript))

    ;; Font-lock.
    (setq-local treesit-font-lock-settings
                (typescript-ts-mode--font-lock-settings 'typescript))
    (setq-local treesit-font-lock-feature-list
                '((comment declaration)
                  (keyword string escape-sequence)
                  (constant expression identifier number pattern property)
                  (function bracket delimiter)))

    (treesit-major-mode-setup)))

;;;###autoload
(define-derived-mode tsx-ts-mode typescript-ts-base-mode "TypeScript[TSX]"
  "Major mode for editing TypeScript."
  :group 'typescript
  :syntax-table typescript-ts-mode--syntax-table

  (when (treesit-ready-p 'tsx)
    (treesit-parser-create 'tsx)

    ;; Comments.
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx (or (seq "/" (+ "/"))
                                           (seq "/" (+ "*")))
                                       (* (syntax whitespace))))
    (setq-local comment-end-skip
                (rx (* (syntax whitespace))
                    (group (or (syntax comment-end)
                               (seq (+ "*") "/")))))

    ;; Indent.
    (setq-local treesit-simple-indent-rules
                (typescript-ts-mode--indent-rules 'tsx))

    ;; Navigation
    (setq-local treesit-sentence-type-regexp
                (regexp-opt (append
                             typescript-ts-mode--sentence-nodes
                             '("jsx_element"
                               "jsx_self_closing_element"))))

  (setq-local treesit-sexp-type-regexp
              (regexp-opt (append
                           typescript-ts-mode--sexp-nodes
                           '("jsx"))))

    ;; Font-lock.
    (setq-local treesit-font-lock-settings
                (typescript-ts-mode--font-lock-settings 'tsx))
    (setq-local treesit-font-lock-feature-list
                '((comment declaration)
                  (keyword string escape-sequence)
                  (constant expression identifier jsx number pattern property)
                  (function bracket delimiter)))

    (treesit-major-mode-setup)))

(provide 'typescript-ts-mode)

;;; typescript-ts-mode.el ends here
