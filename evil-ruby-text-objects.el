;;; evil-ruby-text-objects.el --- Evil text objects for Ruby code -*- lexical-binding: t -*-

;; Copyright (C) 2019 Sergio Gil

;; Author: Sergio Gil <sgilperez@gmail.com>
;; Version: 0.1
;; Keywords: languages
;; URL: https://github.com/porras/evil-ruby-text-objects
;; Package-Requires: ((emacs "25") (evil "1.2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds text objects and keybindings to work with Ruby code with Evil.
;;
;; See documentation at https://github.com/porras/evil-ruby-text-objects

;;; Code:

(require 'evil)

(defun evil-ruby-text-objects--oneliner-p (keyword)
  ""
  (string-match-p (concat "^\s*" keyword ".*;\s*end\s*$") (thing-at-point 'line)))

(defun evil-ruby-text-objects--ruby-mode-mark (count keyword)
  ""
  (if (evil-ruby-text-objects--oneliner-p keyword)
      (evil-ruby-text-objects--ruby-mode-mark-oneliner)
    (evil-ruby-text-objects--ruby-mode-mark-multiline count keyword)))

(defun evil-ruby-text-objects--ruby-mode-mark-multiline (count keyword)
  ""
  (skip-syntax-forward " ")
  (unless (looking-at keyword)
    (ruby-beginning-of-block)
    (re-search-forward "do" (line-end-position) t))
  (dotimes (i count)
    (while (not (looking-at keyword))
      (when (bobp) (user-error "Can't find current %s opening" keyword))
      (backward-up-list))
    (unless (= i (- count 1)) ;; if it's not the last one
      (backward-up-list)))
  (set-mark (point))
  (ruby-end-of-block)
  (when (looking-at "end") (evil-forward-word-begin)))

(defun evil-ruby-text-objects--ruby-mode-mark-oneliner ()
  ""
  (beginning-of-line-text)
  (set-mark (point))
  (end-of-line))

(defun evil-ruby-text-objects--enh-ruby-mode-mark (count keyword)
  ""
  (skip-syntax-forward " ")
  (unless (looking-at keyword)
    (enh-ruby-beginning-of-block))
  (dotimes (i count)
    (while (not (looking-at keyword))
      (when (bobp) (user-error "Can't find current %s opening" keyword))
      (enh-ruby-up-sexp))
    (unless (= i (- count 1)) ;; if it's not the last one
      (enh-ruby-up-sexp)))
  (set-mark (point))
  (enh-ruby-end-of-block))

(defun evil-ruby-text-objects--evil-range (count type keyword &optional inner)
  "Defines a linewise ‘evil-range’ selecting the specified Ruby expression.
COUNT: number of times it should go up the tree searching for the target
expression (for nested expressions)
TYPE: managed by ‘evil-range’ and passed as is
KEYWORD: string or regexp with the keyword that marks the beginning of the
target expression
INNER: When t, then only the content of the expression is selected but not its
opening or closing"
  (save-excursion
    (cond ((eq major-mode 'enh-ruby-mode) (evil-ruby-text-objects--enh-ruby-mode-mark count keyword))
          ((eq major-mode 'ruby-mode) (evil-ruby-text-objects--ruby-mode-mark count keyword))
          (t (user-error "evil-ruby-text-objects requires ruby-mode or enh-ruby-mode to be enabled")))
    (when inner
      (search-backward "end")
      (exchange-point-and-mark)
      (skip-chars-forward "^;\n")
      (forward-char)
      (skip-syntax-forward " "))
    (evil-text-object-make-linewise (evil-range (region-beginning) (region-end) type :expanded t))))

(defmacro evil-ruby-text-objects--define-object (object &optional keyword)
  "Defines an inner and an outer object. Accepted parameters are the OBJECT name (a string) and optionally a KEYWORD (string or regexp, defaults to the object name). It defines two evil text objects, evil-a-ruby-<OBJECT> (outer), and evil-inner-ruby-<OBJECT> (inner)."
  (let ((keyword (or keyword object))
        (outer-object (intern (concat "evil-a-ruby-" object)))
        (inner-object (intern (concat "evil-inner-ruby-" object))))
    `(progn
       (evil-define-text-object ,outer-object (count &rest _)
         ,(format "Select a Ruby %s." object)
         (evil-ruby-text-objects--evil-range count type ,keyword))
       (evil-define-text-object ,inner-object (count &rest _)
         ,(format "Select the inner content of a Ruby %s." object)
         (evil-ruby-text-objects--evil-range count type ,keyword t)))))

(evil-ruby-text-objects--define-object "method" "def")
(evil-ruby-text-objects--define-object "class")
(evil-ruby-text-objects--define-object "module")
(evil-ruby-text-objects--define-object "namespace" (regexp-opt '("class" "module")))
(evil-ruby-text-objects--define-object "block" "do")
(evil-ruby-text-objects--define-object "conditional" (regexp-opt '("if" "unless" "case")))
(evil-ruby-text-objects--define-object "begin")

;;;###autoload
(define-minor-mode evil-ruby-text-objects-mode
  "Enables Evil keybindings for ruby text objects"
  :keymap (make-sparse-keymap)
  (evil-define-key '(operator visual) evil-ruby-text-objects-mode-map
    "am" 'evil-a-ruby-method
    "am" 'evil-a-ruby-method
    "arm" 'evil-a-ruby-method
    "arc" 'evil-a-ruby-class
    "arM" 'evil-a-ruby-module
    "arn" 'evil-a-ruby-namespace
    "arb" 'evil-a-ruby-block
    "ari" 'evil-a-ruby-conditional
    "arg" 'evil-a-ruby-begin
    "im" 'evil-inner-ruby-method
    "irm" 'evil-inner-ruby-method
    "irc" 'evil-inner-ruby-class
    "irM" 'evil-inner-ruby-module
    "irn" 'evil-inner-ruby-namespace
    "irb" 'evil-inner-ruby-block
    "iri" 'evil-inner-ruby-conditional
    "irg" 'evil-inner-ruby-begin))

(provide 'evil-ruby-text-objects)

;;; evil-ruby-text-objects.el ends here
