;; -*- lexical-binding: t -*-
;;; evil-ruby-text-objects.el --- Defines  text objects to operate over different Ruby constructs
;;; Version: 0.1
;;; Commentary:
;; See https://github.com/porras/evil-ruby-text-objects
;;; Homepage: https://github.com/porras/evil-ruby-text-objects
;; Package-Requires: ((emacs "25") (evil "0") (enh-ruby-mode "0") (bind-key "0"))
;;; Code:


(require 'evil)
(require 'enh-ruby-mode)
(require 'bind-key)

(defun evil-ruby-text-objects--evil-range (count type keyword &optional inner)
  (save-excursion
    (skip-syntax-forward " ")
    (unless (looking-at keyword)
      (enh-ruby-beginning-of-block))
    (dotimes (i count)
      (while (not (looking-at keyword))
        (when (bobp) (user-error "Can't find current %s opening" keyword))
        (enh-ruby-up-sexp))
      (when (not (= i (- count 1))) ;; if it's not the last one
        (enh-ruby-up-sexp)))
    (set-mark (point))
    (enh-ruby-end-of-block)
    (exchange-point-and-mark)
    (when inner
      (skip-chars-forward "^;\n")
      (forward-char)
      (skip-syntax-forward " ")
      (exchange-point-and-mark)
      (search-backward "end"))
    (evil-text-object-make-linewise (evil-range (region-beginning) (region-end) type :expanded t))))

(defmacro evil-ruby-text-objects--define-object (object &optional keyword)
  "Defines an inner and an outer object. Accepted parameters are the object name (a string) and optionally a keyword (string or regexp, defaults to the object name). It defines two evil text objects, evil-a-ruby-<object> (outer), and evil-inner-ruby-<object> (inner)."
  (let ((keyword (or keyword object))
        (outer-object (intern (concat "evil-a-ruby-" object)))
        (inner-object (intern (concat "evil-inner-ruby-" object))))
    `(progn
       (evil-define-text-object ,outer-object (count &optional beg end type)
         ,(format "Select a Ruby %s." object)
         (evil-ruby-text-objects--evil-range count type ,keyword))
       (evil-define-text-object ,inner-object (count &optional beg end type)
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
(defun evil-ruby-text-objects/bind-keys ()
  "Activates the key bindings for all the ruby text objects. It should be called as a major mode hook."
  (interactive)
  (dolist (keymap '(evil-operator-state-local-map evil-visual-state-local-map))
    (bind-keys :map (symbol-value keymap)
               ("am" . evil-a-ruby-method)
               ("arm" . evil-a-ruby-method)
               ("arc" . evil-a-ruby-class)
               ("arM" . evil-a-ruby-module)
               ("arn" . evil-a-ruby-namespace)
               ("arb" . evil-a-ruby-block)
               ("ari" . evil-a-ruby-conditional)
               ("arg" . evil-a-ruby-begin)
               ("im" . evil-inner-ruby-method)
               ("irm" . evil-inner-ruby-method)
               ("irc" . evil-inner-ruby-class)
               ("irM" . evil-inner-ruby-module)
               ("irn" . evil-inner-ruby-namespace)
               ("irb" . evil-inner-ruby-block)
               ("iri" . evil-inner-ruby-conditional)
               ("irg" . evil-inner-ruby-begin))))

;;;###autoload
(add-hook 'enh-ruby-mode-hook 'evil-ruby-text-objects/bind-keys)

(provide 'evil-ruby-text-objects)

;;; evil-ruby-text-objects.el ends here
