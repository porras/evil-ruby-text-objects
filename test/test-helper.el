(require 'evil-ruby-text-objects (f-join (f-dirname (f-this-file)) ".." "evil-ruby-text-objects.el"))
(require 'evil-test-helpers)
(require 'enh-ruby-mode)

                                        ; workaround or enh-ruby crashes in batch mode (test run)
                                        ; taken from https://github.com/zenspider/enhanced-ruby-mode/blob/f334c42986e93c60fba144d732becfcbdb13bb7d/test/enh-ruby-mode-test.el#L13-L20
(defun erm-darken-color (name)
  (let ((attr (face-attribute name :foreground)))
    (unless (equal attr 'unspecified)
      (color-darken-name attr 20)
      "#000000")))

(defmacro test-with-ruby-modes (description &rest forms)
  (let* ((test-suffix (downcase (replace-regexp-in-string "\\W" "-" description)))
         (enh-ruby-mode-test-name (mode-test-name "enh-ruby-mode" test-suffix))
         (ruby-mode-test-name (mode-test-name "ruby-mode" test-suffix)))
    `(progn
       (ert-deftest ,enh-ruby-mode-test-name ()
         ,description
         :tags '(enh-ruby-mode)
         (with-ruby-sample-file
          (enh-ruby-mode)
          (erm-wait-for-parse)
          (font-lock-fontify-buffer)
          (evil-ruby-text-objects-mode)
          ,@forms))
       (ert-deftest ,ruby-mode-test-name ()
         ,description
         :tags '(ruby-mode)
         (with-ruby-sample-file
          (ruby-mode)
          (evil-ruby-text-objects-mode)
          ,@forms))
       )))

(defun mode-test-name (mode-name test-suffix)
  (intern (s-join "-" `("test" ,mode-name ,test-suffix ,(number-to-string (random most-positive-fixnum))))))

(defmacro should-have-copied (&rest lines)
  `(should (equal (split-string (current-kill 0) "\s*\n+\s*") ',lines)))

(defmacro with-ruby-sample-file (&rest forms)
  `(evil-test-buffer
    (insert-file-contents ,(f-join (f-dirname (f-this-file)) "sample.rb"))
    ,@forms))

(provide 'evil-ruby-text-objects-test-helper)
