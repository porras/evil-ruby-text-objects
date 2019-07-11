(require 'evil-ruby-text-objects)
(require 'evil-test-helpers)

                                        ; workaround or enh-ruby crashes in batch mode (test run)
                                        ; taken from https://github.com/zenspider/enhanced-ruby-mode/blob/f334c42986e93c60fba144d732becfcbdb13bb7d/test/enh-ruby-mode-test.el#L13-L20
(defun erm-darken-color (name)
  (let ((attr (face-attribute name :foreground)))
    (unless (equal attr 'unspecified)
      (color-darken-name attr 20)
      "#000000")))

(defmacro should-have-copied (&rest lines)
  `(should (equal (split-string (current-kill 0) "\s*\n+\s*") ',lines)))

(defmacro with-ruby-sample-file (&rest forms)
  `(evil-test-buffer
     (insert-file-contents ,(f-join (f-dirname (f-this-file)) "sample.rb"))
     (enh-ruby-mode)
     (erm-wait-for-parse)
     (font-lock-fontify-buffer)
     ,@forms))

(provide 'evil-ruby-text-objects-test-helper)
