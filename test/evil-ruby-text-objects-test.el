(require 'evil-ruby-text-objects-test-helper)
(require 'enh-ruby-mode)

                                        ; tests use the yank operator for some reasons:
                                        ; 1. It's easy to test afterwards (what's in the clipboard)
                                        ; 2. Not modifying the buffer avoids interactions between the tests
                                        ; 3. It means it works with all evil normal operator anyway. A separate test needs to be added for visual mode, but the whole functionality doesn't need to be tested again

(ert-deftest test-outer-method ()
  "Select a whole method"

  (with-ruby-sample-file
   (re-search-forward ":hey")
   ("yarm")
   (should-have-copied "def m" ":hey" "end")))

(ert-deftest test-outer-method-on-end ()
  "Select a whole method starting on the end keyword"

  (with-ruby-sample-file
   (re-search-forward ":hey") (forward-line) (end-of-line)
   ("yarm")
   (should-have-copied "def m" ":hey" "end")))

(ert-deftest test-outer-method-on-def ()
  "Select a whole method starting on the def keyword"

  (with-ruby-sample-file
   (re-search-forward ":hey") (forward-line -1) (beginning-of-line)
   ("yarm")
   (should-have-copied "def m" ":hey" "end")))

(ert-deftest test-inner-method ()
  "Select the contents of a method"

  (with-ruby-sample-file
   (re-search-forward ":hey") (forward-line) (end-of-line)
   ("yirm")
   (should-have-copied ":hey")))

(ert-deftest test-inner-method-on-end ()
  "Select the contents of a method starting on the end keyword"

  (with-ruby-sample-file
   (re-search-forward ":hey")
   ("yirm")
   (should-have-copied ":hey")))

(ert-deftest test-outer-class ()
  "Select a whole class"

  (with-ruby-sample-file
   (re-search-forward ":hey")
   ("yarc")
   (should-have-copied "class D" "def m" ":hey" "end" "end")))

(ert-deftest test-inner-class ()
  "Select the contents of a class"

  (with-ruby-sample-file
   (re-search-forward ":hey")
   ("yirc")
   (should-have-copied "def m" ":hey" "end")))

(ert-deftest test-outer-module ()
  "Select a whole module"

  (with-ruby-sample-file
   (re-search-forward ":hey")
   ("yarM")
   (should-have-copied "module C" "class D" "def m" ":hey" "end" "end" "end")))

(ert-deftest test-inner-module ()
  "Select the contents of a module"

  (with-ruby-sample-file
   (re-search-forward ":hey")
   ("yirM")
   (should-have-copied "class D" "def m" ":hey" "end" "end")))

(ert-deftest test-two-namespaces ()
  "Select two namespaces"

  (with-ruby-sample-file
   (re-search-forward ":hey")
   ("y2arn")
   (should-have-copied "module C" "class D" "def m" ":hey" "end" "end" "end")))

(ert-deftest test-def-not-found ()
  "Try to select a method from out of the class"

  (with-ruby-sample-file
   (let ((error (should-error (execute-kbd-macro "yam") :type 'user-error)))
     (should (equal (car (cdr error)) (format-message "Can't find current def opening"))))))

(ert-deftest test-outer-begin ()
  "Select a whole begin block"

  (with-ruby-sample-file
   (re-search-forward ":block_without_args")
   ("yarg")
   (should-have-copied "begin" ":block_without_args" "rescue" "whatever" "end")))

(ert-deftest test-inner-begin ()
  "Select the contents of a begin block"

  (with-ruby-sample-file
   (re-search-forward ":block_without_args")
   ("yirg")
   (should-have-copied ":block_without_args" "rescue" "whatever")))

(ert-deftest test-outer-block-without-args ()
  "Select a whole block"

  (with-ruby-sample-file
   (re-search-forward ":block_without_args")
   ("yarb")
   (should-have-copied "do" "begin" ":block_without_args" "rescue" "whatever" "end" "end")))

(ert-deftest test-inner-block-without-args ()
  "Select the contents of a block"

  (with-ruby-sample-file
   (re-search-forward ":block_without_args")
   ("yirb")
   (should-have-copied "begin" ":block_without_args" "rescue" "whatever" "end")))

(ert-deftest test-outer-block-with-args ()
  "Select a whole block"

  (with-ruby-sample-file
   (re-search-forward ":block_with_args")
   ("yarb")
   (should-have-copied "do |arg|" ":block_with_args" "end")))

(ert-deftest test-inner-block-with-args ()
  "Select the contents of a block"

  (with-ruby-sample-file
   (re-search-forward ":block_with_args")
   ("yirb")
   (should-have-copied ":block_with_args")))

(ert-deftest test-outer-conditional ()
  "Select a conditional block"

  (with-ruby-sample-file
   (re-search-forward "'a'")
   ("yari")
   (should-have-copied "if a?" "'a'" "elsif b?" "unless c?" "'c'" "else" "'b'" "end" "end")))

(ert-deftest test-inner-conditional ()
  "Select the content of conditional block"

  (with-ruby-sample-file
   (re-search-forward "'a'")
   ("yiri")
   (should-have-copied "'a'" "elsif b?" "unless c?" "'c'" "else" "'b'" "end")))

(ert-deftest test-nested-conditional ()
  "Select a nested conditional block"

  (with-ruby-sample-file
   (re-search-forward "'c'")
   ("yari")
   (should-have-copied "unless c?" "'c'" "else" "'b'" "end")))

(ert-deftest test-two-nested-conditionals ()
  "Select two nested conditional blocks"

  (with-ruby-sample-file
   (re-search-forward "'c'")
   ("y2iri")
   (should-have-copied "'a'" "elsif b?" "unless c?" "'c'" "else" "'b'" "end")))

(ert-deftest test-case-block ()
  "Select inner case block using the conditional command"

  (with-ruby-sample-file
   (re-search-forward ":foo")
   ("yiri")
   (should-have-copied "when 1" ":foo" "when 2" ":bar" "else" ":default")))

(ert-deftest test-works-in-visual-mode ()
  "Select a whole method from the visual mode"

  (with-ruby-sample-file
   (re-search-forward ":hey")
   ("varmy")
   (should-have-copied "def m" ":hey" "end")))

(ert-deftest test-oneliner-outer-method ()
  "Select a oneliner method"

  (with-ruby-sample-file
   (re-search-forward "oneliner")
   ("yam")
   (should-have-copied "def oneliner; 1; end")))

(ert-deftest test-oneliner-inner-method ()
  "Select the content of a oneliner method"

  (with-ruby-sample-file
   (re-search-forward "oneliner")
   ("yim")
   (should-have-copied "1; ")))

(ert-deftest test-oneliner-inner-method-from-end ()
  "Select the content of a oneliner method"

  (with-ruby-sample-file
   (re-search-forward "oneliner") (end-of-line)
   ("yim")
   (should-have-copied "1; ")))
