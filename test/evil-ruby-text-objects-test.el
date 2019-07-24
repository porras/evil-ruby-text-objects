(require 'evil-ruby-text-objects-test-helper)
                                        ; tests use the yank operator for some reasons:
                                        ; 1. It's easy to test afterwards (what's in the clipboard)
                                        ; 2. Not modifying the buffer avoids interactions between the tests
                                        ; 3. It means it works with all evil normal operator anyway. A separate test needs to be added for visual mode, but the whole functionality doesn't need to be tested again

(test-with-ruby-modes "Select a whole method"
                      (re-search-forward ":hey")
                      ("yarm")
                      (should-have-copied "def m" ":hey" "end"))

(test-with-ruby-modes "Select a whole method starting on the end keyword"
                      (re-search-forward ":hey") (forward-line) (end-of-line)
                      ("yarm")
                      (should-have-copied "def m" ":hey" "end"))

(test-with-ruby-modes "Select a whole method starting on the def keyword"
                      (re-search-forward ":hey") (forward-line -1) (beginning-of-line)
                      ("yarm")
                      (should-have-copied "def m" ":hey" "end"))

(test-with-ruby-modes "Select the inner content of a method"
                      (re-search-forward ":hey") (forward-line) (end-of-line)
                      ("yirm")
                      (should-have-copied ":hey"))

(test-with-ruby-modes "Select the inner content of a method starting on the end keyword"
                      (re-search-forward ":hey")
                      ("yirm")
                      (should-have-copied ":hey"))

(test-with-ruby-modes "Select a whole class"
                      (re-search-forward ":hey")
                      ("yarc")
                      (should-have-copied "class D" "def m" ":hey" "end" "end"))

(test-with-ruby-modes "Select the inner content of a class"
                      (re-search-forward ":hey")
                      ("yirc")
                      (should-have-copied "def m" ":hey" "end"))

(test-with-ruby-modes "Select a whole module"
                      (re-search-forward ":hey")
                      ("yarM")
                      (should-have-copied "module C" "class D" "def m" ":hey" "end" "end" "end"))

(test-with-ruby-modes "Select the inner content of a module"
                      (re-search-forward ":hey")
                      ("yirM")
                      (should-have-copied "class D" "def m" ":hey" "end" "end"))

(test-with-ruby-modes "Select two namespaces"
                      (re-search-forward ":hey")
                      ("y2arn")
                      (should-have-copied "module C" "class D" "def m" ":hey" "end" "end" "end"))

(test-with-ruby-modes "Try to select a method from out of the class"
                      (let ((error (should-error (execute-kbd-macro "yam") :type 'user-error)))
                        (should (equal (car (cdr error)) (format-message "Can't find current def opening")))))

(test-with-ruby-modes "Select a whole begin block"
                      (re-search-forward ":block_without_args")
                      ("yarg")
                      (should-have-copied "begin" ":block_without_args" "rescue" "whatever" "end"))

(test-with-ruby-modes "Select the inner content of a begin block"
                      (re-search-forward ":block_without_args")
                      ("yirg")
                      (should-have-copied ":block_without_args" "rescue" "whatever"))

(test-with-ruby-modes "Select a whole block without args"
                      (re-search-forward ":block_without_args")
                      ("yarb")
                      (should-have-copied "do" "begin" ":block_without_args" "rescue" "whatever" "end" "end"))

(test-with-ruby-modes "Select the inner content of a block without args"
                      (re-search-forward ":block_without_args")
                      ("yirb")
                      (should-have-copied "begin" ":block_without_args" "rescue" "whatever" "end"))

(test-with-ruby-modes "Select a whole block with args"
                      (re-search-forward ":block_with_args")
                      ("yarb")
                      (should-have-copied "do |arg|" ":block_with_args" "end"))

(test-with-ruby-modes "Select the inner content of a block with args"
                      (re-search-forward ":block_with_args")
                      ("yirb")
                      (should-have-copied ":block_with_args"))

(test-with-ruby-modes "Select a conditional block"
                      (re-search-forward "'a'")
                      ("yari")
                      (should-have-copied "if a?" "'a'" "elsif b?" "unless c?" "'c'" "else" "'b'" "end" "end"))

(test-with-ruby-modes "Select the inner content of conditional block"
                      (re-search-forward "'a'")
                      ("yiri")
                      (should-have-copied "'a'" "elsif b?" "unless c?" "'c'" "else" "'b'" "end"))

(test-with-ruby-modes "Select a nested conditional block"
                      (re-search-forward "'c'")
                      ("yari")
                      (should-have-copied "unless c?" "'c'" "else" "'b'" "end"))

(test-with-ruby-modes "Select two nested conditional blocks"
                      (re-search-forward "'c'")
                      ("y2iri")
                      (should-have-copied "'a'" "elsif b?" "unless c?" "'c'" "else" "'b'" "end"))

(test-with-ruby-modes "Select inner case block using the conditional command"
                      (re-search-forward ":foo")
                      ("yiri")
                      (should-have-copied "when 1" ":foo" "when 2" ":bar" "else" ":default"))

(test-with-ruby-modes "Select a whole method from the visual mode"
                      (re-search-forward ":hey")
                      ("varmy")
                      (should-have-copied "def m" ":hey" "end"))

(test-with-ruby-modes "Select a oneliner method"
                      (re-search-forward "oneliner")
                      ("yam")
                      (should-have-copied "def oneliner; 1; end"))

(test-with-ruby-modes "Select the inner content of a oneliner method"
                      (re-search-forward "oneliner")
                      ("yim")
                      (should-have-copied "1; "))

(test-with-ruby-modes "Select the inner content of a oneliner method while at the end of the line"
                      (re-search-forward "oneliner") (end-of-line)
                      ("yim")
                      (should-have-copied "1; "))
