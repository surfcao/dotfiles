;;; Compiled snippets and support files for `php-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'php-mode
                     '(("var" "/**\n * @var ${3:bool} ${4:Description}\n */\n${1:$$(yas-choose-value '(\"public \" \"private \" \"protected \"))}\\$${2:variable}$0;\n" "variable" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/variable" nil nil)
                       ("todo" "// @todo ${1:Do something}" "todo" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/todo" "direct-keybinding" nil)
                       ("fu" "/**\n * Documentation\n *\n * @return void\n */\n${1:$$(yas-choose-value '(\"public \" \"public static \" \"private \" \"private static \" \"protected \" \"protected static \"))}function ${3:new_function}(${4:$args}) {\n  $0\n}\n" "function" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/function" nil nil)
                       ("foreach" "foreach (${1:things} as $${2:thing}) {\n  $0\n}" "foreach" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/foreach" "direct-keybinding" nil)
                       ("doc_h" "/**\n * ${1:A PHP script file.}\n *\n * PHP Version 5\n *\n * @author    Aaron Bieber <abieber@wayfair.com>\n * @copyright 2015 Wayfair LLC - All rights reserved\n */\n$0" "file_head_documentation" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/file_head_documentation" nil nil)
                       ("ed" "echo \"${1:Hello}\"; die();" "echo_and_die" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/echo_and_die" "direct-keybinding" nil)
                       ("ebr" "echo \"$0<br/>\";\n" "echo-html" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/echo-html" "direct-keybinding" nil)
                       ("doc_vp" "/**\n * ${1:description}\n *\n * @var ${2:type}\n */" "doc-variable-post" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/doc_vp" "direct-keybinding" nil)
                       ("doc_v" "/**\n * ${3:description}\n *\n * @var ${4:type}\n */\n${1:$$(yas-choose-value '(\"public \" \"protected \" \"private \"))} ${2:$variable};" "doc-variable" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/doc_v" "direct-keybinding" nil)
                       ("doc_fp" "/**\n * Documentation\n *\n * @return ${1:type} ${2:Description}\n */" "doc-function-post" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/doc_f" "direct-keybinding" nil)
                       ("del" "// @todo @delete" "del" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/del" "direct-keybinding" nil)
                       ("dd" "debug(${1:what}, '$1'); // @todo @delete" "debug_with_description" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/debug_with_description" "direct-keybinding" nil)
                       ("ds" "debug('${1:what}'); // @todo @delete" "debug_just_string" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/debug_just_string" "direct-keybinding" nil)
                       ("de" "debug(${1:what}); // @todo @delete" "debug" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/debug" nil nil)
                       ("class" "class ${1:Class_Name} {\n  $0\n}\n" "class" nil nil nil "/Users/guofeng/.emacs.d/snippets/php-mode/class" nil nil)))


;;; Do not edit! File generated at Mon Nov  2 16:08:28 2020
