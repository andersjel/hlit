# TODO list

1. Split off Lit.Splice.
1. haskell-src-exts adds main to the export list if a module does not have a
   `module SomeName where` header. This causes problems if no such header is
   given. This needs to be worked around.
1. Be more literate haskell friendly. Add an option to not include the code of
   a document in the splice module. Instead the splice module should import
   the literate haskell document.
1. Configurable decimal point and thousand separator.
1. Plot.ly integration.
1. Friendlier error messages.
1. Add `--allow-io` option.
1. Safe haskell import of splice module.
