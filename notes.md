## Code handling

Here are the modes we want to support

`hlit --mode=merge`

* **splice or do** — Transformed into a splice
* **haskell and not ignore** — Copied to splice module
* **include** — Copied to splice module
* **not hidden** — Printed in output

If the document is a literate haskell file, it may make more sense to import
it into the splice module, rather than copying it over. This way we get line
numbers in error messages for free, and it may play more nicely with other
imports. In this case we want

`hlit --mode=import`

* **splice or do** — Transformed into a splice
* **haskell and not ignore** — Parsed for imports
* **include** — Copied to splice module
* **not hidden** — Printed in output

The imports should be copied to the splice module and an import of the
document itself should be added to splice module. Lastlty we want a mode

`hlit --mode=explicit`

* **splice or do** — Transformed into a splice
* **include** — Copied to splice module
* **not hidden** — Printed in output

The default should be `hlit --mode=auto` which will select

* `import` — if the document is a literate haskell file.
* `merge` — otherwise.

## TODO list

1. Split off Lit.Splice.
1. haskell-src-exts adds main to the export list if a module does not have a
   `module SomeName where` header. This causes problems if no such header is
   given. This needs to be worked around.
1. Configurable decimal point and thousand separator.
1. Plot.ly integration.
1. Friendlier error messages.
1. Add `--allow-io` option.
1. Safe haskell import of splice module.
