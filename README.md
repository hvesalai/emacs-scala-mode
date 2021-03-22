# emacs-scala-mode

The mode intends to provide basic emacs support for the Scala language, including:

- local indenting of code, comments and multi-line strings
- motion commands
- highlighting

See also [emacs-sbt-mode](https://github.com/hvesalai/emacs-sbt-mode).

## Installation

The preferred mechanism is via MELPA and `use-package` as per our
[Learning Emacs](https://www.emacswiki.org/emacs/LearningEmacs) guide:

```elisp
(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))
```

## Multi-line comments

The start of a multi-line comment is indented to the same level with
code.

By default, if a multi-line comment begins with `/*` it is considered
to be a Scaladoc comment. Scaladoc comments are indented according to
the Scaladoc style guide.

```scala
/** This is a Scaladoc comment.
  * 2nd line.
  */
```

Alternatively, if the configurable variable
*scala-indent:use-javadoc-style* is set to `t`, multi-line comments
beginning with `/**` will be indented according to the Javadoc style,
wherein all following lines are indented under the first asterisk.

```scala
/**
 * This is a Javadoc-style comment.
 * 2nd line.
 */
```

All other multi-line comments are indented under the first asterisk.

```scala
/**
 * Supercalifragilistic-
 * expialidocious!
 */

/*
 A comment
 */
```

Typing an asterisk in multi-line comment region, at the start of a
line, will trigger indent. Furthermore, if the configurable variable
`scala-indent:add-space-for-scaladoc-asterisk` is `t` (default) and
the asterisk was the last character on the line, a space will be
inserted after it. If you type a forward slash after the automatically
inserted space, the space is deleted again so that you can end the
comment without deleting the space manually.


## Filling (i.e. word wrap)

Paragraph `filling` is supported for comments and multi-line strings.
Auto-fill is not supported yet.

To re-fill a paragraph, use the `fill-paragraph` command ( `M-q` ). As
always, the column at which to wrap is controlled by the `fill-column`
variable, which you set it with the `set-fill-column` command. To set
the default, you use the `customize-variable` command or a mode-hook.


## Motion

Emacs commands `forward-sexp` and `backward-sexp` ( `M-C-f`, `M-C-b` )
motion commands will move over reserved words, literals, ids and
lists.

Text paragraph motion (i.e. `forward-paragraph`, `backward-paragraph`)
works inside comments and multi-line strings, and it respect
Scaladoc's wiki-style markup.

`scala-syntax:beginning-of-definition` and
`scala-syntax:end-of-definition` move the cursor forward and backward
over class, trait, object, def, val, var, and type definitions. These
functions are assigned to the buffer local variables
`beginning-of-defun-function` and `end-of-defun-function` which makes
it so that the `beginning-of-defun` and `end-of-defun` functions
behave in a way that is appropriate to scala. These functions are not
currently able to support some of the more advanced scala definition
types.


## Highlighting

The highlighting of variable definitions, such as

```var test = "some mutable variable"```

now result in the variable name ("test" above) to be highlighted using
the variable scala-font-lock:var-face. Per default, the value of
scala-font-lock:var-face is 'font-lock-warning-face. You can always
change the highlighting of vars by changing scala-font-lock:var-face
through the Emacs face customization (use `M-x` *customize-face*).

Very complex scala files may need the following in your emacs init
(.emacs, etc):

```lisp
;; For complex scala files
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 5000)
```

## imenu

scala-mode supports imenu, a library for accessing locations in
documents that is included in emacs 24. The custom variable
`scala-imenu:should-flatten-index` controls whether or not the imenu
index will be hierarchical or completely flat. The current iMenu
implementation only goes one level deep i.e. nested classes are not
traversed. scala-mode's imenu support depends heavily on the
`scala-syntax:end-of-definition` and
`scala-syntax:beginning-of-definition` functions, and as such, it
shares their limitations.

## Joining lines (delete indentation) and removing horizontal whitespace

Scala-mode defines its own `scala-indent:join-line` and
`scala-indent:fixup-whitespace` functions.

Unlike the normal `join-line` (aka `delete-indentation`),
`scala-indent:join-line` detects the comment fill-prefix and removes
it.

The `scala-indent:fixup-whitespace` first removes all horizontal
whitespace, then adds one space the context requires none to be
present (before semicolon, around dot, after `(` or `[`, before `)` or
`]`, etc).

## Indenting

**Where four developers meet, there are four opinions on how code should be indented**.

`scala-mode` supports 2^4 different ways of applying local heuristics to indentation.

Note that when using `sbt-scalariform`, your local indentation rules will be overwritten.

### Run-on lines

Provided by `scala-indent:default-run-on-strategy`

The indenting engine has three modes for handling run-on lines. The
`reluctant` (default) mode is geared toward a general style of coding
and the `eager` for strictly functional style. A third mode called
`operators` is between the two.

The difference between the modes is how they treat run-on lines. For
example, the `eager` mode will indent `map` in the following code

```scala
val x = List(1, 2, 3)
 map(x => x + 1)
```

The `operators` and `eager` modes will indent the second row in the
following code, as the first line ends with an operator character.

```scala
val x = 20 +
  21
```

The `reluctant` mode (default) will not indent the line in either
case. However, all three modes will indent the second line in these
examples as it is clear that the first line cannot terminate a
statement.

```scala
val x = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).
  map (x => x + 1) // last token of previous line cannot terminate a statement

val y = (List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
           map (x => x + 1)) // inside 'newlines disabled' region
```

You can use empty lines in the `eager` mode to stop it from indenting
a line. For example

```scala
val x = foo("bar")
           ("zot", "kala") // indented as curry

val y = foo("bar")

("zot", "kala") // a tuple
```

However, in all three modes pressing the `tab` key repeatedly on a
line will toggle between the modes.

### Value expressions

Provided by `scala-indent:indent-value-expression`

When this variable is set to *nil* (default), body of a value
expressions will be indented in the traditional way.

```scala
val x = try {
  some()
} catch {
  case e => other
} finally {
  clean-up()
}
```

However, when the variable is set to `t`, the body will be indented
one extra step to make the `val`, `var` or `def` stand out. For
example:

```scala
val x = try {
    some()
  } catch {
    case e => other
  } finally {
    clean-up()
  }
```

### Parameter lists

Provided by `scala-indent:align-parameters`

When this variable is set to `nil` (default), parameters and run-on
lines in parameter lists will not align under or according to the
first parameter.

```scala
val y = List( "Alpha", "Bravo",
  "Charlie" )

val x = equals(List(1,2,3) map (x =>
  x + 1))
```

When the variable is set to `t`, the same will be indented as:

```scala
val y = List( "Alpha", "Bravo",
              "Charlie" )

val x = equals(List(1,2,3) map (x =>
                 x + 1))
```

### Expression forms: if, for, try

Provided by `scala-indent:align-forms`

When this variable is set to `nil` (default), `if`, `for` and `try`
forms are not aligned specially.

```scala
val x = if (kala)
  foo
else if (koira)
  bar
else
  zot

val x = try "1".toInt
catch { case e => 0}
finally { println("hello") }

val xs = for (i <- 1 to 10)
yield i
```

When the variable is set to `t`, the same will be indented as:

```scala
val x = if (kala)
          foo
        else if (koira)
          bar
        else
          zot

val x = try "1".toInt
        catch { case e => 0}
        finally { println("hello") }

val xs = for (i <- 1 to 10)
         yield i
```

## Prettify-Symbols

Scala-mode has a preconfigured list of prettify-symbols rules. The
`prettify-symbols-mode` minor-mode (included with emacs from version
24.4 onwards) displays text in your buffer as (usually) unicode
symbols that express the same thing to improve readability. A good
example would be displaying the boolean operators as their unicode
equivalents.

To enable the feature just add these lines to the `scala-mode-hook`:

```elisp
(setq prettify-symbols-alist scala-prettify-symbols-alist)
(prettify-symbols-mode)
```

Also feel free to customise the prettify rules by adding or removing
from the `scala-prettify-symbols-alist` alist.

Libre fonts that seems to work well with this feature are:

- [Source Code Pro](https://github.com/adobe-fonts/source-code-pro)
- [Hack](https://github.com/chrissimpkins/Hack)
