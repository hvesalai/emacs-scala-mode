# scala-mode2 — A new scala-mode for emacs

This is a new scala major mode for emacs 24. It is a complete rewrite
based on the *Scala Language Specification 2.9*.

The mode intends to provide the basic emacs support, including
- indenting of code, comments and multi-line strings
- motion commands
- highlighting

Currently the indenting of code has been finalized. Highlighting is
under work. No scala specific motion commands have been added, but
standard emacs motions work ofcourse.

## Setting the mode up for use

1. Make sure you have the latest version of **GNU Emacs** installed. 
The mode has been developed on 24.2 and uses features not available
in emacs prior to version 24.

2. Download the files to a local directory. You can use the *git clone*
command, this will create a new directory called scala-mode2.
```
git clone git://github.com/hvesalai/scala-mode2.git
```

3. Include the following in your `.emacs`  file. If you have been
using the old scala-mode, make sure it is no longer in *load-path*.
```
(add-to-list 'load-path "/path/to/scala-mode2/")
(require 'scala-mode)
```

4. That's it. Next you can start emacs and take a look at the
customization menu for scala-mode (use **M-x** *customize-mode* when
in scala-mode or use **M-x** *customize-variable* to customize one
variable). Also be sure to check the customization tips on various
keyboard commands and general emacs parameters which cannot be
modified from the scala-mode customization menu.

## Indenting modes

*Where four developers meet, there are four opinions on how code should
be indented. Luckily scala-mode already supports 2^4 different ways of 
indenting.*

### Run-on lines (scala-indent:default-run-on-strategy)

The indenting engine has three modes for handling run-on lines. The
**reluctant** (default) mode is geared toward a general style of coding
and the **eager** for strictly functional style. A third mode called
**operators** is between the two.

The difference between the modes is how they treat run-on lines. For
example, the *eager* mode will indent *map* in the following code

```
val x = List(1, 2, 3)
  map(x => x + 1)
```

The *operators* and *eager* modes will indent the second row in the
following code, as the first line ends with an operator character.

```
val x = 20 + 
  21
```

The *reluctant* mode (default) will not indent the line in either
case. However, all three modes will indent the second line in these
examples as it is clear that the first line cannot terminate a statement
(see the *Scala Language Specification 2.9*, section 1.2).

```
val x = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).
  map (x => x + 1) // last token of previous line cannot terminate a statement

val y = (List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
           map (x => x + 1)) // inside 'newlines disabled' region
```

You can use empty lines in the *eager* mode to stop it from indenting a
line. For example

```
val x = foo("bar")
           ("zot", "kala") // indented as curry

val y = foo("bar")

("zot", "kala") // a tuple
```

However, in all three modes pressing the **tab** key repeatedly on a
line will toggle between the modes.

### Value expressions (scala-indent:indent-value-expression)

When this variable is set to *t* (default), blocks in value
expressions will be indented one extra step to make the *val*, *var*
or *def* stand out. For example:

```
val x = try {
    some()
  } catch {
    case e => other
  } finally {
    clean-up()
  }
```

When the variable is set to *nil*, the same will indent as:

```
val x = try {
  some()
} catch {
  case e => other
} finally {
  clean-up()
}
```

### Parameter lists (scala-indent:align-parameters)

When this variable is set to *t* (default), parameters and run-on
lines in parameter lists will always align under and acording to the
first parameter.

```
val y = List( "Alpha", "Bravo",
              "Charlie" )

val x = equals(List(1,2,3) map (x =>
                 x + 1))
```

When the variable is set to *nil*, the same will be indented as:

```
val y = List( "Alpha", "Bravo",
    "Charlie" )

val x = equals(List(1,2,3) map (x =>
    x + 1))
```

### Expresison forms: if, for, try (scala-indent:align-forms)

When this variable is set to *t* (default), *if*, *for* and *try*
forms are aligned.

```
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

When the variable is set to *nil*, the same will be indented as:

```
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

## Indenting multi-line comments

The start of a multi-line comment is indented to the same level with
code.

If a multi-line comment begins with `/**` it is considered to be a
scaladoc comment. Scaladoc comments are indented acording to the
scaladoc style guide.

```
/** This is a scaladoc comment.
  * 2nd line.
  */
```

Other multi-line comments are indented under the first asterix.

```
/****
 * Supercalifragilistic-
 * expialidocious!
 */

/*
 A comment
 */
```

## Filling (i.e. word wrap)

Paragraph *filling* (emacs jargon for word wrap) is supported for
comments and multi-line strings. Auto-fill is not supported yet.

To re-fill a paragraph, use the *fill-paragraph* command ( **M-q**
). As always, the column at which to wrap is controlled by the
*fill-column* variable, which you set it with the *set-fill-column*
command. To set the default, you use the *customize-variable* command
or a mode-hook.

## Motion

Basic emacs motion will work as expected. 

Text paragraph motion (i.e. *forward-paragraph*, *backward-paragraph*)
works inside comments and multi-line strings, and it respect scaladoc's
wiki-style markup.

The commands *forward-sexp* and *backward-sexp* ( **M-C-f**, **M-C-b**
) motion commands will move over reserved words, literals, ids and
lists.

## Keymap and other commands

For the sake of customizability, scala-mode does not alter the default
emacs keymap beyond what is absolutely necessary. However, you can customize
the mode through the scala-mode-hook. Here are some suggested modification
you may want to try. Just copy-paste it to your `.emacs` file.

```lisp
(add-hook 'scala-mode-hook '(lambda ()

  ;; Bind the 'newline-and-indent' command to RET (aka 'enter'). This
  ;; is normally also available as C-j. The 'newline-and-indent'
  ;; command has the following functionality: 1) it removes trailing
  ;; whitespace from the current line, 2) it create a new line, and 3)
  ;; indents it.  An alternative is the
  ;; 'reindent-then-newline-and-indent' command.
  (local-set-key (kbd "RET") 'newline-and-indent)

  ;; Bind the 'join-line' command to C-M-j. This command is normally
  ;; bound to M-^ which is hard to access, especially on some European
  ;; keyboards. The 'join-line' command has the effect or joining the
  ;; current line with the previous while fixing whitespace at the
  ;; joint.
  (local-set-key (kbd "C-M-j") 'join-line)

  ;; Bind the backtab (shift tab) to
  ;; 'scala-indent:indent-with-reluctant-strategy command. This is usefull
  ;; when using the 'eager' mode by default and you want to "outdent" a 
  ;; code line as a new statement.
  (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)

  ;; Add the scalaindent:indent-scaladoc-asterisk
  ;; post-self-insert-hook which indents the multi-line comment
  ;; asterisk to its correct place when you type it. It also adds a
  ;; space after the asterisk if the parameter to the function is
  ;; t. If you don't want the space, replace t with nil.
  (add-hook 'post-self-insert-hook
            '(lambda () (scala-mode:indent-scaladoc-asterisk t)))

  ;; and other bindings here
))
```

## Whitespace

Emacs has a very nice minor mode for highlighting bad whitespace and
removing any unwanted whitespace when you save a file. To use it, add
this to your .emacs file.

```lisp
(add-hook 'scala-mode-hook '(lambda ()
  (require 'whitespace)

  ;; clean-up whitespace at save
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; turn on highlight. To configure what is highlighted, customize
  ;; the *whitespace-style* variable. A sane set of things to
  ;; highlight is: face, tabs, trailing
  (whitespace-mode)
))
```

Since scala is indented with two spaces, scala-mode2 does not use tabs
at all for indents by default. If you want to turn on tabs mode, you
can do so in the mode hook (set *indent-tabs-mode* to t).

## Code highlighting

Highlighting code is still a work in progress. Feedback on how it
should work is welcomed as issues to this github project.

Free emacs tip: if you are using emacs from a text terminal with dark
background and you are having trouble with colors, try setting the
customization variable *frame-background-mode* to *dark* (use **M-x**
*customize-variable*).

## Other features
- highlights only properly formatted string and character constants
- indenting a code line removes trailing whitespace

## Future work

- beginning-of-defun, end-of-defun
- indent case, etc after they are typed (use first space as self-insert-hook)
- indent multi-line strings with margin correctly
- movement commands to move to previous or next definition (val,
  var, def, class, trait, object)
- highlight headings and annotations inside scaladoc specially (use
  underline for headings)
- highlight variables in string interpolation (scala 2.10)

All suggestions and especially pull requests are welcomed in github
https://github.com/hvesalai/scala-mode2

## Credits

Mode development: Heikki Vesalainen

Contributors and valuable feedback:
- Ray Racine
- Eiríkr Åsheim (aka Erik Osheim)
- Seth Tisue
- Gary Pamparà