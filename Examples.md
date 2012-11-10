# Examples on good and bad indentation #

This file is for discussing indentation issues. If you have an
indentation that is broken, you can make a pull request for it to be
included here along with your view on how it should be indented.

## Indentation of constructor parameters and TemplateOpts (`extends`, `with`) ##

Scala style guide (example below) only recognizes two styles: either
all parameters are on one line, or then start the parameter list on a
new line. The third where the parameter lists starts on the first
line, but continues on the second is not mentioned by the guide.

```scala
// Acording to style guide, this is the only way to break
// parameters on multiple lines

class Foo(
    x: String, // note the double step (4 spaces)
    y: Boolean)
  extends Bar // note the single step (2 spaces)
```

Currently we do one step indent for params and two step indent, which
is totally oposite to the style-guide.

```scala
// current scala-mode2

private abstract class Foo( bar: String, zot: String )
    extends Bar
    with Zot {
  body
}

private abstract class Foo(
  bar: String,
  zot: String)
    extends Bar
    with Zot {
  body
}

// with scala-indent:align-parameters = t
private abstract class Foo( bar: String,
                            zot: String )
    extends Bar
    with Zot {
  body
}

// with scala-indent:align-parameters = nil
private abstract class Foo( bar: String,
  zot: String )
    extends Bar
    with Zot {
  body
}
```

### Proposal of what to change ###

I'm hesitating. Should we change to what the style guide has?

