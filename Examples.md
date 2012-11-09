# Examples on good and bad indentation #

This file is for discussing indentation issues. If you have an
indentation that is broken, you can make a pull request for it to be
included here along with your view on how it should be indented.

## parameter lists (value and type) ##

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

In scala-mode2, the indentation engine uses the first expression
inside the parentheses as the indent anchor.

```scala
// current scala-mode2 with indent-value-expression

val someLongName = aFunctionCall(
    one, // 1 or 2 steps varying on indent-value-expression
    two,
    tree)

class Foo extends Map[
  String,
  Boolean](
  x: String, // as opposed to style guide we have only one step here
  y: Boolean) {
  body
}

def aFunction(
  one,
  two,
  tree) {
  body
}

val someLonName = for {
    i <- 1 to 10,
    j <- 2 to 4
  } yield i * j

// style guide does not recorgnize this way of splitting the lines
val someLongName = aFunctionCall( one,
                                  two, // aligned under one
                                  three )

class Foo extends Map[String,
                      Boolean]

val someLonName = for { i <- 1 to 10,
                        j <- 2 to 4 } yield i * j
```


### Proposal on what to change ###

- parameter lists starting on a new line should follow the scala style
  guide, i.e. the list should be indented with double step (4 spaces)
  at least in the case of constructors.
- flush-parameters-left mode that stops parameters aligning


```scala
// Proposed new indentation (flush-parameters-left has no effect)

class Foo extends Map[
  String, // the style guide gives no guidance on how to indent these
  Boolean](
    x: String,
    y: Boolean) {
  body
}

def aFunction(
    one,
    two,
    tree) {
  body
}
```

```scala
// Proposed new indentation with flush-parameters-left == t

def someLongName(foo: String,
    bar: String, zot: String) {
  body
}

val someLonName = for { i <- 1 to 10,
    j <- 2 to 4 // 1 or 2 steps varying on indent-value-expression
  } yield i * j
```

## Nested blocks and run-on lines in parameter lists ##

Current indentation indents also the nested blocks and run-on
lines acording to the same anchor as the parameter list itself.

```scala
// current scala-mode2 with indent-value-expression

val someLongName = aFunctionCall(foo,
                                 aCollection map ( name =>
                                   value // a nested block
                                 ),
                                 "foo".
                                   length) // a run-on line

val someLongName = aFunctionCall(aCollection map ( name =>
                                   value
                                 ))

trait Foo extends Map[Option[x] forSome { 
                        type x <: String 
                      }, Boolean]

val someLongName = aFunctionCall(
    foo, // 1 or 2 steps varying on indent-value-expression
    aCollection map ( name =>
      value
    ))

val someLongName = aFunctionCall(
    aCollection map ( name =>
      value
    ))

trait Foo extends Map[
  Option[x] forSome {
    type x <: String
  }, Boolean]


```

### Proposal on what to change ###

- nothing, however if flush-parameters-left is implemented, then the
  nested block body is also flushed left

```scala
// indentation of nested blocks with proposed flush-parameters-left = t

val someLongName = aFunctionCall(aCollection map ( name =>
    value // 1 or 2 steps varying on indent-value-expression
  ))

val someLongName = aFunctionCall("foo".
    length)

trait Foo extends Map[Option[x] forSome {
    type x <: String
  }, Boolean]
```

## Alignment of else and yield ##

Currently `else` and `yield` are aligned below the `if` or `for` statement.

```scala
// current scala-mode2

val x = if (foo)
          1
        else
          2

val y = for (i <- 1 to 10)
        yield i * i

val someLongName = aFunctionCall(bar, 
                                 if (zot)
                                   1
                                 else
                                   3,
                                 for (i <- i to 10)
                                 yield i)

// the current indent engine has some known issues demostrated here
val someLongName = aFunctionCall(bar, if (zot)
                                   1
                                      else
                                   3)
val x = for {
    i <- i to 10 }
        yield i)

```

### Proposal on what to change ###

I'm open to ideas! One proposal was to change the way `yield` is
aligned under `for`

```scala
val y = for (i <- 1 to 10)
          yield i
```

With a variable similar to flush-parameters-left, `yield` and `else`
(and `else if`) could also be

```scala
val y = for (i <- 1 to 10)
    yield i // one or two steps depending on indent-value-expression

val y = if (zot)
    1
  else
    2
```

## Indentation of TemplateOpts (`extends`, `with`) ##

Currently we do two step indent, which is against the style guide.

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

private abstract class Foo(
  bar: String,
  zot: String
) extends Bar
    with Zot {
  body
}

private abstract class Foo( bar: String,
                            zot: String )
    extends Bar
    with Zot {
  body
}


```

### Proposal of what to change ###

I'm hesitating. Should we change to what the style guide has, i.e. one step indent

```scala
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

private abstract class Foo(
    bar: String,
    zot: String
) extends Bar
  with Zot {
  body
}

private abstract class Foo( bar: String,
                            zot: String )
  extends Bar
  with Zot {
  body
}
```
