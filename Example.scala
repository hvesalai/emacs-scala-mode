
/*
1. Indenting

The rules how this mode indents code, are as follows
*/

/*
1.1 Indent anchors

Indenting happens relative to an indent anchor. Usually the indent anchor is the first character on the starting line of a declaration or expression, however if
the declaration or expression is inside a parameter list, then the anchor is inside the list.
*/

def f(s: String,
      i: Int) =
  s.take(i) // indented relative to 'def'

/* */ val x = foo(
  zot, // indented relative to '/* */'
  someThing 
     map (x => x.length) // indented relative to 'someThing'
)

val x =
  foo( // indented relative to 'val'
    zot // indented relative to 'foo'
  )

class OmgLol[Option[x] forSome {
               val x: String // indented relative to 'Option'
             }] {
  // body
}

/*
1.2. Run-on lines

Any line not beginning with a declaration or expression start reserved word (i.e. class, trait, object, package, type, val, var, def, for, yield, if, else if, else, do, while) will be interpreted as a run-on of the previous line, and will be indented two step from the indent anchor. 

This rule does not apply in the following cases:
- if the previous lines was empty
- previous line was an annotation
- previous statement ended with ';,'
- the current line starts a body (of declaration or anonymous function)
- block or simple expression starts with an anonymous function (lambda) declaration
*/

val f = List("foo", "bar")
    map ( s => // run-on, anchor is 'val', also start of lambda
      s.length // not a run-on line, is body
    )
    sum // still a run-on

val f = 
  List("foo", "bar") // not a run-on line, is body
      map (s => s.length) // run-on line, anchor is 'List'

zot() // previous line was empty

val f = List("foo", "bar");
zot() // previous statement ended with ';'

val f: Bar = new Zot
    with Bar // run-on line, achor is 'val'

class Foo(
  foo: String,
  bar: Bar
      with Zot // run-on line, anchor is 'bar'
) extends Bar
    with Zot { // run-on line, anchor is 'class'
  // body here
}

trait Leijona( x: Int,
               y: Int )
    extends Kissa // run-one line, acnhor is 'trait'
    with Harja // ditto

def someThingReallyLong(having: String, aLot: Int, ofParameters: Boolean): 
    SomeAbsurdlyLongTypeThatBarelyFitsOnALine[Z, F] // run-on line, anchor is 'def'
    with SomeOtherType = { // ditto
  // body here
}

List("foo", "bar") 
    map ( s => // 'map' indented as run-on, 's =>' start lambda
      s.length
    )

List("foo") map (
  s => // start lambda
  s.length // run-on rule does not apply
)

/*
1.3 Parameter lists

Both value ('()') and type ('[]') parameter lists will be indented acording to the following rule:

- If the first parameter is on its own line, it will be indented one step from the
indent anchor.
- All following parameters will be indented to the same column with the first parameter
- Af the opening parentheses of a second or subsequent parameter group is at the start 
of a line, it will be indented to the same column as the first paremeter groups opening
parentheses.
- A closing parantheses that is on its own line, will be indented to the same column
  with the indent anchor.
- Rule does not apply, if the first parameter was a lambda expression
*/

class Foo( /* */
  i: String, // one step from indent anchor column
  k: String
) // at indent anchor column

def f(   i: String, j: String,
         k: String) // indented acording to previous

val f = foo(kissa,
            kala)

val g = bar(
  kissa,
  kala
)

val h = 
  zot(
    kissa,
    kala
  )

type ->[FirstElementOfAPair,
        SecondElementOfAPair] =
  (FirstElementOfAPair,
   SecondElementOfAPair)

/*
1.4
*/

/*
1.5 Case blocks

Any block whose first word is 'case', but not 'case class' is a case block. The lines of a case block are 
indented with two steps, except for lines beginning with the word 'case'
*/

val x = 
  f match {
    case xs: Seq[String] =>
      xs map ( x => x.length)
          sum
    case x: String =>
      println(x);
      x.length // not the ';' on previous line. Without it, this line would be interpreted as run-on
  }

/*
1.6 For comprehension

There are two special rules for for statements:

- even if the bindings of a for statement are in curly brackets, they will be indented acording to the 
parentheses indenting rules.
- yield is aligned with for
*/

val y = for { i <- 1 to 10,
              j <- 2 to 20 }
        yield i +j

val z =
  for {
    i <- 1 to 10,
    j <- 2 to 20
  } yield {
    i + j
  }

val z = for { i <- 1 to 10,
              j <- 2 to 20 }
        yield {
          i + j
        }

/*
1.7 If and try statements

If statements will be indented acording to the following rules:

- If the the body of the 'if' or 'else if' statement is a simple expression (i.e. not a block), then the next 'else if' or 'else' is aligned with the previous 'if' or 'else if',
- otherwise 'else if' and 'else' is aligned with previous block close.

Try statements will be indented acording to the following rules:
- If the body of the 'try' is a simple expression (i.e. not a block), then the next 'catch' is aligned with the previous try
- otherwise 'catch' is aligned with previous block close.
- Finally is aligned similarly
*/

val x = if (kissa)
          foo
        else if (kala)
          bar
        else
          zot

val y = if (kissa) {
  foo
} else if (kala) {
  bar
} else {
  zot
}

val a = try
          foo()
        catch {
          case e => bar()
        }

val b = try {
  foo()
} catch {
  case e => bar()
} finally {
  zot()
}

val c = try
          zot()
        finally
          log("zotted")

/*
1.8 Block opening curly brackets on new line

While not couraged, should the opening curly bracket of a block be on a new line, it is indented acording to the following rules:

- If the block is the body of a package object, object, class or trait, then the opening is aligned acording to the anchor.
- If the block is the body or value of a val, var, type, then the opening is aligned one step from the anchor
- If the block is the body of a def, then the opening is aligned acording to the anchor for Unit functions without explicitly
  specified return value and one step from anchor otherwise.
*/

class Foo 
{
  def foo
  {
    println(zot)
  }

  def bar =
    {
      zot
    }

  val zot =
    {
      "hello"
    }
}

/* 
2. font-lock support
*/

/*
2.1 Types
*/

val strings = Seq("""multi
line"quote
strings""", "normal strings", 'c')

val `quoted id` = 1

val symbol = 'Symbol
