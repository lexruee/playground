# Scala

From Wikipedia:

```
Scala is a general-purpose programming language providing support for functional programming 
and a strong static type system. Designed to be concise, many of Scala's design decisions 
aimed to address criticisms of Java.
```

## Installing Scala on Arch Linux

First, we need to install the scala package:

```
sudo pacman -S scala
```

Once installed, you should have a working scala compiler `scalac` 
and a scala interpreter or repl `scala`.

Lastly, we install the scala build tool called `sbt`:

```
sudo pacman -S sbt
```

## Scala Compiler

```
scalac -help
scalac -version
```

## Scala REPL

```
scala -help
scala -version
```

## Scala Build Tool

```
sbt -help
sbt help
```

## Creating an empty scala project

```
sbt new scala/hello-world.g8
```

## Running the hello-world project

```
cd hello-world
sbt run
```


## Learning Resources

* https://docs.scala-lang.org/getting-started-sbt-track/getting-started-with-scala-and-sbt-on-the-command-line.html
* https://learnxinyminutes.com/docs/scala/
* https://www.scala-exercises.org/scala_tutorial/terms_and_types
* https://wiki.archlinux.org/index.php/Scala
* https://en.wikipedia.org/wiki/Scala_(programming_language)
