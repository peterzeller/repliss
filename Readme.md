# Repliss

The **repl**icated **i**nformation **s**ystem verification tool for the development of applications with *s*trong guarantees on weakly consistent data stores.

# Compilation

Compilation requires the following tools:

- The [Scala Build Tool (SBT)](http://www.scala-sbt.org/) (Version 0.13.7)
- The [Boogie](https://github.com/boogie-org/boogie) verification tools (Version 2.3.0.61016)

To run the examples start an SBT console and execute `run <filename>`.
For example: 

    $ sbt
    [info] Loading global plugins from /home/peter/.sbt/0.13/plugins
    [info] Loading project definition from /home/peter/work/repliss/project
    [info] Set current project to crdt-verify-scala (in build file:/home/peter/work/repliss/)
    > run examples/userbase.scala
    [info] Running crdtver.Test examples/userbase.scala
    Reading input examples/userbase.scala
    Starting boogie
    Verification successful!
    [success] Total time: 2 s, completed Dec 15, 2016 4:20:43 P
    
    
To build an executable jar file use `sbt assembly`.