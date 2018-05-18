# Repliss

The **repl**icated **i**nformation **s**ystem verification tool for the development of applications with *s*trong guarantees on weakly consistent data stores.

# Compilation

Compilation requires the following tools:

- The [Scala Build Tool (SBT)](http://www.scala-sbt.org/) (Version 0.13.7)
- The [Why3](http://why3.lri.fr/) verification tool (Version 0.88.2)

    
To run the Repliss Demo webserver run:

    sbt "run --server"
    
Hostname and port can be configured with the `--host` and `--port` arguments.   
    

Other useful commands, which can be used in an SBT console:
    
    
  - Build an executable jar file with `assembly`
  - Compile with `compile`
  - Run tests with `test`
  - Run a specific file with `run <filename>`
    By default Repliss tries to verify the file. 
    Use the `--quickcheck` option to enable automatic tests and the `--noverify` option to disable verification.
  
      For example:
        
      
      
        run userbase.rpls --noverify --quickcheck    

