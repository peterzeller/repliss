# Repliss

The **repl**icated **i**nformation **s**ystem verification tool for the development of applications with *s*trong guarantees on weakly consistent data stores.

# Compilation

Compilation requires the following tools:

- The [Scala Build Tool (SBT)](http://www.scala-sbt.org/)
- The [CVC4](https://cvc4.github.io/) library
    - Compile the CVC4 library and put the files (`libcvc4jni.so`, `libcvc4.so`, and `libcvc4.so.6`) under `native/bin`

    
To run the Repliss Demo webserver run:

    sbt "run --server"
    
Hostname and port can be configured with the `--host` and `--port` arguments.   
    

Other useful commands, which can be used in an SBT console:
    
    
  - Build an executable jar file with `assembly`
  - Compile with `compile`
  - Run tests with `test`
  - Run a specific file with `run <filename>`
    By default Repliss only parses and typechecks the file. 
    Use the `--quickcheck` option to enable automatic tests and the `--symbolicCheck` option to verify the input.
  
      For example:
        
      
      
        run userbase.rpls --symbolicCheck --quickcheck    

