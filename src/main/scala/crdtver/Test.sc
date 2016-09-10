import crdtver.BoogieOutputParser

val output = """
               |Boogie program verifier version 2.3.0.61016, Copyright (c) 2003-2014, Microsoft.
               |model/test.bpl(148,11): Verification of 'updateMail' timed out after 10 seconds
               |model/test.bpl(173,3): Timed out on BP5002: A precondition for this call might not hold.
               |model/test.bpl(79,1): Related location: This is the precondition that might not hold.
               |Execution trace:
               |    model/test.bpl(161,3): anon0
               |
               |Boogie program verifier finished with 7 verified, 0 errors, 1 time out
  """.stripMargin

val parser = new BoogieOutputParser()
parser.parse(output)