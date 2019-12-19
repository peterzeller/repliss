
Repliss is a verification tool for replicated information systems with strong correctness guarantees. It can be used to build dependable applications on top of highly available databases like [AntidoteDB](https://www.antidotedb.eu/).

Invariants written in Repliss can be checked using automatic testing to find bugs in an application and they can be formally verified to prove the absence of bugs. To learn more, read the User Documentation below.




<div class="alert alert-info alert">
  <p>The Repliss Tool is still under development. 
  Not all features documented here are already implemented and for others the documentation might be outdated.</p>
</div>

Bug reports and pull requests are welcome at [Github](https://github.com/peterzeller/repliss/).


## Overview

Here is a typical workflow when developing an highly available application with Repliss.

1. Define the data model using replicated data types (CRDTs).
2. Implement procedures to work with the data model.
3. Specify the expected behaviour of the application using assertions and invariants.
4. Use the automatic testing tool to find potential bugs.
5. Use the verifier to prove the absence of bugs.
    This step usually involves to write additional invariants.

These steps are explained below.

## Contents

[TOC]


# Implementation Language

The implementation language is syntactically close to the Scala programming language.
It is however a much simpler and minimalistic language.

A complete grammar for the language is given at the end of this document.
We introduce here only the statements and expressions that are not typically built into a programming language:

## Built-in datatypes

Repliss includes data types that can be used in the implementation and data types that are only used for specifying the application.
The following data types are built into Repliss.

 - `int`: Arbitrary precision integers
 - `boolean`: Boolean values
 
 Types for specification:
 
 - `invocationId`: An invocation of a procedure
 - `callId`: A call to the database
 - `transactionId`: A database transaction
 - `invocationInfo`: Information about an invocation (includes name of invoked procedure and the corresponding arguments)
 
 
  
 
 



## Statements

### Generating Unique Identifiers 

Repliss includes a builtin construct for generating new globally unique identifiers. 
This statement can be used with user-defined Id-types (see [Defining Types](#defining-types) below). 

    var m: MessageId
    m = new MessageId

### Atomic and Database Calls

The `call` statement is used to perform updates on the database.
Queries can be used like normal function calls, like `message_exists` in the example below.
An `atomic` block encloses a block of database calls and ensures that they are executed atomically.
This means that either all or none of the calls in the block can be visible in another procedure invocation.

    atomic {
        if (message_exists(message_id)) {
            call chat_remove(message_id)
            call message_delete(message_id)
        }
    }

### Assertions

An assert statement can be used to specify that an expression must always be true at a certain point in the program.
    
    assert n > 0


## Expressions




## Defining Types

<!--
- how to write procedures and define types
- short explaination of
    - statements: newIdStmt, crdtCall, atomicStmt, assertStmt
    - exprs: happened before/after, is visible, quantifier, (implication ==>)
- defining types
    - difference idtypes/types
    - data types / cases (with example)
    - built-in types: boolean, int, callId, invocationId, invocationInfo
-->


# Specification Language

<!--
- how to write invariants (invariant expr)
- invariants: have to hold, but not for uncommitted calls
-->

## Defining Database Structure

```
crdtDecl: 'crdt' keyDecl

keyDecl: ID ':' crdttype

crdttype:
      structcrdt
    | crdt

structcrdt: '{' keyDecl (',' keyDecl)* '}'

crdt: ID ('[' crdttype (','crdttype )* ']')?
```

<!--
- explain struct CRDT
- crdt ID from library
- see [example](#chat-crdt)
-->

### CRDT library

In order to aid in expressing the database schema of an application, Repliss provides the following operations and queries on CRDT objects -

#### RegisterCrdt(Register)
* operation assign - assigns the latest update.
* query get - returns the latest assignment.

#### MultiValueRegisterCrdt(multiValueRegister)
* operation assign - assigns the latest update. In case of concurrent updates, merges the concurrent assignments and stores them in the form of a list.
* query get - returns the list of latest concurrent assignments.
* query getFirst - returns the head of the list of assignments.
* query mv_contains - checks whether an update exists in the list of latest assignments.

#### SetAdd(Set_aw)/ SetRemove(Set_rw)
* operation add - adds a value to the Set.
* operation remove - removes a value from the Set.
* query contains - checks whether a value is present in the Set.

#### MapAddCrdt(Map_aw)/ MapRemoveCrdt(Map_rw)
* operation delete - removes a key and the corresponding value from the map.
* query exists - checks whether a key exists in the Map.

Operations and queries are derived from the value datatypes. The specification of map datatypes depends on the specification of the datatype for the values. Basically, a map inherits all operations from the value type and adds a key to the update operation, so that different entries can be updated independently. Map operations are illustrated in detail later.

### Defining custom CRDTs

Further CRDTs can be defined manually using the `operation` and `query` constructs in the language.


# Tutorial: Building a correct chat application

In this section, we use a chat application example to demonstrate how to use Repliss. A Repliss program consists of a set
of procedures and a definition of the database schema with relevant type definitions.

```
type ChatId
type UserId
idtype MessageId
type String
```
We can declare custom types using the keyword ```idtype``` and builtin types such as **String** using the ```type``` keyword.

```
crdt chat: Map_rw[ChatId, {
    messages: Set_rw[MessageId]
}]
```

#### Chat CRDT

Furthermore, the CRDT datatypes are declared using ```crdt``` keyword. In this example, we use a delete wins map(Map_rw) named ```chat``` to store the set of messages in each chat using the id of the message as the key of the map. The suffix rw is short for remove wins semantics. The specification of this semantics states that that an element x is in the CRDT, if there is an update add(x) and all remove(x) updates are causally followed by an add(x) update.

```
crdt message: Map_rw[MessageId, {
    author: multiValueRegister[UserId],
    content: multiValueRegister[String],
    chat: multiValueRegister[ChatId]
}]
```

We use another delete wins map(Map_rw) named ```message``` to store the data for each message. Each message has an author, a message content, and a reference to the chat the message belongs to. All three fields of the message use a multi-value register, which resolves concurrent assignments by keeping all concurrently assigned values.

```
def sendMessage(from: UserId, content: String, toC: ChatId): MessageId {
    var m: MessageId
    atomic {
        m = new MessageId                                                    #(1)
        call message_author_assign(m, from)                                  #(2)
        call message_content_assign(m, content)                              #(3)
        call message_chat_assign(m, toC)                                     #(4)
        call chat_messages_add(toC, m)                                       #(5)   
    }
    return m
}
```

Repliss procedures are implemented in a simple, imperative language which includes an atomic-statement to execute a block of operations in a database transaction. When sending a message via ```sendMessage```, we first create a new unique MessageId on line 1. Then we execute the calls to the database by initializing each of the fields in the message map. All of these operations are executed in the context of a transaction which is denoted by the atomic block. The Map operations for different value fields of the ```message``` map in ```sendMessage``` are derived by prefixing the key ```message``` to each of the value datatype as follows: ```author``` as ```message_author```, ```content``` as ```message_content``` and ```chat``` as ```message_chat```. Then, we call the multivalueregister operation ```assign``` to update the contents of the registers. Thus, the ```message``` map operations used in this procedure are ```message_author_assign```(line 2), ```message_content_assign```(line 3) and ```message_chat_assign```(line 4). On line 5,
we add the MessageId created in line 1 to the **Set_rw** datatype ```messages``` of ```chat``` map.

```
def editMessage(id: MessageId, newContent: String) {
    atomic {
        if (message_exists(id)) {                                            #(1)
            call message_content_assign(id, newContent)                      #(2)
        }
    }
}
```

The content of a message can be edited using the ```editMessage``` procedure. It assigns a new value to the content field of the given message in line 2, but checks first if the message exists to avoid reviving a message that has already been deleted (line 1). Similar to the ```sendMessage``` procedure, the operations are executed in an atomic block.

```
def deleteMessage(message_id: MessageId) {
    var c: ChatId
    atomic {
        if (message_exists(message_id)) {                                    #(1)
            c =  message_chat_getFirst(message_id)                           #(2)
            call chat_messages_remove(c, message_id)                         #(3)
            call message_delete(message_id)                                  #(4)
        }
    }
}
```

The procedure ```deleteMessage``` takes the identifier of a message, then looks up the chat the message belongs to in line 2, and finally removes the message from the set of messages of the ```chat``` and from the ```message``` map in line 3 and 4 respectively.

#### Verification using invariants

For the chat application, we want to verify that every MessageId occurring in the message-set of a chat also has the message data defined in the message map. We can specify this with the following invariant:

```
invariant(forall c: ChatId, m: MessageId ::
    chat_messages_contains(c, m) ==> message_exists(m))
```

Another invariant that can be checked for correctness is that there is always exactly one author per message which can be represented as follows:

```
invariant(forall m: MessageId, a1: UserId, a2: UserId ::
    message_exists(m) && message_author_mv_contains(m, a1) && message_author_mv_contains(m, a2) ==> a1 == a2 )
```

When an invariant fails, Repliss outputs a visualization of the failing execution.



# Language Reference

## Grammar

The complete grammar for Repliss is given below.
It is derived from the Antlr grammar used to generate the actual parser.
Ambiguities are resolved by rule priority -- higher productions have a higher priority. 

```
program: declaration* EOF;

declaration:
      procedure
    | typedecl
    | operationDecl
    | queryDecl
    | axiomDecl
    | invariant
    | crdtDecl
;


typedecl: ('idtype'|'type') ID ('=' dataTypeCase ('|' dataTypeCase)*)?;

dataTypeCase: ID '(' (variable (',' variable)*)? ')';

operationDecl: 'operation' ID '(' (variable (',' variable)*)? ')';

queryDecl: ('@inline')? 'query' ID '(' (variable (',' variable)*)? ')' ':' type
    ('=' expr | 'ensures' expr)?;

axiomDecl: 'axiom' expr;

procedure: 'def' ID '(' (variable (',' variable)*)? ')' (':' type)? stmt;

crdtDecl: 'crdt' keyDecl;

variable: ID ':' type;

keyDecl: ID ':' crdttype;

type: ID;

crdttype: 
      structcrdt
    | crdt
    ;

structcrdt: '{' keyDecl (',' keyDecl)* '}';

crdt: ID ('[' crdttype (','crdttype )* ']')?;

stmt:
      blockStmt
    | atomicStmt
    | localVar
    | ifStmt
    | matchStmt
    | crdtCall
    | assignment
    | assertStmt
    | newIdStmt
    | returnStmt
    ;

blockStmt: '{' stmt* '}';

assertStmt: 'assert' expr ;

atomicStmt: 'atomic' stmt;

localVar: 'var' variable;

ifStmt: 'if' '(' expr ')' stmt ('else' stmt)?;

matchStmt: expr 'match' '{' matchCase* '}';

matchCase: 'case' expr '=>' stmt*;

crdtCall: 'call' functionCall;

assignment: ID '=' expr;

newIdStmt: ID '=' 'new' ID;

returnStmt: 'return' expr (assertStmt)*;

expr:
      ID
    | ('true'|'false')
    | INT
    | expr '.' ID
    | '!' expr
    | expr 'is' 'visible'
    | expr 'happened' ('before'|'after') expr
    | expr ('*'|'/'|'%') expr
    | expr ('+'|'-') expr
    | expr ('<'|'<='|'>'|'>=') expr
    | expr ('=='|'!=') expr
    | expr '&&' expr
    | expr '||' expr
    | expr '==>' expr
    | quantifierExpr
    | functionCall
    | '(' expr ')'
    ;

quantifierExpr: ('forall'|'exists') variable (',' variable)* '::' expr;

functionCall: ID '(' (expr (',' expr)*)? ')';

invariant: 'free'? 'invariant' (ID ':')? expr;
```