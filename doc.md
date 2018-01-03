Repliss is a verification system for checking correctness of applications built on top of weakly consistent distributed databases such as [Antidote](http://syncfree.github.io/antidote/).

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

In this section, we use a chat application example to demonstrate how to use Repliss. A Repliss program consists of a set
of procedures and a definition of the database schema with relevant type definitions.

```python
type ChatId
type UserId
idtype MessageId
type String
```
We can declare custom types using the keyword ```idtype``` and builtin types such as **String** using the ```type``` keyword.

crdt chat: Map_rw[ChatId, {
    messages: Set_rw[MessageId]
}]

Furthermore, the CRDT datatypes are declared using ```crdt``` keyword. In this example, we use a delete wins map(Map_rw) named ```chat``` to store the set of messages in each chat using the id of the message as the key of the map. The suffix rw is short for remove wins semantics. The specification of this semantics states that that an element x is in the CRDT, if there is an update add(x) and all remove(x) updates are causally followed by an add(x) update.

crdt message: Map_rw[MessageId, {
    author: multiValueRegister[UserId],
    content: multiValueRegister[String],
    chat: multiValueRegister[ChatId]
}]

We use another delete wins map(Map_rw) named ```message``` to store the data for each message. Each message has an author, a message content, and a reference to the chat the message belongs to. All three fields of the message use a multi-value register, which resolves concurrent assignments by keeping all concurrently assigned values.

```python
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
The web interface for Repliss is available at https://softech.cs.uni-kl.de/repliss/.  
