.. _overview:

The Basics
-------------------

The mechanism is simple. Observatory listens to an input for events coming from different
services. Each individual service is called a *node*. A group of nodes is called a *stream*. To form
these groups, nodes are connected via *edges*. An edge is an implied connection between two nodes:
it can be, e.g., a web server talking to a logging mechanism, or an event system pushing events
downstream to a message queue.

The *movement* on a stream will be initiated by the root node. The message, which is said to pass
throughout a stream, is tagged with a **tracing ID**. This way, the message can be tracked as it
moves down the stream. All of these pieces are called *elements*, while checks are *behaviours*.

Elements
~~~~~~~~

Nodes
#####

.. graphviz::

   digraph {
     "web-server";
   }

A node is an individual component in the stream. It can be a web service or any program that reads
input from one place and produces output to another.

Edges
#####
.. graphviz::

   digraph {
     rankdir=LR;
     "web-server" -> "event-processor";
   }

An edge is a *proof of connectivity* between two nodes. You can define, e.g., that a web service `A`
talks to web service `B` and by observing traffic between these two nodes, you can identify some
mechanisms for checking that traffic is OK between these nodes.

Streams
#######

.. graphviz::

   digraph {
     rankdir=LR;
     "web-server" -> "event-processor" -> "data-warehouse";
   }

A stream is a group of connected edges. It represents the movement of individual messages within an
observed system. The above figure illustrates, in very broad terms, that each message from
``web-server`` will move to ``event-processor`` and from there to ``data-warehouse``.

Checks
######

Monitoring stream traffic is of little interest if you don't define *how* traffic should move. For
example, from observational data (by analyzing logs, etc.) we can say that requests from
``web-server`` should reach ``event-processor`` within 300ms. We define that the edge traffic is "OK"
when, for a hundred requests, or any such number, *eighty* must make it to ``event-processor``. in this
time. If this doesn't happen, we say that there is something wrong in the connection.

The above constitutes a *temporal check*. 

In practice
~~~~~~~~~~~

Initially, we configure a *stream*. A stream is a directed acyclic graph (DAG) that models the flow
of information in a distributed system. So if our system consists of two services, ``web-service`` and
``event-processor``, it will initially look like this:

.. graphviz::

   digraph {
     rankdir=TB;
     "web-server"; "event-processor";
   }


We model this in the configuration as a stream in which the elements are defined as ``["web-server",
"event-processor"]``. This is configured using the following syntax:

.. code-block:: none
             
   [[stream]]
   name = "my-example-stream"
   nodes = ["web-server", "event-processor"]

Streams are composed of nodes and edges. A node is identified by a unique UTF-8 string. An edge is a
pair between two distinct nodes. Defining an edge means configuring the rate of monitored
information flow.

Correlation
###########

How all of this works can be illustrated with a sequence diagram:

.. uml:: 

   participant "Client" as C
   participant "Web Service" as WS
   participant "Event Processor" as EP
   participant "Observatory" as O #00FF88

   activate C
   C -> WS: ""GET /foo""
   activate WS
   group Asynchronously: generate tracing id
       WS --> O: ""(web-server, **abcd1234**, T1)""
       activate O
   end

   WS -> EP: POST /events ...\nX-Tracing-ID: abcd1234
   note left: tracing id\nin headers
   activate EP
   group Asynchronously: propagate downstream id
       EP --> O: ""(event-processor, **abcd1234**, T2)""
   end

   deactivate O

   WS <- EP: ""HTTP 201 Created""
   deactivate EP
   C <- WS: ""HTTP 200 OK""
   deactivate WS
   deactivate C
   
So, node ``web-server`` receives a HTTP request. A unique id ``abcd1234`` is generated. Webserver
sends the information packet ``(web-server, abcd1234, T1)`` to Observatory, where ``T1`` is the current
timestamp in ISO8601 format. App then sends that request downstream to a journal system, passing the
tracing ID in a HTTP header, ``X-Tracing-ID: abcd1234`` in the HTTP request header. The ``event-processor``
system reads this header and correlates this packet by sending ``(event-processor, abcd1234, T2)`` to
observatory. Now, observatory sees that these elements are part of a stream---because they share the
tracing token---so it starts observing it, and because T2 > T1, it will understand that information
is flowing from ``web-server`` to ``event-processor``. Now our DAG looks like this:

.. graphviz::

   digraph {
     rankdir=LR;
     "web-server" -> "event-processor"[label="OK(pass=1/1 100%)", color="#00AA00"];
   }

After this, the stream is considered *observed*. Without any
configuration, Observatory will render this graph forever. This is quite
useless, so we can configure what it means for a stream to be *healthy*. This is described in
:ref:`configuration`.



