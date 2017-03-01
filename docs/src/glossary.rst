.. _overview:

========
Glossary
========

The mechanism is simple. Observatory listens to an input for events coming from different
services. Each individual service is called a *node*. A group of nodes is called a *stream*. To form
these groups, nodes are connected via *edges*. An edge is an implied connection between two nodes:
it can be, e.g., a web server talking to a logging mechanism, or an event system pushing events
downstream to a message queue.

The *movement* on a stream will be initiated by the root node. The message, which is said to pass
throughout a stream, is tagged with a **tracing ID**. This way, the message can be tracked as it
moves down the stream. All of these pieces are called *elements*, while checks are *behaviours*.

Below is a helpful glossary of the terms.

.. glossary::

   :ref:`node`
       An individual node in the network, e.g., a web service.

   :ref:`edge`
       A connection between two nodes, e.g., from a web service to a database, or from a database to
       a logging system

   :ref:`edge`
       A group of connected edges that define an abstract data stream, e.g. from a web service to a
       database to a logging system

   :ref:`health_check`
       How to identify whether individual edge traffic is healthy or not

   :ref:`event`
       Information used to tell Observatory traffic has occurred

   :ref:`tracing_token`
       A unique tracing id that can be used throughout the stream to identify data moving accross
       the stream

.. _node:

Node
++++

.. graphviz::

   digraph {
     "web-server";
   }

A node is an individual component in the stream. It can be a web service or any program that reads
input from one place and produces output to another.

Attributes
@@@@@@@@@@

:name: A unique node identifier

.. _edge:

Edge
++++

.. graphviz::

   digraph {
     rankdir=LR;
     "web-server" -> "event-processor";
   }
   
An edge is a *proof of connectivity* between two nodes. You can define, e.g., that a web service `A`
talks to web service `B` and by observing traffic between these two nodes, you can identify some

Attributes
@@@@@@@@@@

:name: The name of this connection (e.g. "web server to database")
:from: The source node of this edge
:to: The destination node of this edge

.. _stream:

Stream
++++++

.. graphviz::

   digraph {
     rankdir=LR;
     "web-server" -> "event-processor" -> "data-warehouse";
   }

A stream is a group of connected edges, a pipeline of nodes. It represents the movement of
individual messages within an observed system. The above figure illustrates, in very broad terms,
that each message from ``web-server`` will move to ``event-processor`` and from there to
``data-warehouse``.

Attributes
@@@@@@@@@@

:name: The description of the stream
:node: The nodes inside the stream
:edges: The edges of the stream

.. _health_check:

Health check
++++++++++++

Monitoring stream traffic is of little interest if you don't define *how* traffic should move. For
example, from observational data (by analyzing logs, etc.) we can say that requests from
``web-server`` should reach ``event-processor`` within 300ms. We define that the edge traffic is "OK"
when, for a hundred requests, or any such number, *eighty* must make it to ``event-processor``. in this
time. If this doesn't happen, we say that there is something wrong in the connection.

A health check defines three thresholds: the OK threshold, the WARN threshold, and the FAIL
threshold. An individual observation window is the sum of the thresholds. If you define 3 for all
thresholds, this would create a sliding observation window of 9 events.**

**Note**: You must have `OK >= FAIL >= WARN`, otherwise the observations don't make sense.

Attributes
@@@@@@@@@@

:within: The time window for the edge 
:unit: The time unit for the window (see :ref:`units`)
:ok: Minimum events that should pass in order to trigger OK
:warn: Minimum events that should pass in order to trigger OK 
:fail: Minimum events that should pass in order to trigger fail 

.. _event:

Event
+++++

An event is a signal to Observatory that a node has registered traffic. 

Attributes
@@@@@@@@@@

:timestamp: A RFC3339 date-time or 64-bit integer in microseconds from Unix epoch time
:node: The node from which the event is sent
:tracing: The tracing token of this event

.. _tracing_token:

Tracing token
+++++++++++++

:format: A unique string, preferably a `UUID <https://en.wikipedia.org/wiki/Universally_unique_identifier>`_.

The tracing token is used to identify the movement of a message. When the message originates at the
*root node*, the root node attaches a unique tracing token to the message. When that message is
passed to the next node, e.g., in a HTTP/MQ header, the node uses that tracing token to inform
observatory. That way, Observatory can identify that messages are moving successfully.




