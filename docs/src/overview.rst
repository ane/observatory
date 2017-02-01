.. _overview:

Overview & Glossary
-------------------

The mechanism is simple. Observatory listens to an input for events coming from different
services. A stream will be initiated by a root node sending a message containing a unique **tracing
ID** and its own identifier. Once that node passes information downstream, e.g., to another server,
it does the same.

Streams
```````

Initially, we configure a *stream*. A stream is a directed acyclic graph (DAG) that models the flow
of information in a distributed system. So if our system consists of two services, ``App`` and
``Journal``, it will initially look like this:

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

Nodes and edges
```````````````
   
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
     "web-server" -> "event-processor"[label="OK(seen=1)", color="#00AA00"];
   }

After this, the stream is considered *observed*. Without any
configuration, Observatory will render this graph forever. This is quite
useless, so we can configure what it means for a stream to be *healthy*. This is described in
:ref:`configuration`.



