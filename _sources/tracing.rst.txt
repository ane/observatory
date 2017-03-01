============
 Monitoring
============

Observatory monitoring is based on two discrete steps: **tracing** and **correlation**. Tracing is
done by nodes. Correlation is internal to Observatory, it's how it couples individual events
together, using the tracing tokens.

Tracing
=======

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

How all of this works can be illustrated with a sequence diagram:

.. uml:: 

   participant "Client" as C
   participant "Web Service" as WS
   participant "Event Processor" as EP
   participant "Observatory" as O #00FF88

   activate C
   C -> WS: ""GET /foo""
   activate WS
   WS --> O: ""POST (web-server, **abcd1234**, T1)""
   note left: async!
   activate O

   WS -> EP: POST /events ...\nX-Tracing-ID: abcd1234
   note left: tracing id\nin headers
   activate EP
   EP --> O: ""POST (event-processor, **abcd1234**, T2)""
   note left: async + upstream id

   deactivate O

   WS <- EP: ""HTTP 201 Created""
   deactivate EP
   C <- WS: ""HTTP 200 OK""
   deactivate WS
   deactivate C

.. _async_warning:

.. warning:: Asynchronous sending

Remember asynchrony
-------------------

In order to have minimum overhead, it's important to not block when informing Observatory. In
Pythonish pseudo-code, we could do it like this, using `grequests
<https://github.com/kennethreitz/grequests>`_:

.. code:: python

   time = datetime.now()
   token_uuid = uuid.uuid()
   event = { 'node': 'web_server', 'timestamp': time.isoformat(), 'tracing_id': str(token_uuid) }

   # non-blocking!
   grequests.post('http://observatory:1234/events', json.dumps(event))

   # ... blocking code ...

If you do this synchronously, you will delay the blocking code segment, but if you use asynchronous
HTTP requests, the code will resume.

Correlation
===========
   
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

Note that the pass label says 100% because we haven't described a check span: how many successive
events should we remember before determining edge health.

Observation windows
-------------------

Defining **observation windows** is essentially done by summing the different thresholds. If you define a
check which has a time window of 150ms, a 85 for ``OK``, 10 for ``FAIL``, 5 for ``WARN``, the
following list is processed in order:

* **85 or more** events are completed within 150ms, the edge is considered ``OK``
* **10 or more** events do not complete within 150ms, the edge is considered ``FAIL``
* **5 or more** events do not complete within 150s, the edge is considered ``WARN``

The checks are done in reverse order, i.e. ``WARN`` thresholds are checked first, then ``FAIL``,
last ``OK``.

.. graphviz::
   :caption: In an observation window where the sum of thresholds is five, at the arrival of the
             sixth event (e6), event e1 is truncated out of the window.

   graph {
     rankdir=LR;

     edge[style=invis];
     subgraph cluster1 {
       label=<<b>observation window</b><br/>time &rarr;>;
       node [style=filled,color="#5D8AA8", fillcolor="#00AA00"];
       e2; e4; e5; e6;
       node [style=filled,color="#5D8AA8", fillcolor="#AA0000"];
       e2 -- e3 -- e4 -- e5 -- e6
     }

     e7[label=<<i>future<br/>event</i>>]
     e1 -- e2
     e6 -- e7

   }

All of these thresholds sum to 100. This means that Observatory maintains an *observation window* of
100 events in memory. Once the 101st event comes, the list is truncated from the start. The window
size is simply the sum of all the thresholds. 

When enough observations are available, the stream is considered *observed*. This is when the edge
status is reliable.

Reliability considerations
--------------------------

The observation window is based on strict truncation. The larger the window, the more reliable the
observation, but it will be slow to react to changes.



