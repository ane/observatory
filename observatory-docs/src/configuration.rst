.. _configuration:

Configuration
=============

Observatory is configured with a manifest. Its syntax is a simple `TOML
<https://github.com/toml-lang/toml>`_ syntax. Here is an example.

.. _manifest:

Manifest
--------

The Observatory configuration lives in the manifest file, usually a file called
``config.toml``. It will contain some useful variables, unrelated to the content of the system
itself.

For Observatory to function, we need the following:

- An input source for events, e.g. a HTTP server, or a message queue
- An output interface, e.g. a HTTP server

This is an example configuration.

.. code-block:: none

   name = "My Observatory"
   description = "blah blah"

   [input.http]
   interface = "localhost"
   port = "8080"
   path = "/events"

   [input.rabbitmq]
   address = "localhost"
   username = "rabbit"
   password = "asdfasdf"
   queue = "observatory.input"

   [output.http]
   interface = "localhost"
   port = "9090"

This will start a HTTP server that listens for incoming event packets in ``/events`` as POST
requests and a RabbitMQ queue binding for the queue ``observatory.input``. It will also start a HTTP
server for the front-end, providing a REST API front-end.

Observatory is meant to be agnostic in terms of input sources, it won't be tied down to any central
mechanism, but initially it will support a HTTP and a MQ of some kind (probably RabbitMQ).

Do note that the inputs and outputs are entirely optional, but a configuration with either no inputs
or no outputs is rather silly.

After this configuration, we can configure streams.

Streams
-------

A part of the system we wish to monitor for data flows is called a *stream*. This is an example:

.. code-block:: none

   [[stream]]
   name = "my-sample-stream"
   nodes = ["web-server", "event-processor", "journal", "database"]

A configuration like this is absolutely minimal. Once Observatory is launched for the first time, it
will look like this:

.. graphviz::
   
   digraph foo {
     A[label="web-server"];
     B[label="event-processor"];
     C[label="journal"];
     D[label="database"];
   }

This stream is still in an *unobserved* state, because we haven't seen any data flowing in it yet.

Such a minimal configuration will result in an edge getting rendered when there is traffic, but the
edge will never disappear, since Observatory has understood that traffic has occurred `at least
once`. So, if ``web-server`` gets a request and sends ``(web-server,foo,TIME1)`` to Observatory, nothing
will happen, except that web-server is rendered as having sent traffic:

.. graphviz::
   :caption: *note the teal color around web-server, indicating an unknown downstream status*
   
   digraph foo {
     A[label="web-server",color="#00AAAA"];
     B[label="event-processor"];
     C[label="journal"];
     D[label="database"];
   }

Lets assume traffic *has* occurred, whereby ``event-processor`` has received that message from
upstream, with the tracing token ``foo``, sending ``(event-processor,foo,TIME2)``. Observatory does
some internal calculations, notes the equal tokens and successive time stamps, giving us this:

.. graphviz::
   
   digraph foo {
     rankdir=LR;
     A[label="web-server"];
     B[label="event-processor"];
     C[label="journal"];
     D[label="database"];

     A->B[label="OK(seen=1)",color="#00AA00"];
   }

Lets assume the same thing happens for the other nodes, ``journal`` and ``database``, giving this:

.. graphviz::
   
   digraph foo {
     rankdir=LR;
     A[label="web-server"];
     B[label="event-processor"];
     C[label="journal"];
     D[label="database"];

     A->B[label="OK(seen=1)",color="#00AA00"];
     B->C[label="OK(seen=1)",color="#00AA00"];
     B->D[label="OK(seen=1)",color="#00AA00"];
   }
   
This information tells us traffic has occured *once* between all the nodes. This configuration is
useless! Let's configure some *edges*.

Edges
-----

An **edge** means communication between two nodes. It is configured thusly:

.. code-block:: none

   [[edge]]
   name = "http traffic sent to processor"
   from = "web-server"
   to = "event-processor"

An edge without **health checks** is equal to not having been defined, because this doesn't really
tell us what to look at. So for this we need health checks.

Health checks
~~~~~~~~~~~~~


Health checking a system can either be *temporal* or *countable*.  Temporal health checking means
that data must flow in the stream under a certain time period. A quantitative health check on the
other hand means that a certain amount of data must flow in the stream for every request.

Temporal
********

This is a *temporal health check*. A temporal health check can either be *edge-based* or *total*. If
we have an edge ``(web-server,event-processor)``, we can define that if ``web-server`` receives a
request, Journal must correlate it within ``N`` seconds (or any other time unit).

The syntax for a temporal health check is this:

.. code-block:: none

   [[edge]]
   name = "http traffic sent to processor"
   from = "web-server"
   to = "event-processor"
   
     [edge.check]
     kind = "time"
     expect = 1
     within = 500
     unit = msec

Once data starts flowing, we get an output like this:

.. graphviz::
   
   digraph foo {
     rankdir=LR;
     A[label="web-server"];
     B[label="event-processor"];
  
     A->B[color="#00AA00",label="OK(seen=3,expect=1,within=10,unit=sec)"];
   }

A configuration like this can be defined between any two nodes in the graph, and there can be any
number of them. The ``from`` and ``to`` fields are limited to the configured nodes.

.. note:: 

   The following inputs are accepted for time units:

   =========== ===========
   Unit        Accepted inputs
   =========== ===========
   year        year, y
   month       month, mo
   day         day, d
   hour        hour, h
   minute      minute, min, m
   second      second, sec, s
   millisecond millisecond, msec, millis, ms
   microsecond microsecond, usec, us, μs
   nanosecond  nanosecond, nsec, ns
   =========== ===========

Countable
*********

A countable health check monitors a correlation between elemenst.  Let's say for every three
``event-processor`` events, there must be an event registered from it to ``database``, within a
certain latency. So if the fourth request occurs, and we haven't received the request from
``event-processor``, we mark the stream as ``NOK``. So, defining a ``1:3`` ratio for ``event-processor`` and
``database`` marks this ratio thusly:

.. code-block:: none

   [[edge]]
   name = "event-processor to database"
   from = "event-processor"
   to = "database"
   
     [edge.check]
     kind = "countable"
     expect = 1
     for = 3
     latency = 500
     unit = msec

This is how the edge in the example figure was configured, in :ref:`the example <sample>`:

.. _example_config:

.. code-block:: none

   [[stream]]
   name = "my-sample-system"
   nodes = ["web-server", "event-processor", "journal", "database"]

     [[edge]]
     name = "web server to event processor"
     from = "web-server"
     to = "event-processor"
     
       [edge.check]
       kind = "time"
       expect = 1
       within = 10
       unit = sec

     [[edge]]
     name = "event processor to database"
     from = "event-processor"
     to = "database"
     
       [edge.check]
       kind = "countable"
       expect = 1
       for = 3
       latency = 500
       unit = msec

     [[edge]]
     name = "event-processor to journal"
     from = "event-processor"
     to = "journal"
     
       [edge.check]
       kind = "time"
       expect = 1
       within = 500
       unit = msec
