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

.. code-block:: ini

   name = "My Observatory"
   description = "blah blah"

   # create a HTTP server
   [input.http]
   interface = "localhost"
   port = "8080"
   path = "/events"

   # listen from rabbitmq exchange 'observatory'
   # with routing key 'observatory.input'
   [input.rabbitmq]
   address = "localhost"
   port = 5672
   username = "rabbit"
   password = "asdfasdf"
   exchange = "observatory"
   exchangeType = "topic"
   routingKey = "observatory.input"

   # read from ActiveMQ using Camel connector
   [input.camel]
   url = "activemq://localhost:8888/observatory.in"

   # frontend HTTP server
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

.. code-block:: ini

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

     A->B[label="OK(pass=1/1 100%)",color="#00AA00"];
   }

Lets assume the same thing happens for the other nodes, ``journal`` and ``database``, giving this:

.. graphviz::
   
   digraph foo {
     rankdir=LR;
     A[label="web-server"];
     B[label="event-processor"];
     C[label="journal"];
     D[label="database"];

     A->B[label="OK(pass=1/1 100%)",color="#00AA00"];
     B->C[label="OK(pass=1/1 100%)",color="#00AA00"];
     B->D[label="OK(pass=1/1 100%)",color="#00AA00"];
   }
   
This information tells us traffic has occured *once* between all the nodes. This configuration is
useless! Let's configure some *edges*.

Edges
-----

An **edge** means communication between two nodes. It is configured thusly:

.. code-block:: ini

   [[stream.edges]]
   name = "http traffic sent to processor"
   from = "web-server"
   to = "event-processor"

:name: A description of the edge
:from: Edge start node ID
:to: Edge destination node ID

The node IDs are matched against sent tracing information. An edge like above will define two nodes:
``web-server`` and ``event-processor``. Node ID matching in tracing is **case sensitive**.

You may guess that an edge without **health checks** is useless, because this doesn't really tell us
what to look at. So for this we need health checks.

Checks
------

Health checking means that data must flow in the stream under a certain time period. Health checks
generally possess a *kind* and *thresholds*. The kind is what metrics are used to define the health
check, and the threshold defines how many times the check must succeed or fail until a change is
triggered.

If we have an edge ``(web-server,event-processor)``, we can define that if ``web-server`` receives a
request, Journal must correlate it within ``N`` seconds (or any other time unit).

The syntax for a temporal health check is this:

.. code-block:: ini

   [[stream.edges]]
   name = "http traffic sent to processor"
   from = "web-server"
   to = "event-processor"
   
     [stream.edges.check]
     kind = "time"
     within = 500
     unit = msec
     ok = 3         
     warn = 0
     nok = 1

:within: The time window as a number
:unit: The time unit (see :ref:`units`)
:ok: *Optional* How many times the check must succeed before setting OK status (default: 1)
:nok: *Optional* How many failures we allow before setting NOK status (default: 0)
:warn: *Optional* How many failures we allow before setting WARN status (default: 0). **Note:** if
       you set this field, Observatory will slap you if you set ``warn > nok``.
     
Once data starts flowing, and we've received four requests, of which all have passed, we get an output like this:

.. graphviz::
   
   digraph foo {
     rankdir=LR;
     A[label="web-server"];
     B[label="event-processor"];
  
     A->B[color="#00AA00",label="OK(pass=4/4 100%)\nCheck(ok=3,warn=0,fail=1)"];
   }

A configuration like this can be defined between any two nodes in the graph, and there can be any
number of them. The ``from`` and ``to`` fields are limited to the configured nodes.

.. _units:

.. table:: Accepted time units

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

Example
-------

This is how the edge in the example figure was configured, in :ref:`the example <sample>`:

.. _example_config:

.. code-block:: ini

   [[stream]]
   name = "my-sample-system"
   nodes = ["web-server", "event-processor", "journal", "database"]
   
       [[stream.edges]]
       name = "web server to event processor"
       from = "web-server"
       to = "event-processor"
   
           [stream.edges.check]
           within = 10
           unit = "sec"
           min = 3
           warn = 0
           fail = 0
   
       [[stream.edges]]
       name = "event-processor to database"
       from = "event-processor"
       to = "database"
   
           [stream.edges.check]
           within = 500
           unit = "ms"
           min = 3
           warn = 1
           fail = 2
   
       [[stream.edges]]
       name = "event-processor to journal"
       from = "event-processor"
       to = "journal"
   
           [stream.edges.check]
           within = 500
           unit = "ms"
           min = 3
           warn = 0
           fail = 0

If you can't make sense of `TOML`_, here's the equivalent JSON:

.. code-block:: none

   "stream": [{
       "edges": [
         {
           "check": {
             "fail": 0,
             "min": 3,
             "unit": "sec",
             "warn": 0,
             "within": 10
           },
           "from": "web-server",
           "name": "web server to event processor",
           "to": "event-processor"
         },
         {
           "check": {
             "fail": 2,
             "min": 3,
             "unit": "ms",
             "warn": 1,
             "within": 500
           },
           "from": "event-processor",
           "name": "event-processor to database",
           "to": "database"
         },
         {
           "check": {
             "fail": 0,
             "min": 3,
             "unit": "ms",
             "warn": 0,
             "within": 500
           },
           "from": "event-processor",
           "name": "event-processor to journal",
           "to": "journal"
         }
       ],
       "name": "my-sample-system",
       "nodes": [
         "web-server",
         "event-processor",
         "journal",
         "database"
       ]
     }
   ]}
