.. _monitor:

==============
User Interface
==============

The user interface of Observatory is divided into two parts: input and output. For input, we read
events from various sourced, configured in the :ref:`manifest`. The output part is the front-end,
the graph visualization.

Input: Sending events
---------------------

Sending input to Observatory is easy. At this stage, you just send an event via HTTP.

Here is a sample payload:

.. code-block:: json

   {
      "origin": "web-server",
      "tracing": "1234abcd",
      "timestamp": "2016-11-13T22:00:00+02:00"
   }

The fields are the following:

:origin: The application where the event occurred
:tracing: The tracing token **unique for this event**
:timestamp: A `ISO 8601 <https://en.wikipedia.org/wiki/ISO_8601>`_ date when the event occurred.

This payload can be sent to Observatory with a simple POST request to the ``/events`` endpoint.

.. code-block:: none

   $ curl -X POST -H 'Content-type: application/json' -d@event.json http://observatory/events

   HTTP/1.1 202 Accepted
   Server: Observatory 0.1
   Date: Sun, 13 Nov 2016 21:46:08 GMT
   Content-Type: application/json; charset=utf-8


Output: Getting status
----------------------

The *monitor* is the Observatory front-end. As we saw in the :ref:`example graph <sample>`, at this
point, the interface is just a constantly refreshing Graphviz rendering of the graph.

Internally, Observatory is accessed with a REST API. If we configure a system called
``my-sample-system``, we get its graph as JSON like this:

.. code-block:: json

   $ curl -i http://observatory/streams/my-sample-system
   HTTP/1.1 200 OK
   Server: Observatory 0.1
   Date: Sun, 13 Nov 2016 21:46:08 GMT
   Content-Type: application/json; charset=utf-8

   {
     "name": "my-sample-system",
     "description": "blah blah",
     "graph": [
       {
         "from": "web-server",
         "to": "event-processor",
         "status": "OK",
         "check": {
           "kind": "time",
           "seen": 1,
           "expect": 1,
           "within": 10,
           "unit": "sec"
         }
       },
       {
         "from": "event-processor",
         "to": "journal",
         "status": "NOK",
         "check": {
           "kind": "time",
           "seen": 0,
           "expect": 1,
           "within": 500,
           "unit": "msec"
         }
       },
       {
         "from": "event-processor",
         "to": "database",
         "status": "WARN",
         "check": {
           "kind": "countable",
           "seen": 1,
           "expect": 1,
           "for": 3,
           "latency": 500,
           "unit": "msec"
         }
       }
     ]
   }

This can be then rendered in the front-end, which is just a dead simple HTTP server rendering this
graph to a Graphviz page, with a meta refresh directive of 1 second.
