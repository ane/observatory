.. _introduction:

==============
 Introduction
==============

What is it?
===========

**Observatory** is a distributed tracing system that can be used to monitor traffic latencies between
networked services.

Observatory is similar to `Zipkin <https://zipkin.io>`_ or `Kamon <http://kamon.io>`_.

Observatory aggregates tracing tokens received from various inputs. These tracing tokens are used to
render an image as seen :ref:`below <sample>`.


Why would I use it?
===================

They say a picture is equal to a thousand words. Observatory displays the status of a distributed
system as a visual graph. Observatory can be a simple auto-refreshing web-page that looks like
this:

.. _sample:

.. graphviz:: 
    :caption: **A prototype of Observatory's user interface.** Yes, at this point, it's just a
              Graphviz graph.

    digraph Example {
      A[label="web-server"];
      B[label="event-processor"];
      C[label="journal"];
      D[label="database"];
  
  
      A->B[color="#00AA00",label="OK(pass=3/3 100%),\nCheck(within=10s,ok=3,warn=0,fail=0)"];
      B->D[color="#AAAA00",label="WARN(pass=4/5 80%),\nCheck=(within=500ms,ok=3,warn=1,fail=2)"];
      B->C[color="#AA0000",label="NOK(pass=1/4 25%),\nCheck=(within=1000ms,ok=3,warn=1,fail=0)"];
    }

The system in it has four nodes: ``web-server``, a root node, ``event-processor``, ``journal``, and
``database``. For every ``web-server`` request, we expect ``event-processor`` to have reacted within
10 seconds. None of the observed requests have failed (i.e. taken more than 10s), so the status is
marked as OK. We've defined a window of 3 requests out of which three must succeed in order to mark
as OK. Since ``warn=0`` and ``fail=0`` should any of the requests fail the edge will automatically
be marked as ``NOK`` (fail trumps warn).

For a ``event-processor`` → ``database`` message, we expect it to react within 500ms. The size of
the observation window is 5 requests (3+1+2). For an OK status, 3 requests are required to succeed
within this window. One request in this window has failed. Since ``warn`` is set to 1, but ``fail``
is at 2, we mark the edge as ``WARN``. 

``event-processor`` → ``journal`` is much stricter: no request in the window (3+1+0 = 4) must fail
(``fail = 0``). As can be seen, only one out of four passed the check, so it is marked as ``FAIL``.


Think of it as what a logging server contains, but relevant parts visualized, and the
visualization can be customized. 

Who is it for?
==============
Sysadmins, developers, anyone who is interested in the status of information flows in a
distributed system they are developing or monitoring. Observatory can be used to get a
"high-altitude" view, using which you can identify the likely sources of problems. The typical use
case scenario is this:

1. Some component in a system is failing -- not receiving messages, data isn't showing up, etc.
2. The cause is identified, messages are missing. Where's the broken link?
3. Check Observatory if anything is broken.
4. Observatory tells us the connection between ``event-processor`` and ``journal`` is broken, no events have been recorded for the requests the ``web-server`` component received.
5. Investigate the problem between ``event-processor`` and ``journal``.

How does it work?
=================

Each component is programmed to log its messages to Observatory, associating a unique tracing token
to a message. Whenever an event occurs in a component, the component tells Observatory about it,
associating a unique tracing token. The component passes this event downstream. When the downstream
component(s) have received the events, they tell Observatory about it, and Observatory correlates
the tracing tokens and timestamps. This lets Observatory track the message rates.

The *quality* of message rates is configured using *health checks*. These are customizable and can
be based on time or count. For example, we can require a system to have a message pass through it
within 1 second. Or we can say that whenever one component generates five events, `x` events must
occur in a downstream component.

If the system deviates from these health checks, the graph indicates this visually, with a red
arrow between the nodes. For more information, see :ref:`overview`.

How do I integrate it to my system?
===================================

You **pre-configure** the information flow with a *manifest*, which at its simplests, is just a list of components in the system. Each component has
an automated mechanism, e.g., a request middleware, that performs the message sending to
Observatory, which listens to various sources of input (HTTP, MQ, log files). 
Manifest configuration is done using a simple TOML  syntax, see 

For more information, see :ref:`configuration`.

Summary
=======

Observatory is

- A distributed system monitoring tool that measures message rates in the system and displays the
  rates visually as a graph
- Its health check mechanism can also be 
- A tool for developers and sysadmins that care about the above information
- Integrated into your system using plugins that tell Observatory about messages, enabling
  measurement (see :ref:`overview`)
- Configured using a simple TOML syntax (see :ref:`configuration`) and run as a stand-alone server
  program with an optional web front-end (the graph)

Now head over to :ref:`overview` to learn more!
