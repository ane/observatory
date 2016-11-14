.. _introduction:

==============
 Introduction
==============

What is it?
===========

Observatory is a system status service, displaying relevant system information, serving a purpose
similar to `Thruk <https://www.thruk.org/>`_ or `Riemann <http://riemann.io/>`_.

Observatory is a **visual information program**. Observatory displays **visually** the flow of
information in a distributed system rendered as a **graph**. Under the hood, it is based on *tracing
tokens*, sent from each node, to a central Observatory server. These tokens are used to render a
real-time graph that can be used to quickly identify the status of information flows.


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
  
  
      A->B[color="#00AA00",label="OK(seen=3),\nCheck(expect=1,within=10,unit=sec)"];
      B->D[color="#AAAA00",label="WARN(seen=1),\nCheck=(expect=1,for=3)"];
      B->C[color="#AA0000",label="NOK(seen=0),\nCheck=(expect=1,within=500,unit=msec)"];
    }

The system in it has four nodes: ``web-server``, a root node, ``event-processor``, ``journal``, and
``database``. For every ``web-server`` request, we expect ``event-processor`` to have reacted within
10 seconds. The ``seen`` value of 3 means this has happened three times. For every
``event-processor`` request, we expect it at ``journal`` within 500 milliseconds. These are
*temporal checks*. These are based on our own observations of the system. For the ``database`` node
we don't know, so we've set up a *quantitative* check: for every 3 requests from its parent
(``event-processor``), we expect ``1`` request there. This check is barely at the threshold, so it
has a warning status.

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
