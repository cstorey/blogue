---
title: Adventures in TCP latency measurement
description: There and back in quantifiable time.
---

Recently, Google have published an article on [BRR](http://queue.acm.org/detail.cfm?id=3022184), an algorithm that explicitly measures the round-trip latency and bandwidth capacity of the link between two machines (be it in a datacenter, or a mobile phone) to avoid sending more traffic than is useful, causing queues to build up in the network that needlessly increase latency. So I thought I'd dig into some of the mechanisms in use, especially as they're also use in general performance monitoring.
<!--more-->

One of the ways to measure round-trip latency is described in [TCP Extensions for High Performance](https://tools.ietf.org/html/rfc7323#section-4) amongst other extensions to TCP.

<!-- ![Normal TCP operation](/images/2016-12-13-tcp-rtt-measurement/seq-a.svg) -->

In normal operation, each packet that a computer sends will have two timestamp values attached, a value from the local clock, and an echo of the latest timestamp value seen from the remote side of the connection. 

On example from the RFC looks like this:
```
             TCP  A                                     TCP B
                             <A,TSval=1,TSecr=120> -->
                  <-- <ACK(A),TSval=127,TSecr=1>
                             <B,TSval=5,TSecr=127> -->
                  <-- <ACK(B),TSval=131,TSecr=5>
               . . . . . . . . . . . . . . . . . . . . . .
                             <C,TSval=65,TSecr=131> -->
                  <-- <ACK(C),TSval=191,TSecr=65>
```

Each packet in this scheme ends up being annotated with two timestamp values, the `TSval` (the outgoing timestamp) and the `TSecr` (the echo value). So, to measure the round-trip time on a connection, you can record the time between seeing an outgoing `TSval` on the outgoing stream, and the same value being echoed on the incoming stream.

My quick hack will track these for each tcp flow, and track them using a histogram bucketed by latency, and then export those into [prometheus](https://prometheus.io/).

However, my initial implementation contained rather unfortunate bug--and one that is specifically mentioned in the RFC linked above. So, for example, I'm running SSH to a machine hosted in Europe, and it's sending an update every 1 second. Graphing the results using grafana results in something like the following:

![Graph with erroneous 1sec round trip time](/images/2016-12-13-tcp-rtt-measurement/wrong-graph.png)

For this graph, the  green line represents the time take to process packets on the local machine, and the orange is the ostensible time taken for the remote host to send and respond to packets. However, this is somewhat less than useful.

I know for sure that the round trip time is not almost exactly a second, and is more like ~20ms. It's also quite telling that the reported round trip time is the same as the frequency of updates from the remote machine.

In this case, the dotted line represents a pause in communication of about a minute. So, packet `C` is the first packet that `A` sends after the pause, and hence contains the echoed timestamp value from before the pause, which unfortunately, implies that the round trip time between packets `ACK(B)` and `C` is 60s, which clearly isn't right. To rectify this, the RFC suggests:

>    RTTM Rule: A TSecr value received in a segment MAY be used to update
>               the averaged RTT measurement only if the segment advances
>               the left edge of the send window, i.e., SND.UNA is
>               increased.

`SND.UNA` in this case means the unacknowledged sequence number, so the first byte that we don't know the receiver has confirmed receipt for. In other words, only use echo times from packets where the remote end acknowledges receipt of some previously unacknowledged data. Whilst this does mean we have fewer samples, being able to ask questions of the wrong data doesn't really buy you much at all.

![Corrected Graph](/images/2016-12-13-tcp-rtt-measurement/fixed-graph-ssh.png)

In this case, I'm just typing a command in at the prompt (hence the short period), and we end up with the rather more realistic RTT time of ~20ms across the internet and back. 

So, why would I care about any of this, when BBR is designed to be used in the kernel? Well, it's quite common for network glitches to happen from time to time, especially when you rent space from a cloud provider. So having a tool to monitor track performance of individual network flows can be very usful when trying to debug a performance issue.

And hey, graphs are _cool_.
