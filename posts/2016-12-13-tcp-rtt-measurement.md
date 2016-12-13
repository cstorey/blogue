---
title: Adventures in TCP latency measurement
description: ...
---

TCP, or the Transmission Control Protocol is the main transport protocols used on the internet today, for example, it's used in HTTP. However, when you're running an application in production, and especially if you manage your own networking equipment (particularly if you rent datacenter space), it's really useful to be able to monitor performance within the network.

[TCP Extensions for High Performance](https://tools.ietf.org/html/rfc7323) describes a series of extensions to the plain Transmission Control Protocol, including the TCP timestamps mechanism. This also describes a way to measure the round trip time for each segment of data in a connection.

<!-- ![Normal TCP operation](/images/2016-12-13-tcp-rtt-measurement/seq-a.svg) -->

In normal operation, each packet that a computer sends will have two timestamp values attached, a value from the local clock, and an echo of the latest timestamp value seen from the remote side of the connection. 

I track these timestamps, and measure the time between seeing a timestamp being seen as an outgoing value, and it then being echoed back by the remote side of the connection. My quick hack will track these for each tcp flow, and track them using a histogram bucketed by latency, and then export those into [prometheus](https://prometheus.io/).

However, my initial implementation contained rather unfortunate bug--and one that is specifically mentioned in the RFC linked above. So, for example, I'm running SSH to a machine hosted in Europe, and it's sending an update every 1 second. Graphing the results using grafana results in something like the following:

![Graph with incorrect data](/images/2016-12-13-tcp-rtt-measurement/wrong-graph.png)

For this graph, the  green line represents the time take to process packets on the local machine, and the orange is the ostensible time taken for the remote host to send and respond to packets. However, this is somewhat less than useful.

I know for sure that the round trip time is not almost exactly a second, and is more like ~20ms. It's also quite telling that the reported round trip time is the same as the update frequency from the remote machine.

So, the example from the RFC looks like this:

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

In this case, the dotted line represents a pause in communication of about a minute. So, packet `C` is the first packet that `A` sends after the pause, and hence contains the echoed timestamp value from before the pause, which unfortunately, implies that the round trip time between packets `ACK(B)` and `C` is 60s, which clearly isn't right. To rectify this, the RFC suggests:

>    RTTM Rule: A TSecr value received in a segment MAY be used to update
>               the averaged RTT measurement only if the segment advances
>               the left edge of the send window, i.e., SND.UNA is
>               increased.

`SND.UNA` in this case means the unacknowledged sequence number, so the first byte that we don't know the receiver has confirmed receipt for. In other words, only use echo times from packets where the remote end acknowledges receipt of some previously unacknowledged data. Whilst this does mean we have fewer samples, being able to ask questions of the wrong data doesn't really buy you much at all.

![Corrected Graph](/images/2016-12-13-tcp-rtt-measurement/fixed-graph-ssh.png)

In this case, I'm just typing a command in at the prompt, and we end up with the rather more realistic RTT time of ~20ms across the internet and back.
