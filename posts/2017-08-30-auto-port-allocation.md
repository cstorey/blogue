---
date: '2017-08-30'
title: 'Automatic port allocation'
description: "For when you just can't decide..."
---

It's quite common to want to test a network service from the outside, as if it was being accessed from a client. Quite often, people will pick a "well-known" port to use, eg: port `8080` or `8888` for a HTTP service. But that means that if you leave a stray service process lying around, you'll need to hunt it down before you can re-test.<!--more-->

Another common approach is to iterate through a randomized set of ports until you find one you can `bind(2)` to with a listening socket; then closing it, then passing that discovered port number to your service. Unfortunately, this approach tends to be somewhat racy, as another service might be allocated that address between the time of check and the time of use.

It's possible to run this process iteratively, retrying until your service is able to bind to a pre-specified port, but even then, there's a chance that your tests may fail for no good reason if the network namespace is shared.

One little known feature of the BSD sockets interface is that whilst port zero isn't actually a usable port for TCP or UDP; asking to listen on port zero will mean the TCP/UDP stack will bind to a free port. You can then learn which port that is using `getsockname(2)`.

For example, in one of our internal clojure applications, we have something like this:

```clojure
(defrecord JettyServer [port web-app server]
  component/Lifecycle
  (start [component]
    (log/info ::starting-jetty :port port)
    (let [server (run-jetty (ring/request-handler web-app) {:port port :join? false})
          port' (->> server (.getConnectors) (some (memfn getLocalPort)))]
      (log/info ::started-jetty server :on-port port')
      (assoc component :server server :port port')))

  (stop [component]
  ;;;...
  ))
```

Whilst it's not entirely clear if you don't read clojure, this uses the [`Server`](http://www.eclipse.org/jetty/javadoc/9.4.6.v20170531/org/eclipse/jetty/server/Server.html)`#getConnectors()` and then [`NetworkConnector`](http://www.eclipse.org/jetty/javadoc/9.4.6.v20170531/org/eclipse/jetty/server/NetworkConnector.html)`#getLocalPort()` to find, and then log the local listen address.

Then in your tests, you can configure your client to connect to the resulting port, meaning that there's one less reason (unrelated to what you're verifying) for your tests to fail.
