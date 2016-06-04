---
date: '2014-10-31'
orig_url: http://www.lshift.net/blog/2014/10/31/clojure-component-patterns
title: More Clojure component patterns
description: Parking the cart before putting the horse to bed.
---
Like the proper nerd that I am, I’ve been recently playing around with
the [National Rail data
feeds](http://nrodwiki.rockshore.net/index.php/Main_Page), with a view
to drawing some pretty pictures of quite how differently timely our
train services are. So, when you're developing at the REPL, it's
convenient to be able to cleanly shutdown and restart a n service.

One of the first steps I take when looking at a dataset is to load it
into a relational database, namely PostgreSQL; in order to more easily
get a feel for the shape of the information, and I've used
[core.async](http://clojure.github.io/core.async/) and the
[component](https://github.com/stuartsierra/component) library to do
this. Whilst this post won't be about the specifics of the ETL process
itself, there are a few patterns around structuring the processes that
I've found useful.

Parents produce, children consume.

When you're shutting down a system when there are messages in flight,
then in most cases, you'll want to avoid dropping messages on the floor,
and so at the system level, you want to shut down the producers first,
then their children, their children, and so on, until you reach your
pipline's egress point. If you start your shutdown from the egress point
and work backwards, then because a given producer may well be still
producing information after a consumer has shut down, that information
will effectively be lost. It's also possible that the system may
deadlock, depending on how your internal queues are configured.

Now, the component library gives us one main way to achieve ordering,
and that's by specifying inter-component dependencies, with the
component/using function, eg:

```clojure
(defn bootstrap [config]
   (component/system-map
     :db (database-pool (:db config))
     :app (component/using
         (my-lovely-application)
         [:db])
     :http (component/using
         (make-http-server (:http config))
         [:app])))
```

To summarize the behaviour of the component library, a `system-map`
represents a hash-map that represents the topology of the system, and
`using` associates meta-data with each component (eg: the database, or
the web application and server) describing which other services it
requires. So in this case, the application queries the database, and the
http-server requires an application to service requests.

However, in this case, we can think of the web-server being a producer
of events (http requests), that are "consumed" (responded to) by the web
server. So, logically, if you shut down the application and/or database
before the http server, then you'll have a service which likely isn't
actually able to service requests, and at best will return 500 Internal
server errors. To avoid this, we need to shut-down and drain the
incoming requests before shutting down the application.

Fully draining inputs.

When I'm using core.async in a pipeline, I associate one or more
core.async processes (or go-routine, if you prefer) with each component
to actually do the work.

When you shut down a system in the above manner, however, you need to
ensure that each component within the system has actually fully
processed it's input before you shut down any components downstream from
it. It's easy enough to construct a fresh input channel when the
component starts, and close it when the component is stopped. However,
with the default core.async channels, it's not generally possible to
inspect the buffer to check whether it has been fully drained or not.

However, when you start a go-routine (or thread) in core.async, you are
returned a channel which represents the value returned from the body of
the go-routine when it completes. So, we can stash a reference to this
channel within the component state, and use that to determine when this
component is fully done. For example:

```clojure
(def consumer-loop [ingress-ch]
   (go-loop []
     (when-let [msg (<! ingress-ch)]
       (do-something-exciting-with msg)
         (loop))))

(defrecord MyBatConsumer [ingress-ch proc-ch]
  component/Lifecycle
  (start [self]
    (let [ingress-ch (chan)
      p (consumer-loop ingress-ch)]
  (assoc self :ingress-ch ingress-ch :proc-ch p)))
  (stop [self]
    (close! ingress-ch)
    (<!! proc-ch)
    (assoc self :ingress-ch nil :proc-ch nil)))
```

As you can see, we do a blocking wait on the channel returned by
`consumer-loop` when we shut down. Granted, this is a proof of concept;
in fully production-ready code, you'll need to allow for the risk of the
component failing, and timing-out on shut down.
