---
date: '2013-09-30'
title: 'My little Backpressure: Flow Control is magic'
description: Avoiding congestion and I/O stuffiness.
---
<div class="content" html="http://www.w3.org/1999/xhtml">

When we’re designing systems that are designed to be robust against
failure, it’s important to know how behaviour at your Integration points
(a term borrowed from Michael Nygard’s book [Release
It!](http://pragprog.com/book/mnee/release-it)) impacts the rest of the
system. For example, if your database or a remote API is running slowly,
then in Synchronous systems, because you (usually) have a limited number
of threads on which to process the requests, any slow down will
naturally be pushed back onto the clients of that service.

<span id="more-1996"></span>

Assuming your application server behaves sensibly, then in the worst
case, then the listen queue on the server’s listening socket will fill
up, and any further connections will be refused. Any load balancer that
you have in front of it should then know to stop sending requests to
that instance.

However, in pervasively Asynchronous systems, it can be more difficult
to manage control flow. For example, in its default configuration, the
Node.js web server module will allow you to have as many outstanding
requests as it can hold in memory. Naturally, for some applications that
can be helpful. However, if as above, your authentication service is
running slowly, then all of those requests can potentially pile up in
your Node.js process until you run out of memory.

Again, as with the synchronous case it is possible to put a cap on the
number of outstanding requests, but managing the flow control can get
quite complicated quite quickly, unless you are *really* careful.

This is where core.async comes in. By allowing you to write
synchronous-looking code that runs asynchronously, I find that it’s a
lot easier to build reliability features, and in our example,
back-pressure.

Let’s take a trivial example of doing this on a Node.JS with the
[Streams2 API](http://blog.nodejs.org/2012/12/20/streams2/). The new api
makes backpressure far easier to manage when *reading* (as the consuming
code is notified when data is ready, and can then pull data when it is
ready), there are still difficulties around writing. For example, the
state of node’s internal buffers are reflected via two mechanisms:

-   The write method on a stream will return true until a high watermark
    is hit, after which it returns false until these are flushed
-   The write method takes an optional callback, which gets invoked when
    requested write has been flushed to the operating system (or
    has failed).

Now, it is entirely possible to use these to build a state machine which
allow you to propagate back-pressure, but such things can often be quite
ad-hoc, and rather more complex than the synchronous version. Also, both
need to implemented when implementing your own Stream. So unfortunately,
this complexity means that back-pressure often seems to get ignored when
writing data.

So, I’ve put together a [full
example](https://gist.github.com/cstorey/6751325) of how one could
implement a buffering version of cat using core.async.

The most complex part of the code is in the function
`writeable-from-chan`, as below:

```clojure
(defn writeable-from-chan [ch strm]
  (let [drains (chan)]
    (go
      (loop [] ;; #1
	(let [buf (<! ch) ;; #2
	  drain-cb (fn drain-cb [] (put! drains :token))] ;; #4
	  (if buf
	(do 
	    (.write strm buf drain-cb) ;; #3
	    (<! drains) ;; #5
	    (recur))
	(.end strm))))))) ;; #6
```

Essentially, this starts a loop (\#1) which takes input from the input
channel (\#2) and writes (\#3) it to the output stream. At this point,
we pass in a callback (\#4) which signals when the write is complete,
and we can then effectively *block* on the completion of the write
(\#5). When the stream is closed (and the take at \#2 returns nil) we
close the output stream. (\#6).

Now, the nice thing here, is that back-pressure is managed for you; a
default channel only offers one cell of space, and so any subsequent
puts will block until another process has taken that value from the
channel. So, data can be pushed through a channel with no need to invoke
`#pause()` or `resume()` methods on the input, nor do you have to handle
back-pressure specially, or do any extra buffering.

In summary, the control flow can become clear in the structure of your
code, rather having to construct an explicit state machine mechanism,
which greatly enhance clarity, whilst preserving a degree of safety.

</div>
