---
date: '2016-01-27'
orig_url: http://www.lshift.net/blog/2016/01/27/rules-based-mio-chat-example
title: Rules-based Network programming with Mio and Rust
description: Making the non-deterministic, less so.
---
One thing that you notice after spending most of your time looking at
the insides of a program, is that it’s very easy to get bogged down in
implementation detail, and end up with rather an optimistic view of how
well the world outside of your application works.<!--more--> This is
an especially common theme in the distributed systems field, and Peter
Deutsch’s infamous [Eight Fallacies of Distributed
Computing](https://blogs.oracle.com/jag/resource/Fallacies.html) are a
good example of this.

I recently read a [blog post from Adrian
Colyer](http://blog.acolyer.org/2016/01/19/dcft/) on the paper
“[Experience with Rules-Based Programming for Distributed, Concurrent,
Fault-Tolerant
Code](http://web.stanford.edu/~ouster/cgi-bin/papers/rules-atc15)“. I’ve
been fiddling with the idea of implementing distributed systems in
[Rust](http://rust-lang.org/) recently, so I figured I’d spend some
seeing how well they fit together.

A principal contribution of the paper itself, is the description of an
implementation pattern where rather than having input events from the
outside world drive a traditional state machine directly, we split the
event handling into updating our understanding of the world, and then
acting based on the updated state. This happens to be quite similar to
how some bottom-up logic programming systems are implemented, so I
suspect that there’s a lot of scope for seeing how logic programming can
be applied here. Especially in light of ideas such as
[Bloom](http://bloom-lang.net/).

In the paper, they give the example of the Hadoop Job tracker, and note
that it’s implemented using a fairly traditional state machine
mechanism, where nodes in the graph are represented by an Java
enumeration type, and handlers are attached to state transition arcs. In
total, this takes up about weighs in at a rather less efficient 2250
lines of Java to implement. They find that the handlers often represent
quite similar actions, and so those handlers can contain a lot of
duplicated code. They provide a link to a re-implementation of the model
in python in approximately 120 lines of code, and demonstrate that is
behaviorally equivalent to the original. If nothing else, it makes it
far easier to get a feel for the shape of the code just by reading it.

So, in the spirit of reductive examples everywhere, our example comes in
the form of a chat server. Where the paper operates on top of an
existing RPC abstraction, we apply it directly on top of the socket
interface provided by [mio](https://crates.io/crates/mio/). The bulk of
our processing happens in the
[`Handler#ready`](https://github.com/cstorey/mio-rules-example/blob/25be0cf04c66a526eb6008dfe587d56120d07e51/src/main.rs#L293-L313)
method of our handler. 

```rust
impl mio::Handler for MiChat {
    type Timeout = ();
    type Message = ();
    fn ready(&mut self, event_loop: &mut mio::EventLoop<Self>,
	token: mio::Token, events: mio::EventSet) {
        info!("{:?}: {:?}", token, events);
        self.connections[token].handle_event(event_loop, events);
        if self.connections[token].is_closed() {
            info!("Removing; {:?}", token);
            self.connections.remove(token);
        }

        let mut parent_actions = VecDeque::new();
        loop {
            for conn in self.connections.iter_mut() {
                conn.process_rules(event_loop, &mut parent_actions);
            }
            // Anything left to process?
            if parent_actions.is_empty() { break; }

            for action in parent_actions.drain(..) {
                self.process_action(action, event_loop);
            }
        }
    }
}
```

So in the terminology of the paper, we have two
tasks, namely listening for new connections, and handling established
connections.

Because our tasks operate mostly at the transport layer, our events are
essentially socket readiness notifications, and the bulk of the actions
include such exciting things as reading/writing from sockets. For
example, the [event handler for the listening
socket](https://github.com/cstorey/mio-rules-example/blob/25be0cf04c66a526eb6008dfe587d56120d07e51/src/main.rs#L222-L227)
is mostly trivial:

```rust
impl Listener {
// ...
    fn handle_event(&mut self, _event_loop: &mut mio::EventLoop<MiChat>,
	events: mio::EventSet) {
        assert!(events.is_readable());
        self.sock_status.insert(events);
        info!("Listener::handle_event: {:?}; this time: {:?}; now: {:?}",
                self.listener.local_addr(), events, self.sock_status);
    }

```

And that [for each
connection](https://github.com/cstorey/mio-rules-example/blob/25be0cf04c66a526eb6008dfe587d56120d07e51/src/main.rs#L94-L99)
is remarkably similar:

```rust
impl Connection {
// ...
    fn handle_event(&mut self, _event_loop: &mut mio::EventLoop<MiChat>,
	events: mio::EventSet) {
        self.sock_status.insert(events);
        info!("Connection::handle_event: {:?}; this time: {:?}; now: {:?}",
                self.socket.peer_addr(), events, self.sock_status);
    }

```

Then the bulk of the activity happens in the
[`Listener#process_rules`](https://github.com/cstorey/mio-rules-example/blob/25be0cf04c66a526eb6008dfe587d56120d07e51/src/main.rs#L229)
method.

```rust
impl Listener {
// ...
    fn process_rules(&mut self, event_loop: &mut mio::EventLoop<MiChat>,
            to_parent: &mut VecDeque<MiChatCommand>) {
        info!("the listener socket is ready to accept a connection");
        match self.listener.accept() {
            Ok(Some(socket)) => {
                let cmd = MiChatCommand::NewConnection(socket);
                to_parent.push_back(cmd);
            }
            Ok(None) => {
                info!("the listener socket wasn't actually ready");
            }
            Err(e) => {
                info!("listener.accept() errored: {}", e);
                event_loop.shutdown();
            }
        }
    }
```

So, when our listening socket is readable (IE: has a client attempting
to connect), we `accept(2)` the connection, and notify the service of
the connection, by passing a `MiChatCommand::NewConnection` into the
queue of outstanding actions to be processed. The
[`Connection#process_rules`](https://github.com/cstorey/mio-rules-example/blob/25be0cf04c66a526eb6008dfe587d56120d07e51/src/main.rs#L103)
method is a little more complex, as it handles both reading, writing to
the socket, and invokes
[`Connection#process_buffer`](https://github.com/cstorey/mio-rules-example/blob/25be0cf04c66a526eb6008dfe587d56120d07e51/src/main.rs#L122)
to actually parse the input into “frames” (lines in this case).

```rust
impl Connection {
// ...
    fn process_rules(&mut self, event_loop: &mut mio::EventLoop<MiChat>,
        to_parent: &mut VecDeque<MiChatCommand>) {
        if self.sock_status.is_readable() {
            self.read();
            self.sock_status.remove(mio::EventSet::readable());
        }

        self.process_buffer(to_parent);

        if self.sock_status.is_writable() {
            self.write();
            self.sock_status.remove(mio::EventSet::writable());
        }

        if !self.is_closed() {
            self.reregister(event_loop)
        }
    }

    fn process_buffer(&mut self, to_parent: &mut VecDeque<MiChatCommand>) {
        let mut prev = 0;
        info!("{:?}: Read buffer: {:?}",
	    self.socket.peer_addr(), self.read_buf);
        for n in self.read_buf.iter().enumerate()
	    .filter_map(|(i, e)|
	      if *e == '\n' as u8 { Some(i) } else { None } ) {
            info!("{:?}: Pos: {:?}; chunk: {:?}",
		self.socket.peer_addr(), n, &self.read_buf[prev..n]);
            let s = String::from_utf8_lossy(&self.read_buf[prev..n])
	      .to_string();
            let cmd = MiChatCommand::Broadcast(s);
            info!("Send! {:?}", cmd);
            to_parent.push_back(cmd);
            prev = n+1;
        }
        let remainder = self.read_buf[prev..].to_vec();
        info!("{:?}: read Remainder: {}",
	    self.socket.peer_addr(), remainder.len());
        self.read_buf = remainder;
    }
```

Whenever we see a full line of input from the client, we pass a
`MiChatCommand::Broadcast`, we pass that
to[`MiChat#process_action`](https://github.com/cstorey/mio-rules-example/blob/25be0cf04c66a526eb6008dfe587d56120d07e51/src/main.rs#L40),
which will in turn enqueue the message on each connection for output.

```rust
impl MiChat {
// ...
    fn process_action(&mut self, msg: MiChatCommand,
	event_loop: &mut mio::EventLoop<MiChat>) {
        trace!("{:p}; got {:?}", self, msg);
        match msg {
            MiChatCommand::Broadcast(s) => {
                for c in self.connections.iter_mut() {
                    if let &mut EventHandler::Conn(ref mut c) = c {
                        c.enqueue(&s);
                    }
                }
            },

            MiChatCommand::NewConnection(socket) => {
                let token = self.connections
                    .insert_with(|token|
			EventHandler::Conn(Connection::new(socket, token)))
                    .expect("token insert");
                &self.connections[token].register(event_loop, token);
            }
        }
  }
```

The rules processing in the `Handler#ready` implementation then halts
once everything we are concerned about has quiesced; in this case, when
no more actions are generated by our rules.

Personally, I do like this approach; it seems easier to end up with code
that is well factored, and can potentially encourage a reasonable
layering of concerns. For example, it is fairly easy to separate the
frame handling code from that which deals with the underlying socket
manipulation by thinking of it as a stack of adapters, each of which
bridging a few layers from the ISO layer model, which ultimately
communicates with your application model.

The full code can be found in the
[mio-rules-example](https://github.com/cstorey/mio-rules-example).

</div>
