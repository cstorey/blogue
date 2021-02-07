---
date: '2013-08-31'
orig_url: http://www.lshift.net/blog/2013/08/31/posix-threads-and-fork2-on-not-crossing-the-streams
title: "POSIX Threads and fork(2): on not crossing the streams"
description: New Jersey's revenge.
---
You’ve maybe heard that with C and C++, it’s rarely a good idea to mix
usage of the POSIX threading library, and `fork(2)` based concurrency.
I’d heard this myself, but to be honest, I never quite understood why.
<span id="more-1914"></span> The Linux manual page for
[fork(2)](http://linux.die.net/man/3/fork) puts it in the following way:

> A process shall be created with a single thread. If a multi-threaded
> process calls fork(), the new process shall contain a replica of the
> calling thread and its entire address space, *possibly including the
> states of mutexes and other resources*. Consequently, to avoid errors,
> the child process may only execute async-signal-safe operations until
> such time as one of the exec functions is called. Fork handlers may be
> established by means of the pthread\_atfork() function in order to
> maintain application invariants across fork() calls.

But like so many things, it’s often easier to understand something when
you have a concrete example.

So, for one of our projects, we’re using
[`collectd`](http://collectd.org/) for system monitoring, together with
the [exec plugin](https://collectd.org/wiki/index.php/Plugin:Exec). Most
the time, it would work perfectly well. But there turned out to be an
interesting, if somewhat sporadic bug. We’d find that the child process
would hang, before it had gotten round to executing the script we wanted
to run.

Even weirder, when attached to the child with a debugger, and looked at
the stack trace, it had hung inside of the name service switch
subsystem, specifically
[`getpwnam_r`](http://linux.die.net/man/3/getpwnam_r). This function can
be used for mapping a username to a numeric user and group id, as
`collectd` generally runs as `root`, and it’s generally unwise to run
your plugins with super-powers intact.

Now, based on the immediate evidence, it looked *awfully* like there was
a manifest bug in the C library. Now, you can imagine how confused I was
by thisーhow could such a commonly used function contain such an obvious
deadlock?

Now, as you might have guessed, it wasn’t a bug, it was what C library
documentation (seemingly mockingly) calls undefined behavior.

What had happened, is that the name service switch code uses a global to
protect its internal state. So, the thread running the exec plugin had
forked whilst *another* thread was running with that mutex held, so when
the mutex was unlocked in the parent, it never got unlocked in the child
(because only the thread that calls
[`fork(2)`](http://linux.die.net/man/3/fork) is copied to the child). So
when `collectd` tried to lookup the user id to switch to, it called into
`getpwnam_r`, and hung.

As it turned out, this had been a [known
issue](https://github.com/collectd/collectd/issues/229) for a while.
Thankfully, like a lot of things, once you understand the problem
correctly, the
[solution](https://github.com/collectd/collectd/commit/9de042657fa536305c5d98ef114dd9750ed4d656)
was simple, just move the username lookups to the parent process.

Anyway, I hope this is illuminating for you as it was for me.
