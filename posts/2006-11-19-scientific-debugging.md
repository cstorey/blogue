---
date: '2006-11-19'
title: The Scientific Method in Debugging.
description: How to know when it works.
---
Now, we've all experienced some kind of problems when using computers
and understand quite how fustrating it can be when the solution doesn't
appear to be in plain sight. This can often be quite a fustrating
experience for both the user, or the person whose function it is to fix
the problem.

Of course, when one is under pressure, or when something has been a
problem for a while, then it's all too easy to regard a problem as being
intractable, or even impossible to solve. We might even begin to imagine
that the computer (or other device) has a will of it's own, and intends
to thwart our good intensions, asserting that the machine contains
“Gremlins” or other malevolent entities. We might then attempt to
exorcize the system by perhaps, rebooting the machine, or reinstalling
the offending item of software. We might even resort to hurling insults
at people who maintain other components of the system in use. By and
large, this is complete and utter hogwash, borne from our own
fustrations. In fact these techniques are often entirely ineffective,
and may serve to impede our diagnoses.

The fact is, that when analysed with a cool mind, we can realise that
the system is simply a set of components, each of which is composed of
smaller components, and so-on. For example, a service (say, an online
application) may be composed of a set of front-end webservers,
middleware servers, and backend relational database servers. The
database servers are composed of a network stack, the RDBMS instance
(say, an installation of Oracle or PostgreSQL, &c), and the storage
stack. The storage stack might be composed of a filesystem, the block
device layer, and the physical storage, so on and so on, until we're
worrying about how many electrons there are in a bit of silicon or
germanium compared to another part (although most people,
understandably, don't need to go that far). Of course, there's not
always a clean seperation between components, but accounting for that is
left as an exercise for the reader.

An example of the stacks given in the paragraph above

Each of these components can usually be considered to be a black box,
which takes certain inputs, and produces certain results. So, as long as
we have an understanding of what *should* happen at each stage, then
we've got a firm basis to start our debugging.

### Obvservation

First of all, we need to observe what's occurring, and note how it
differs from our model of the inputs and outputs of the system as a
whole. For example, we might have a web application which is mangling
internationalized text, such that non-english characters (such as
accented characters) come out as two or more bogus characters. With some
knowledge of [Unicode and character
sets](http://www.joelonsoftware.com/articles/Unicode.html),
we can hypothesize that the application is encoding what it thinks is
[ISO-8859-1](http://en.wikipedia.org/wiki/ISO_8859-1)
text as [UTF-8](http://en.wikipedia.org/wiki/UTF-8)
(as demonstrated in
[Iñtërnâlizætiøn](http://www.intertwingly.net/stories/2004/04/14/i18n.html)
by Sam Ruby). All one really needs to know at this point is to have an
idea of how it *should* look, under various circumstances.

For example, we know what the french word “vélo” looks like, and we can
do a test to see how it should be encoded on the wire:

    : cez@apsu; echo vélo | sed -ne l
    v\303\251lo$
    : cez@apsu; 

Here, we use the “l” command in `sed(1)` prints out the current line in
a “visually unambiguous” form, and handily, displays octets outside of
the ASCII range as their escaped octal equivalents.

So, from this we see that “é” is represented in UTF-8 as two octets. For
the time being, we needn't worry about the exact value of those octets,
although those impatient to know should read [RFC
3629](http://www.ietf.org/rfc/rfc3629.txt).

You'll also need to know enough about the protocols used between each of
components to account for any re-encoding of the data. For example in
the vast majority of XML-based protocols (eg: the [Atom Publishing
Protocol](http://bitworking.org/projects/atom/)),
the encoding is specified either as a [parameter to the
Content-Type](http://www.ietf.org/rfc/rfc3023.txt);
or within the [XML
declaration](http://www.w3.org/TR/REC-xml/#charencoding)
(if it's specified in both places, this may lead to inconsistencies, and
problems), so we need to inspect those to account for any changes in
encoding. However, a change in the declared encoding may signify a
problem in itself, of course.

So, armed with this information, we can work though the stack
incrementally, inspecting the traffic at each point in the highest level
of the stack to check that the data is being transmitted correctly.

So, in our example, you would inspect traffic:

-   From the user to the browser
-   From the browser to the web front-ends
-   From the front-ends to the middleware
-   From the middleware to the database

And vica versa:

-   From the database to the middleware
-   From the middleware to the front-ends
-   From the front-ends to the browser.
-   From the browser to the user

For example, you could use `tcpdump(8)` to inspect the traffic as it
flows over the wire between each component. (Admittedly, I'm yet to meet
a user who could be inspected using a network packet capture tool).

Now this implies that you must work your way through the stack one level
at a time; not so. Personally, I find that this approach would work for
systems composed of different components, each with a differing
function. However, if you're dealing with a system which is composed of
quite homogenous components (for example IP routers, or more generally
parts of the system which shuffle a bit of data along to the next part
without altering it much), then it can be useful to take a binary search
model, whereby you start in the middle, and eliminate the halves of the
model which appear to be operating correctly.

Of course, whilst this can safe some amount of work, this can miss
problems when a component will be symetrically broken. This might be the
case where you're sending UTF-8 data to a database which is expecting
ISO-8859-1 text, but stores it again in UTF-8. The data may be stored
doubly encoded, but it'll be decoded when it's sent back out again. In
some cases, this may not be a problem at present, but it'll serve to
trip us up in the future, should we ever need to go around the back of
the component.

### Hypothesis and Experimentation

Now, armed with this information, we might find that the data going in
one side of the middleware is correctly encoded, but it's producing the
wrong output. In this case, we need to be able to extract that component
from the system, and test it as a single unit. Sometimes, a software
package will provide a suite of software tests that you use to start
from, and it can be useful to integrate your failing example into these
tests (if possible) to aid in the diagnosis. This helps to ensure that
our tests are repeatable.

But we needn't always be quite so formal about it. Sometimes, all that's
required are a few quick invocations of a command line interface in
order to test a hypothesis. But in general, we should endevor to
document the tests we've done (for example, in a trouble ticket) to
ensure that our tests are repeatable, and that we don't do the same test
twice. On the other hand, we may need to go to the trouble of setting up
a duplicate environment to test against (although many shops will have
one of these in place already).

So once we've established that we have the faulty component, we must use
our knowledge of the component's internals to subdivide it into it's
parts, and then do appropriate tests on these parts. For example, you
might add code to the suspected code-paths to log the state of the data
at strateigic points. Although, we need to be wary of how this data may
be inplicitly transformed when it's either taken into, or output from,
the system. Languages such as
[Perl](http://perl.org/) which attempt to
automagically treat strings as unicode where appropriate can be
dangerous, because it's not always obvious where this is occurring, how,
or why, so this may lead to inaccurate, or just plain wrong results.

Now, the line between what constitutes experimentation and observation
can be somewhat fuzzy, mostly because in practice, the steps of “trying
an experiment” and “observing the results” get conflated into one.
However, proper practice of experimentation is vitally important when
testing a hypothesis, because if you don't check that you're actually
looking at the correct component, you're liable to be wasting large
amounts of time. If you're not able to reproduce the problem in an
isolated environment, then it's possible that you've either neglected
something which affects the results, or you're testing the wrong
component.

For example, some time ago, the company I was working for had a
provisioning application which was largely based around Perl DBI and
PostgreSQL. Then one day, we suddenly found that some transactions were
hitting a deadlock. At this point, two methodologies were applied by two
seperate parties:

-   One party decided to look at what resources were being held by each
    postgresql connection, and attempt to correlate that with a client
    process, to see if we could see which query was causing the problem.
-   The other decided that the first port of call was to
    upgrade PostgreSQL.

After a few hours of experimentation, and investigation by both parties
(and a few upgrades of PostgreSQL) later, we traced the problem to a
long running application, and an unforseen consequence of DBI's behavior
when AutoCommit was turned off. DBI will implicitly begin a transaction
when you first issue a query in this mode, and so the transaction will
only be over (and held resources released) once an explicit `COMMIT` or
`ROLLBACK` instruction is sent to the database. The application
concerned was caching datahase connections (as creating a connection to
PostgreSQL is relatively expensive, as each connection is served by a
seperate Unix process), but it wasn't rolling back the current
transaction when some part of the process failed.

It wasn't entirely clear to either party exactly why this behaviour was
causing problems, but the point is that one party lept to an assumption,
whereas the other decided to observe the behaviour of the system as a
whole before jumping to conclusions. The point is, that we need to be
precisely sure of why we're doing each test, otherwise our tests will be
inefficient, and may not actually help solve the problem.

### Applying the fix

Then, of course, once you've discovered the faulty component, you need
to discover *why* exactly it's at fault. It may be tempting to apply a
band aid, but over time these build up cruft in the system, as well as
making it more fragile, as the band-aid may interact unexpectedly with
something else, perhaps because it changes the interface of that
component. For example, if we were to “fix” the internationalisation
problem above by stripping out all non-ASCII characters, then it's
likely that there'd still be complaints from customers in France, for
example.

### “Rebooting the computer”

One technique that seems to be commonly used in debugging practice is
that of rebooting the computer at fault, or re-installing a faulty
component, which, more often than not, seems to have the desired effect.
However, it's likely that you've only temporarily removed a symptom, but
not corrected the underlying cause. An example of this is when a machine
experiences a kernel crash, or complete and utter hang. Yes, rebooting
the machine helps fix the problem temporarily, but it's usually best to
investigate the underlying problem, and attempt to rectify this.

The reason that rebooting, or re-installing a component can seem to
alleviate a problem, is because it re-sets that component to a (at least
partially) known state. In some cases, this is simply a retrograde step,
because you may be unwittingly destroying information which may prove to
be vital in the diagnosis. For example, some operating systems may
display a kernel stack backtrace on the console of the machine, which
may seem like random gibberish to the uninitiated. However, this can be
vitally important when to a systems developer.

Another example might be to reboot a machine which has been subject to
unwanted intrusion, and is causing a denial of service against either
your, or someone else's network. In this case, the state of the running
processes on the machine, and network connections can be of great help.
For example, there may be a process on the machine which is permitting
remote control of the machine by a malicious third party. A common
method of controlling these bots is to connect to an IRC server, and
join a channel which distributes commands. If you just turn the machine
off, then you won't be able to discover where the commands are coming
from (this is probably going to be quite difficult anyway, because an
attacker with an ounce wouldn't normally be connected directly from say,
their home ADSL connection).

Of course, that's not to say that we shouldn't leave the machine in an
unrecoverable state. In the case of a break in, it's often best to
disconnect the machine as soon as you can, and you may not be able to
move the machine onto a quarantine area for examination, we need to
compromise by taking a snapshot of the state of the machine (filesystem
snapshots and / or virtual machine snapshots are useful here). Or in the
case of a kernel crash, you might take a photograph of the console
screen, or just configure a logged serial terminal for the next time it
occurs.

### In real life

Now, by and large, you won't need to think about the steps that you
undergo when you start to debug a problem; once you've been working on
something for a while, you internalise the structure, and you no longer
have to think about it. But of course, we sometimes get stuck, and find
a problem that's seemingly impossible to solve. In these cases, stepping
back from the system, and working through it methodically is the answer.

It's also often the case that multiple teams of people are involved with
fixing a particular problem, which can mean that it'll take far longer
to arrive at a diagnosis, especially when they don't keep a detailed
record of what has been tested, and the evidence that's been gathered
already. It can be easy to descend into irrationality and start blaming
other teams for problems, when some methodical diagnosis would help
solve the problem. One answer to this is to state that all investigation
must be recorded in a form that's accessiable to all concerned (for
example, a trouble ticket system).

For example, some of the lower levels of support teams aren't always
entirely forthcoming with any diagnostic information they've gathered
(if any) when escalating an issue to higher level teams. One answer to
this problem is to standardize on what level each team is expected, what
tests they should perform at each step, and how these should be
recorded. Of course, this needs to be enforced, perhaps by having one
member of each team who is dedicated to doing triage, and ensuring that
all the required information is present. If not, the request should be
returned to the original department (of course, this draconian method
isn't always appropriate).