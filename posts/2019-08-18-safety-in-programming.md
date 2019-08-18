---
date: '2019-08-18'
orig_url: null
title: "Title"
description: 'Witty subheading'
---

This is an adaptation of something I wrote on Slack, when discussing the pros and cons of just following convention when it comes to safety.
<!--more-->

---


To my mind, the biggest value in having local customs around service layout is to lower the cost of change.

Let's say we have a component that listens for events from others. In that case, you might have a habit of putting those listeners into a file named `listener.rs`. If this becomes a habit, you will know to look at the listener file, instead of needing to search for it.

When we write a service then change it rarely afterwards, this habit can be very useful. If you are unfamiliar with a section of the codebase, these kinds of affordances can make life easier. This results in less time sifting through code, and so makes it easier to change.

 Another way to lower the cost of change is to have clear roles for each module within it. (And these roles themselves can follow clear patterns). Even them, these roles serve to hide some detail from the rest of the system.

A good interface means that a caller express precise intent, and let someone else worry about  the details. For example, the rust postgres library lets you can ask questions and get data back. Without it, you'd need to know exactly how to format queries, and parameters in the way the database understands, and then parse the replies.

A good interface will also make clear what it depends on. Let's say some people in an office want to number documents. We can keep a logbook with the last number used. We can read the previous value we gave out, write down the next number, and use that for our document.

If lots of people want to number documents at the same time, then we encounter a problem. What if two people read then write the same numbers, and have the same number for their documents? People shouldn't make this mistake, as they'll notice that someone else is holding the book, and won't race to write in it before the other has finished. Some people describe this as “common sense”.

Computers don't have common sense. This is why we need to spell everything out to them in exacting detail. If they have read the last document number, they won't know to check no-one else has changed it when they write down the new number, either.

So, in a distributed system we can use a centralised lock service to solve this problem. With this, the computer will take the lock, update the number and release the lock. Even then, if the new number routine does not know anything about the lock mechanism, it's still possible to make the same mistake. A programmer might forget that they need to take the lock to call the new number routine, for example.

So, we have ways to solve this. One is we make the new number routine manage the lock itself. This is fine as long as it controls everything itself, but once we have other routines that use locks involved, then things get more complicated. This is okay if the lock for the new number is only for that new number. But if we share it with other routines, we risk problems like deadlocks.

Another solution is to inform the new number routine that we hold the lock, and have the routine fail if we do not. For example, the lock server may provide a token the routine can verify. (https://martin.kleppmann.com/2016/02/08/how-to-do-distributed-locking.html).

The logical conclusion to this is in the rust standard library. The `Mutex<T>` type holds a value of some type T, and will only permit access via a guard that guarantees we hold the lock.

A big part of designing safe systems is understanding how things go right over the long term. A big part of this is to ensure that it's easy to do the safe thing, and *very hard* to do something unsafe.

The example of numbering documents is quite a low stakes task. In a bank though, we need to number payment cards, so we know whose account to charge. Having two cards with the same card number may mean one person can spend another's money. This is obviously bad for the customer, and leaves the bank liable for the mistake.

Now, these may seem like mistakes that are simple to avoid, but in complex systems, things go wrong all the time (https://how.complexsystems.fail). So it's wise to design assuming that failures will happen. For example, a tired, or rushed developer may not know to hold a lock while calling the new number routine. Having the routine fail in that case will mean that at they discover their mistake quickly.
