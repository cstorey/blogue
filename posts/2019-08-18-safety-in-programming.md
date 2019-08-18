---
date: '2019-08-18'
orig_url: null
title: "Title"
description: 'Witty subheading'
---

This is an adaptation of something I wrote on Slack, when discussing the pros and cons of just following convention when it comes to safety.
<!--more-->

---

Okay, I see what you mean, although I think it'd be interesting to find out which parts of this are deliberate choice, and which have emerged accidentally over time.

To my mind, the biggest value in having local customs around service layout is to lower the cost of change, in the sense that people will know where to look for a certain kind of behaviour, thus reducing the amount of time spent sifting through the code.

Another good way to lower the cost of change is to have modules that have well-defined contracts, and provide more value than just being a facade (John Ousterhout talks about how modules should be deep). And ideally, each module should be able to fulfil that contract while minimising the details that the caller needs to know about.

In this case, the contract for the `dao.IncrementCounterThing()` is “*Assuming that the caller holds a given lock*, we return a unique monotonic integer”. The “assuming” clause there represents some detail that the *caller* needs to know for the called function to do it's job correctly.

If we consider in terms of people, then it's like having a machine (eg: mechanical saw) where a the person working the machine feeds the work pieces past the saw. *But*, they also rely on whoever passed that work to them to ensure that the safety guards are in place. Also, to extend the metaphor, the person working the machine doesn't have an easy way to check that the safety features *are actually in place*, so is entirely reliant on the requestor.

A big part of designing safe systems is understanding how things go right over the long term. A big part of this is to ensure that it's easy to do the right thing safely, and *really hard* to do do things unsafely.

Now, with our metaphor it might well be that they have a habit of having the one person manage safety for the person working the machine, but it does however that the workers are exposed to risks that could otherwise be avoided.

Likewise, in our case, the risk isn't a threat to life and limb, but one of confusion–eg: if we allocate the same external ID to two requests, we may think we've fulfilled both, and not provide information that's legally mandated, resulting in an incident. It's not likely in the short term, but most accidents in complex systems arise out of multiple things going wrong. Having spent a long time dealing with systems other folks have designed, I'm really very keen to make it easy for folks working with this down the line to do things safely.

(There's also a lot to be said about how we think about domain-specific logic vs. logic mandated by the technologies, but that's one for another day)

Hopefully, if nothing else, this helps elucidate where i'm coming from.
