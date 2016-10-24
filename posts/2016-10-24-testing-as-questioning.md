---
date: '2016-10-24'
title: Testing as question asking or Hypothesis Driven Development
description: The praxis of What-who-where-when-why .
---
So, my co-worker Ian asks the question “[Why bother testing](http://www.lshift.net/blog/2016/09/14/why-bother-testing/)?”.

I that an under-asked question is how do we think about testing. I would wager, that a sizable majority of programmers (myself included) will usually learn one or two techniques for testing, and then graviated towards those same set of answers for most problems. As an example, just think of how many long-running WebDriver based test suites exist in how many projects, and consider what kind of habits lead to that kind of inertia.

I’d suggest that a more interesting way to approach testing is as a means of asking questions–for one example, [Behavior Driven Development](https://lizkeogh.com/2007/04/16/why-behaviour-driven-development-is-not-an-evolution-of-test-driven-development/) was originally intended to be a way of framing about and discussing the value of a product and it’s features, allowing one to ask “Why is this feature valuable?”. Things like [Impact Mapping](https://www.impactmapping.org/) again, are designed to map out what goals stakeholders have, and what capabilities the software must provide to enable those.

So, even when we’re thinking about planning a project, we’re asking the question of is there an architecture that is fit for purpose, and cost effective in terms of developer and machine time; something you’d normally do in the very earliest phases of the project. In this case, the “testing” is one of doing thought experiments, and using approximate calculations to estimate capacity requirements, etc.

Recently, [Dan Luu](http://danluu.com/) gave a talk at StrangeLoop on [Reasoning about Performance](https://www.youtube.com/watch?v=80LKF2qph6I). In this talk, he sketches out the process of proposing a hypothesis, And during this process, he drills down through several straw-man architectures, considering what data-structures, and access patterns you’d have at each stage, creating a ball-park estimate of what it would cost to process a search query with various architectures. The key part here is that this is iterative, each one learning from the mistakes of the last, until you find something that you believe will fit.

Then during the actual implementation process, this questioning process is just as important, if-not more so. During the course of implementing a feature within an existing system, you’ll be asking how the functionality you’re implementing relates to what’s already implemented.  For example, does this fit within the scope of an existing user-interface component, or do we need to create something new? Or to put it another way, how do we refactor the existing implementation in order to make this feature easy to implement, whilst minimising the risks of introducing problems?

One way to do this is to constantly ask what the smallest change you can make in order to make progress on your implementation. London-style TDD very much leads to this kind of style, and can be described as a kind of “Programming by Wishful Thinking”. So, if you have a module that bridges a web browser with your core application, then it it’s often fair game to provisionally describe the interfaces to the rest of the system as mocks or stubs, allowing you to focus more closely on that module, to get a feel for where it’s responsibilities should lie.

Afterwards, when implementing a module you’d previously stubbed out, you might realise that the work has already been done elsewhere, but slightly differently, in which case you have a choice to refactor, or allow some duplication.

And quite often, you’ll end up finding that you need solve several ostensibly unrelated problems (whether they be bugs or mis-specification) before you can solve the original problem. Which again, involves more questions what to do next.  

But the most valuable thing in all of these to my mind is essentially model formation. Any time you need to make a decision, you need to understand the territory in which that decision plays out, how that needs to change to achieve a goal, the next best step, and then how to act. All of these involve having a model of the system that you’re trying to change, whether that be a business, or a lump of software.

And most importantly of all, there’s the question of how you know your work is useful, or does what the customer needs. Customer-facing tests can be useful, but quite often sitting down with a customer and a concrete prototype can help to clarify misunderstandings about how the system fulfills the customer’s need. Sometimes the best thing you can learn from a chunk of work is that you’re solving the wrong problem, especially if it means you find the mistake two weeks, rather than six-months into the project, saving a chunk of money developing a non-useful system.

In summary, you might even say that a principle of Agile, and especially in Lean-derived approaches, is to always be questioning. It might not look like testing, but the very same patterns of thought are absurdly useful.

To mangle a famous phrase, “Tests are useless, but testing is everything”.
