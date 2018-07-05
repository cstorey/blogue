---
date: '2018-07-05'
orig_url: null
title: "Safety II in development"
description: '...'
---

Testing, and the new view of safety?

Before fold.<!--more--> 

One thing that I've found very interesting is the notion of safety culture, and it's parallels with how we build software. In 2013, the organisation in charge of European air traffic control safety released the white paper [From Safety-I to Safety-II](https://www.skybrary.aero/bookshelf/books/2437.pdf), which describes an "old view" of safety (Safety-I) and the "new view" (Safety-II). 

For the full detail, I encourage you to read the white paper itself, but in short, the old view of safety is that we should move from trying to minimize the things that go _wrong_, wheras the newer view is that we should focus on ensuring things go _right_. 

In computer science terms, you can think of this as a shift from preventing bugs, to ensuring that it does the right thing, and importantly how that comes about. I should clarify that the one doesn't come at the expense of the other, and it's safe to assume that if software is so buggy as to be unusable, nothing good will come of your software.

This line of reasoning can be applied at almost any scale; however. From focussing on company culture and strategy, to simply avoiding customer-facing outages.

## Accidents

Accidents in a software company can be anything from accidentally introducing a trivial bug, that you notice and then fix immediately before you commit your changes, finding that a problem is ten times more complex than you thought, to [outages that impact customers](https://github.com/danluu/post-mortems).

In the old world view, this means that we'd end up putting as many checks and gates as possible in the process to ensure that nothing bad goes into production. This would often result in a development process where you end up spending more time waiting for approval and feedback from other teams (eg: architectural review boards, or getting database schema changes approved by the database operations team). Alternatively, you might end up in a situation where the environment is specified so completely ahead of time (eg: mandating technology, or even producing UML diagrams to implement).

This can quite often end up strangling the life and innovation out of your project. If your goal is to implement yet another CMS for a customer's corporate website, then that's a fairly well understood problem. But if you're a consultancy attempting to increase revenue for a company by a certain degree, you're really going to have to experiment. And experimentation requires the ability to fail safely.

The main issue here is the same issue that the original [agile manifesto](http://agilemanifesto.org/) hoped to prevent; because software development is in a sense, crystallising human knowledge in a rigid automated form, you often find that you learn an awful lot about the problem or situation that you didn't recognise. 

In this way, it's rather like figuring out how long a piece of string you need to wrap around the british isles. You might be able to get a rough estimate based on a rough sketch of the outline on the back of an envelope, but implementing the software is very much like ensuring that said string wraps around every detail of the coastline down to the inch. Essentially, as we come closer to the problem at hand (wrapping the Island) you discover that there is far, far more detail involved, and an awful lot more complexity in the details.

And sometimes, these unanticipated details can render an up-front estimate nearly useless. For example, you might find that your back of the envlope estimate only really covers the distance from Brighton to Hastings, say.

## Optimising recovery

So from this perspective, we really want to focus on reslilience, so that we can both limit the scope of accidents and get back on track as quickly as possible. This not only involves ensuring that your feedback loops are as fast as possible (eg: having [Fast tests](https://www.youtube.com/watch?v=RAxiiRPHS9k)), but also ensuring that you have the _right set of feedback loops_. 

In the world of testing, it's very common for an application to have extensive end to end tests. For the web, we often find that people will use [Selenium Webdriver](https://www.seleniumhq.org/) to write automated tests to exhaustively test the correctness of their application. The problem is that given that features will interact in complex ways, so you end up with a combinatorial explosion in how many tests you need, to _guarantee_ correctness.

And because any one of these tests can cover an absurdly large amount of program code, going from a reported failure (often reported as "a thing didn't happen on this page") to the underlying cause can be very time consuming,. I've literally spent three days tracking down a bug, that came down to someone having left a stray `throw RuntimeException("Error")` in production code.

... J.B. Rainsberger talks about this exact problem in [Integrated Tests Are A Scam](http://blog.thecodewhisperer.com/permalink/integrated-tests-are-a-scam). 

So, this exhaustive approach is very much an example of old view safety; in order to avoid bad outcomes, we end up doing more of the same. This is essentially how you end up with test suites that take over half an hour to run. And when you're trying to deliver software quickly, that makes life absurdly slow, and painful. And one of the exciting things about software is that it's usually rare to have the same kind of bug happen twice; so exhaustively testing for known bugs may not do anything to reveal a new, exciting kind of error.

Newer views would take this an opportunity to figure out what approaches work well, such as using the tests to describe how the components interact, and what protocols they need to do that (this is essentially the original purpose of [mock-objects](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.23.3214)).

More recently, we've seen a focus on [micro-services](https://www.martinfowler.com/articles/microservices.html), which again encompasses the notion that each service should focus on providing a single business function, and collaborate with others. One thing that we've found is that smaller things are easier to reason about, but conversely, the more things you have, the [more complex the interactions](https://michaelfeathers.silvrback.com/microservices-until-macro-complexity). 

So in order to ensure good outcomes, we need to ensure that we can tractably reason about the system.



So how does this mean for developing our system? We should ensure that our system has a set of [strong centers](http://www.tkwa.com/fifteen-properties/strong-centers-2/) and [boundaries](http://www.tkwa.com/fifteen-properties/boundaries-2/) ^[both from Christopher Alexander's "[Fifteen Properties of Wholeness](http://www.tkwa.com/fifteen-properties/fifteen-properties/)"].

Strong centers manifest in the sense that each component (or service) should have a single responsibility, or provide a single function that supports some kind of outcome. Ideally, there should be a single, obvious location for functionality (and if not, there's an opportunity to learn and reorganise).

Boundaries mean that there's a clear seperation between functions (eg: maintaining a ledger of transactions vs. reporting on profit and loss), but also well defined interfaces that link them. This focus on the interfaces means that when something depends on several sub-components but some failure occurs; we can compare the information crossing these boundaries with what we'd expect, and use that to pin-point the error. Once we've learned what the failure is, we can decide how to proceed; whether to add some more component-level tests to describe what we've found, add some more constraints to the [contracts](https://martinfowler.com/articles/consumerDrivenContracts.html) that we publish or consume, or even decide that it's okay to handle an edge case manually.


