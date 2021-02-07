---
date: '2018-03-09'
title: "How software systems learn"
description: 'Homely systems'
---
As part of our weekly tech meeting, we recently watched the first episode of Stewart Brand's series [How Buildings Learn](https://www.youtube.com/watch?v=AvEqfg2sIH0), as a way to prompt discussion on what it means for software systems to be "liveable". <!--more-->

### Flexible foundations
So, buildings, like software systems, have parts that are more or less changeable. For example, as buildings have relatively fixed structures like foundations, or load bearing walls, software has storage systems like databases, or deployment infrastructures like server farms.

Conversely, there are parts that can (and do) change more frequently, such as furniture arrangements, decorations such as wall hangings or seasonal items, just as software has the implementations of it's internal modules, or front-end designs. Or even just how people will make use of the structures presented.

For example, software that tends to form ecosystems like [Emacs](https://en.wikipedia.org/wiki/Emacs), the [Bourne shells](https://en.wikipedia.org/wiki/Bourne_shell) or [Kubernetes](https://kubernetes.io/) tend to provide a kind of platform, or enough core functionality and extensibility that they're easily customisable to how the user wants to use the system, such as defining keyboard macros or modes in Emacs, or customising shell prompts.

They can also be built on top of to create such things as the [Gnus](https://www.emacswiki.org/emacs/GnusTutorial) email client, or ad-hoc processing tools (e.g.: [Doug McIlroy's word count program](http://www.leancrew.com/all-this/2011/12/more-shell-less-egg/)).

Kubernetes is another good example of a system that can organised and modified to suit it's users needs. The user specifies how their software can be deployed in terms of declarative resources, such as a deployment of a set of containers, persistent volumes and claims for storage, and services that provide a uniform way for services to find each-other.

It also provides easy extensibility so [custom resources](https://kubernetes.io/docs/concepts/api-extension/custom-resources/) can manage third party resources, or [operators](https://coreos.com/blog/introducing-operators.html) can be used to provide higher level software abstractions over stateful services.

When you compare this to systems that use docker-compose files based around the _container_ as primary abstraction, additional functionality like service discovery or storage can feel like a second class citizen, and can make it more difficult to manage a cluster of stateful services.

### Applications as items in place

As systems go, applications typically seem to behave more like furniture than a building; at least from the point of view of someone who mainly deals with infrastructure concerns. For one, rather than being designed and then built in place, they're typically built on a developers workstation, and then deployed into a staging and then production environments.

Given that it's often difficult to replicate a production environment on a workstation (although systems like [minikube](https://github.com/kubernetes/minikube) can help), and that production environments can involve more moving parts and indirection (e.g.: packaging an an application into a docker image before it can be spun up by Kubernetes) this can often add overhead to the development workflow that prolongs the cycle time, and makes development less comfortable.

After all, no matter how useful it is to visit the building site and understand how the buildings are assembled, an architect typically doesn't spend their drawing and thinking time on site, as the noise and other activity can provide unwanted distractions. In the same way, furniture designers won't need to spend all of their time in the home where it is to be used.

However, for an application that has a lot of dependencies on it's environment (e.g.: uses multiple external services that don't have stubs), attempting to replicate production locally may seem like the only choice. However, experiencing the discomfort this brings can help to motivate solutions, by encouraging the decoupling of components, or by removing hard dependencies.

### Grand, elegant but unusable designs

And just as how architects can be motivated to produce buildings that look glamorous and modern on the outside but don't suit their purpose. One example Brand mentions is the French central library  required so much re-work to mitigate the windows that trapped heat that would adversely affect the books, they had to save money by _buying fewer books_.

In the software space, this is reflected by developers building on the latest and greatest technology (e.g.: using Hadoop or Spark to process a few hundred Megabytes or even Gigabytes of data), which often delivers more value to their curriculum vitae than it does to the end customer.

There are also more subtle examples such as using Rails and in particular, ActiveRecord for applications with a relatively complex domain model. Whilst it's perfectly possible to write high quality software using these, it's easy to conflate your persistence mechanism with the application logic. So writing tests around that logic is made more difficult as they are implicitly coupled to the database, which will require it's own setup and tear-down, and mechanisms to ensure tests do not interfere with each other.

These kind of systems can even seem great to work with for the first few days or even weeks of use, but often characteristics only emerge after they have been used in production for some time. If you're used to building monolithic systems that are deployed onto a single machine, then having to account for the transient failures possible in dynamic systems like Kubernetes can come as quite a shock.

So, provisioning a volume via say, [EBS](https://aws.amazon.com/ebs/) may transiently fail, and sometimes the only way to fix that seems to be restart or replace the node. If you haven't chosen a storage mechanism that uses replication to mitigate node failures, this will mean an outage for anything that depends directly or indirectly on that storage.

### Ockham's heuristic

So, what this means, is that we should all choose the right tool for the job. Whilst that's easy to say, it comes with the vast pitfall that _understanding_ the right tool for the job usually seems to come with experiences of using the wrong tool for the job. However, reductively, this comes down to [Ockham's razor](https://en.wikipedia.org/wiki/Occam%27s_razor), a heuristic stating that a model with fewest assumptions is probably the best one.

And in software, assumptions can range from coupling between components, or to the nature of the environment, or even development tooling. This isn't to say that we should try to be strictly minimalist, just that we should ensure that we understand and even [record](http://thinkrelevance.com/blog/2011/11/15/documenting-architecture-decisions) the trade-offs we make when deciding how to create systems.
