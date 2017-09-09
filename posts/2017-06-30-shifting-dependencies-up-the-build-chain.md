---
date: '2017-06-30'
# orig_url:
title: Adding flexibility to build processes
description: Knowing where to break the chain
---

One of the ways we can improve a build, release and deploy process is changing where in the chain we make use of dependencies. <!--more--> For example, people quite commonly want to be able to deploy as frequently as they can, thus iterating quickly and being able to get faster feedback on how valuable users find a feature, or other change.

The first iteration of this might be to implement some python scripts that builds and packages application from source code, then pushes and installs them onto the target matchines. Alternatively, you might end up deploying to AWS, using Chef at instance start time to build and then configure some docker containers, that then then get run from a container scheduler.

In each case, we defer a significant amount of work to deployment or instance start up time respectively, and end up with a large amount of redundant work. As an additional risk, if any services on the critical path (eg: a chef server or registry) are at risk of failure, then not only does that increase the risk of a deployment failure, it can lengthen the time to recovery if you need to redeploy to fix an error.

So in typical software engineering terms, we've ended up with a high degree of temporal coupling in our build process; in that we have several items that may change for different reasons (eg: base system configured with chef and an application docker image) that are forced to change in lockstep. ^[Technically, this might well be the dual of coupling, or perhaps de-coherence, as we have components that change for different reasons forced together, rather than having components that have common reasons to change forced apart.]

In each process, we end up with two parts of the system that can change at different rates, ie: the artifacts output from the build process, and the state of the target systems. For example, you might create build artifacts from feature branches for testing purposes, but only deploy work from the master branch to production, but to many identically configured machines. Following Plato's nation of carving nature at the joints, this difference in rates of change implies we can introduce a degree of freedom into our process.

That's all a rather long-winded way of saying that we can split our process into stages:

 * Creating a deployable artifact or report that can be validated
 * Putting said artifact into service.

So, for example, rather than using chef-client to configure an AWS machine whenever a new instance is created, we could create an AMI that is pre-configured with all of the changes that chef would make; configured ahead of time to start the requisite services on boot. For docker images, we can have a single instance of a registry; which can be seeded with image tarballs (ie: `docker save`) stored on a well-known webserver (eg: S3).

Or, instead of re-building packages every time we deploy; we could publish signed packages to a well-known repository that can be consumed by `apt-get`, `yum` or similar. This also means that if you don't have bit-for-bit reproducable builds, you can still minimise accidental differences in configuration, as well as making software provenance more legible.

The downside to this is that each degree of staging can introduce additional complexity: package repositories need to be considered when thinking about software security, for one. So as with any kind of system, there's a trade off between the additional flexibility granted by the degress of freedom, and the carrying cost.

As a postscript, this was somewhat inspired by ideas from J. B. Rainsberger's [Demystifying the Dependency Inversion Principle](http://blog.thecodewhisperer.com/permalink/consequences-of-dependency-inversion-principle/), and Donella Meadows' [Leverage Points](http://donellameadows.org/archives/leverage-points-places-to-intervene-in-a-system/) applied to software supply chains.
