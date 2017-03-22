---
date: '2017-03-20'
orig_url: https://tech.labs.oliverwyman.com/blog/2017/03/20/a-taste-of-cloudformation/
title: A Taste Of Cloudformation.
description: Coercing the cloud into your preferred shape.
---
So, we’ve recently had cause to move one of our internal applications to
the cloud; which has largely been an excuse for me to get some
experience in some relatively modern operations technologies. Amazon’s
[CloudFormation](https://aws.amazon.com/cloudformation/) is designed so
that you can declaratively specify the infrastructure resources (eg:
virtual machines, load balancers, container configuration, &c) that your
application needs, and apply changes relatively atomically.

For someone who’s spent a goodly deal of their career dealing with
physical machines that live in a datacenter (so-called
‘[Pets](https://blog.engineyard.com/2014/pets-vs-cattle)‘), this is
something of a revelation. For a start, because everything is specified
in terms of textual configuration (usually JSON, but we’ll get to that);
it can be checked into version control. This might not seem like a big
deal, but it certainly hugely more effective than having to manage a
fleet of hardware, with all of the cabling, inventorying, and fiddling
that involves.

What this buys you principally is leverage. For example, it’s common to
have staging environments for validating applications on before they’re
deployed into production, and one issue with that is that it can be
difficult to maintain parity between those staging, and your production
environments. For one, it’s relatively common to have a simplified
network topology in staging (eg: skipping firewalls, &c) to save on
cost; but that can bite you say, when you introduce new communication
pathways between services. If you haven’t told networking that service A
needs to talk to service B, then you may suddenly find that what works
on stage will mysteriously fail in production. By being able to spin up
an exact copy of your running infrastructure on demand, you minimise the
risk of any unpleasant surprises.

It’s quite common to have different teams responsible for different
parts of the stack–eg: at one place I worked, we had a hardware team who
would fly out to far-flung datacenters, and deploy and manage the
physical kit and networking, wheras the sysadmin team would manage the
operating systems and everything up. So, in a sense, each team presents
an abstraction; the hardware team presents running boxes, and the
sysadmins manage and monitor, as well as providing means to bootstrap a
new deployment.

So, in the same way, we end up with a split of applications in the
cloud. Systems like Kubernetes, Rancher, &c provide a platform, assuming
that all you care about is providing some code artifacts and having them
run; and that the underlying infrastructure can be abstracted away, and
worried about by someone else.

CloudFormation can provide this; but by necessity ends up providing a
lower level of abstraction. So, it’s probably fairer to say that you can
build a platform on top of CloudFormation’s services; but you’ll likely
need to provide your own abstraction on top for this to be workable long
term.

So, wanting to avoid working with the CloudFormation JSON syntax
directly, [Tom](https://tech.labs.oliverwyman.com/author/palfrey/) found
a library named
[Troposphere](https://github.com/cloudtools/troposphere), which allows
you to express configurations as python code, and provides some degree
of configuration linting, too. Expressing the configuration as an
embedded DSL in python allows you to take advantage of the structuring
features of the host language, so in python, you can use classes to
represent sub-groupings of resources (eg: a cluster configuration for
[ECS](https://aws.amazon.com/ecs/)), and those can be consumed by say,
an application instance without it needing to worry about how that
container system was configured.

You can replicate this in plain CloudFormation, by having a number of
separate stacks; and importing references from them; but you’ll end up
needing to specify the names of each stack somehow. You could
potentially end up
[Greenspunning](https://en.wikipedia.org/wiki/Greenspun's_tenth_rule) a
module system from scratch, though, using these imported references and
string concatenation, so using a language that provides modularity feels
more natural.

However, because the python ecosystem provides a packaging mechanism,
it’s entirely possible to publish a python module providing say, 90% of
what you need for a typical application stack, and have the consuming
application inject references to source trees or deployable artifacts,
and provide a deployable configuration. It’s not quite as convenient as
a fully plumbed solution like Kubernetes since you’ll have to integrate
that with your build / deploy mechanisms, but it’s still a pretty good
solution when you need the flexibility.

So; when would I want to use CloudFormation, vs. something higher level?
Well, as mentioned above, the principle use of this is going to be
building infrastructure for others to consume, or for more complex
projects where you need relatively finely grained control over say,
database usage or placement for real-time services and batch jobs.

Conversely though, because CloudFormation provides relatively thin
abstractions; this can potentially make it easier to debug or trace
faults than systems like Kubernetes if you don’t have an expert team
managing it, simply because the more fine-grained control can make it
easier to trace what process is running on which server; even if the
servers themselves are ephemeral, the roles they serve may not be.
