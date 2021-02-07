---
date: '2019-02-03'
title: "Explicit relationships and the principle of least surprise"
description: 'When simplicity is more complex than you might expect'
---


Software developers seem to love describing things as “complex” or “simple”, but it's really quite rare to find someone defining these terms.<!--more-->

John Ousterhout in “A Philosophy of Software Design” comes the closest to this I've seen, in the sense that he defines it as “... anything related to the structure of a software system that makes it hard to understand and modify the system”, going on to talk about the size of the interface compared to the implementation as one metric.

Unless you never work on anything for more than three months, chances are that you'll have spent more time reading and understanding existing systems than writing new components from scratch. And in my experience, just as the value of a component in a system is found in how it relates to others (eg: the user interface in a system made up of people and automation), that's also where the complexity (or incomprehensibility) risk lies.

I'm going to say that there seem to be two ways of looking at this–one of which comes down to counting the number of *components* (whether this be functions, packages, classes, or whatever) in the system, the other by looking at the *relationships* (or interfaces) between these components.

The first approach tends to be typified by frameworks such as [Ruby on Rails](https://rubyonrails.org/) and the like. Access to resources tends to be via globally accessible names via implicit interfaces, so if you know the name of an resource, you can access it. For example, resources like database connection pools are usually defined as an ambient, implicitly used resource (eg: via the [active record](https://martinfowler.com/eaaCatalog/activeRecord.html) pattern).

Conversely, the second approach tends to rely on access to resources via it's explicit, public interface. In this model, each component has the resources it requires explicitly passed in, and will only access resources [via it's own instance variables, or parameters](https://en.wikipedia.org/wiki/Law_of_Demeter). For example, a [repository](https://martinfowler.com/eaaCatalog/repository.html) will tend to be explicitly constructed with a connection pool as a parameter.

So, the main issue as I understand it, is that we want our systems to be comprehensible, and thus components should [express their intent](https://blog.jbrains.ca/permalink/the-four-elements-of-simple-design). Not just by having meaningful names for what they do, but also by making their relationships to the rest of the system clear.

However, the first thing must do in order to manage something, is to find a way to make it [legible](https://www.ribbonfarm.com/2010/07/26/a-big-little-idea-called-legibility/). In other words, to understand the relationships between components, you need to find a way to map them. Systems which only ever use explicit resources make this easier, as everything a component might communicate with will be part of it's public interface. Conversely, when a component has an implicit interface to the rest of the system, it's interface potentially includes _everything else_ in the system.

So we have the problem  (to grossly oversimplify) that potential interactions with implicit interfaces scales with the square of the number of components that *might possibly* interact ($\approx{O(n^2)}$), whereas explicit interface complexity scales with the sum of other components that each component *names* ($\approx{O(kN)}$).

So, ensuring that the relationships in your software are explicit can really help to avoid surprising interactions down the line.
