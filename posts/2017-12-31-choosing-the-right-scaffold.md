---
date: '2017-12-31'
title: "Choosing the right scaffold"
description: 'Tooling for mere mortals'
---

One thing I've come to realise as I've matured as a developer, is that it turns out I'm merely human. That is somewhat obvious, but<!--more-->  you often hear people opine on various discussion boards that their particular tools (that other people feel are error prone) are _actually_ just fine; as long as you remember to avoid doing any one of a bajillion things that can lead to silent failures. C, or any other language with raw pointers and memory management would be a favorite example here.

I've come to find that I'm much more productive using tools that offer the ability to build structure that means you can find mistakes sooner rather than later. This mostly shows up in having a very strong preference for strongly typed programming languages, such as OCaml, Rust or even Kotlin. 

But in my line of work, it's not that uncommon to be working on a project for a month or three with a given set of tools, and then the next project will have a different set. (One of the downsides if developer empowerment is that sometimes we forget the side effects our choices have on our team mates, amongst others). 

This however means, that unless you end up with the same technologies several projects in a row, it can become very difficult to develop expertise in a technology, rather than mere competency. That's not to say though that we don't have one or two experts on a team. 

So, what can we do? On top of ensuring we have a few experts on each team, it seems prudent to choose tools that make it easier to make guesses about a behaviour and test those in the context of a whole system, or component. 

For example, I've done some degree of work using Ruby and Clojure. In Ruby, thankfully there is a culture of focusing on having fast tests where possible. Whilst you don't have a type system per-se, having well structured methods and reasonably decoupled objects usually means that you can deduce what mistake has been made.

On the other hand, I found Clojure rather less forgiving. The philosophy of everything being a map, and most of the core functions treating nil as an empty collection mean that the source of a type error can be some distance from where you observe the error. To me, it literally feels like building a bridge with dry sand.

Now, that's not to say that languages such as Clojure has no value. There is definitely a way to write code that is "Clojure-like", and I clearly haven't found it. Not to mention that you can do some very elegant things with it.

One metaphor I like to use is what kinds of scaffold can the language provide? Types being one such scaffold, tests another. I'm just going to stick with types for now, although this applies to anything that introduces constraints to your development.

Types can allow you to create a scaffold that can guide how you build your application, or at least rule out certain classes of failure. One property that can be very valuable in languages with powerful type systems is parametricity. That is, the ability to say that assuming you don't use any unsafe operations or side effects, a function or module is only capable of certain kinds of operations, by the fact that it doesn't know very much about it's environment or inputs.

In other words, this means you can customize your scaffold to prevent some kinds of mistakes, such as ensuring that some of your application invariants stay unviolated.

In a project context then, this scaffold can make a huge difference when say, setting up a project for the first time, or onboarding a new member. Especially as most projects will have chosen a subset of those available, and it's usually easier to communicate those in a machine checkable form, than in an easily forgotten document.
