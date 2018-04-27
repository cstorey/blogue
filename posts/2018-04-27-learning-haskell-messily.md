---
title: Learning Haskell messily
date: 2018-04-27
description: Learning by getting stuck in
---

So, over the past year or so, a few of us at work have been meeting
weekly^[Well, occasionally] to read the book [Haskell
Programming from first principles](http://haskellbook.com/), so having
finally having finished it, I thought I'd give writing something
non-trivial another go.<!--more--> I say another, as I've made a whole bunch of
attempts before, but for one reason or another (eg: too ambitious for my
skills at the time, or mere impatience) gave up on them.

One thing I realised when I was learning a second language, is
that [input](http://www.antimoon.com/how/input.htm), that is reading and
listening to material in the target language is absurdly valuable, even
if you don't actually understand it all.

For example, I would end up watching TV drama shows where, even though I
couldn't actually understand a word of the dialogue, i could just about
infer the plot from how the characters on-screen interacted. And because
those characters were compelling, I'd want to find out what they were
saying, and thus I'd end up going over the subtitles (in the target
language) with a dictionary where needed, and attempting to match them
up with the audio dialogue.

And honestly, sometimes the only way I could match them was via the
timing of the subtitles, as the first task when learning to listen is
just being able to pick out broad structures, say sentences, and then
maybe the roles of words in a sentence from the timing and intonation,
never mind what the word was.

So I'd noted that a similar thing happens with programming language. I'd
learned [OCaml](https://ocaml.org/) years ago, so whilst the broad
shape of ML languages was relatively familiar, things like why you have
'do' notation with a bunch of left arrows in one place and 'let's in
another was kinda beyond me. But sometimes, I'd find a bit of code that
I could broadly follow, and used that as a springboard to dig into how
the code itself was structured, and attempt to develop intuitions and
relationships between how Haskell did things, and other languages did
things.

So, the one thing that Haskell does very differently is how it manages
Effects, or how a program will interact with the outside world. People
will often discuss "Monads", and attempt to explain them, but until
you've got a feel for how lazy evaluation works within Haskell, and that
statements in a program won't necessarily be evaluated in the order they
appear in the program text (at least compared to other languages, like
Java or C and the like), it's difficult to get a sense of why we even
need to care about this seemingly alien construct.

But read enough code, thinking through how you think it probably works
(maybe trying some experiments to see if you're right?) and eventually
you will build an intuition for it.

But of course, it's totally fine to not understand things. Even if you
don't understand half of what the code does, you can still use the other
half to get a feeling for how it uses libraries, or just the shape of
how the source is laid out.

However, reading things is only half of the battle, but if you've read
far more than you write, you've got a far better chance of understanding
how and why other developers express solutions in the way they do. And
besides, when you're learning (which in my case is most of the time)
it's totally fine to try experiments, like copying and pasting an
example, and then changing bits and seeing if they produce say, the
error messages you expect. And if not, then that's something to dig
into.

And when you do feel like starting something from scratch, it's totally
fine to make a complete pigs ear of something, and then either try
something else, or as I've been doing recently, figure out how to
re-factor the code to be neater, or figure out how to take advantage of
these "Monad" things, or whatever.

So, crucially:

*   It's okay to get things wrong.
*   It's natural to feel like you should have done better, but aren't
    quite sure how. It just shows that your judgement is ahead of your
    ability to express it.
*   It's okay to make a mess! Nobody else has to see it, unless you want
    them to.
*   Learning works best when it's done playfully. If you enjoy solving
    crossword puzzles, maybe programs to help do that.
*   It's okay to feel like you're not making progress
*   It's okay to take a break. I've occasionally put things down for a
    couple of months (or years) and come back to find that something
    that seemed opaque, and suddenly realise just that it makes a whole
    lot more sense.
