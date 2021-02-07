---
date: '2013-04-28'
title: Mutant Refactoring powers
description: Knowing your tests are actually worth something.
---
<div class="content" html="http://www.w3.org/1999/xhtml">

Occasionally, I’ve found that when doing some refactorings (especially
when splitting a class for example), it can be all too easy to include
redundant code. Whilst most of the time it’s possible to eliminate that
by a combination of careful inspection and keeping the tests green, I’ve
found that [mutation
testing](http://en.wikipedia.org/wiki/Mutation_testing) tools can
greatly automate the process here.

<span id="more-1737"></span>

For a concrete example, I’m working on a personal [spaced
repetition](http://en.wikipedia.org/wiki/Spaced_repetition) project,
that also serves as an excuse to play with
[CQRS](http://martinfowler.com/bliki/CQRS.html). The idea is that rather
like [Anki](http://ankisrs.net/) each card (fact in Anki terms) can have
multiple fields, and questions and answers are templated.

In this example, I’ve split the [view model
repository](https://github.com/cstorey/srsrb/blob/0c72f4194064e6a15c65142ef7b626fad66b5005/lib/srsrb/deck_view.rb)
into two by areas of functionality; one for [editing
cards](https://github.com/cstorey/srsrb/blob/98037ea93686021c375544b327242b571504eb61/lib/srsrb/card_editor_projection.rb)
(where we see N fields per card), and another for [cards as they would
appear whilst
reviewing](https://github.com/cstorey/srsrb/blob/98037ea93686021c375544b327242b571504eb61/lib/srsrb/review_projection.rb)
(were you just have a front and a back to the card).

However, there’s still some cruft here and there and dead code that we’d
like to get rid of. Mutation testing makes it easier to find this, as
deductively, if you mutate code that is never called, there should be no
changes to the result of your program.

Running [`mutant`](https://github.com/mbj/mutant) over the new
`SRSRB::CardEditorProjection` class, we (eventually) see the following
in the final report:

<pre>
<span style="color:#A00">!!! Mutant alive:
rspec:SRSRB::CardEditorProjection#card_for:/Users/cez/projects/srs-rb/lib/srsrb/card_editor_projection.rb:60:235ac
!!!</span>
 <span style="color:#00A">@@ -1,4 +1,4 @@
 </span> def card_for(id)
 <span style="color:#A00">– cards.get[id]
 </span><span style="color:#0A0">+ cards.get
 </span> end
Took: (4.74s)
<span style="color:#A00">!!! Mutant alive:
rspec:SRSRB::CardEditorProjection#card_for:/Users/cez/projects/srs-rb/lib/srsrb/card_editor_projection.rb:60:81c99
!!!</span>
 <span style="color:#00A">@@ -1,4 +1,4 @@
 </span> def card_for(id)
 <span style="color:#A00">– cards.get[id]
 </span><span style="color:#0A0">+ cards[id]
 </span> end
Took: (4.75s)
<span style="color:#A00">!!! Mutant alive:
rspec:SRSRB::CardEditorProjection#card_for:/Users/cez/projects/srs-rb/lib/srsrb/card_editor_projection.rb:60:f83c4
!!!</span>
 <span style="color:#00A">@@ -1,4 +1,4 @@
 </span> def card_for(id)
 <span style="color:#A00">– cards.get[id]
 </span><span style="color:#0A0">+ id
 </span> end
Took: (4.71s)
<span style="color:#A00">!!! Mutant alive:
rspec:SRSRB::CardEditorProjection#card_for:/Users/cez/projects/srs-rb/lib/srsrb/card_editor_projection.rb:60:9e7ae
!!!</span>
 <span style="color:#00A">@@ -1,4 +1,4 @@
 </span> def card_for(id)
 <span style="color:#A00">– cards.get[id]
 </span><span style="color:#0A0">+ nil
 </span> end
Took: (4.59s)
<span style="color:#A00">!!! Mutant alive:
rspec:SRSRB::CardEditorProjection#card_for:/Users/cez/projects/srs-rb/lib/srsrb/card_editor_projection.rb:60:61efb
!!!</span>
 <span style="color:#00A">@@ -1,4 +1,4 @@
 </span><span style="color:#A00">-def card_for(id)
 </span><span style="color:#0A0">+def card_for(s8e207872a38234087817)
 </span> cards.get[id]
 end
Took: (4.76s)
<span style="color:#A00">!!! Mutant alive:
rspec:SRSRB::CardEditorProjection#card_for:/Users/cez/projects/srs-rb/lib/srsrb/card_editor_projection.rb:60:12b0a
!!!</span>
 <span style="color:#00A">@@ -1,4 +1,4 @@
 </span><span style="color:#A00">-def card_for(id)
 </span><span style="color:#0A0">+def card_for
 </span> cards.get[id]
 end
Took: (4.64s)
</pre>

So, `mutant` has found that for all the ways it knows how to mutate the
method `#card_for` (renaming parameters, deleting code, &c) the result
is that the tests still pass. However, the smoking gun is renaming input
parametersーby renaming the method parameter without renaming their
usages, it’s quite clear that this method never actually gets
calledーotherwise the tests for that mutation would fail with a
`NameError`. And by inspection, we can see it’s not referenced, so we
can easily excise it.

Of course, the original purpose of mutation testing wasn’t just to find
dead code; it’ll also show you where you have missing tests, or an edge
case for a behavior has been missed. For example, it’s quite informative
to run this once with just your unit tests, and then again including
your end to end tests, as that will quite clearly highlight any
short-cuts you might have (even unwittingly) taken in your unit tests.

</div>
