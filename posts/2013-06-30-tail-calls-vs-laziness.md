---
date: '2013-06-30'
orig_url: http://www.lshift.net/blog/2013/06/30/tail-calls-vs-laziness
title: Tail calls in functional languages aren’t always a good fit.
description: Avoiding the thunk explosion.
---
Recently I’ve started playing with Haskell a little more seriously, and
I’ve been toying with the idea of using it to calculate approximated
percentiles over streams of numerical data, as found in the
[](http://metrics.codahale.com/getting-started/#histograms)[histograms](http://metrics.codahale.com/getting-started/#histograms)
from Coda Hale’s well known metrics library. The first step in this
process is picking out a representative sample of the input data, as
described in the paper [Random Sampling with a
Reservoir](http://www.mathcs.emory.edu/~cheung/papers/StreamDB/RandomSampling/1985-Vitter-Random-sampling-with-reservior.pdf).
But in the course of writing the code, I learnt a couple of things that
seemed to be worth sharing.

So, the first implementation of the reservoir sampling algorithm looked
like this:…

```haskell
sample :: R.RandomGen g => g -> Int -> [a] -> V.Vector a
sample r n l = go r vec0 (succ n) remainder where
    vec0 = V.fromList beginning
    (beginning, remainder) = splitAt n l
    go _ sample _ [] = sample
    go r sample i (x:xs) = sample ‘seq‘ go r'' updated (succ i) xs where 
  updated | p < n = sample // upd
      | otherwise = sample
    upd = [(idx, x)]
(p, r') = rand0 where rand0 = R.randomR (0, i-1) r
(idx, r'') = rand1 where rand1 = R.randomR (0, n-1) r'
```

When I’ve used functional languages in the past (e.g.: OCaml, or
Clojure) they’ve generally used strict evaluation (i.e.: parameters to
functions are all evaluated before the function is called) unlike
Haskell, which uses pervasive laziness. So, in Haskell, arguments to
functions are only evaluated when they are finally needed to make a
decision, eg: with an `if` or `case … of` statement.

So, if we look at this code, we notice a couple of things:

1.  We tail recurse.
2.  We use the `seq` function to force evaluation of the accumulator
    `sample` (i.e.: the current state of the “reservoir”) before
    we recurse.
3.  It’s not clear where we’re producing information (the updates as a
    updates as a function of the random number generation and the
    input), as opposed consuming it (i.e.: applying any updates to the
    reservoir vector)

Now, both of these things are quite intimately related, and in fact, the
first leads to the second. Most strict functional languages do this with
proper tail calls, so let’s look at how OCaml would treat the above code
(ignoring the call to `seq`). When the interpreter (or equivalent
compiled code) is about to recursively invoke `go` as the final
operation in the function sample, all of the remaining changes to sample
have been made so the interpreter then knows that there’s nothing left
to do within that function call, and so it replaces the current stack
frame with the one for the next invocation of `go`.

However, in Haskell, this can turn out to be less than optimal. In the
initial version, I didn’t have the call to `seq`, and would find myself
getting scary-looking stack overflow errors, which would be all the more
confounding because Haskell doesn’t have a stack in the traditional
sense!

Because Haskell is lazy by default, the updates to our reservoir vector
will only get applied when we want to actually read from it. In the mean
time, the interpreter builds up a list of unevaluated thunks (or
fragments of code and data that it hasn’t bothered to run yet). So,
it’ll end up being something like
`((((sample // upd0) // upd1) // upd2)` where `upd0`.. represent the
pending changes to the vector. So, we can see that for a lot of input,
then you can potentially build up quite a long list of pending changes,
and indeed, when we try to display the final list, the interpreter has
to churn through the pending work. In fact, I would end up with stack
overflow errors when running this code on sufficiently large inputs.

With the additional call to `seq`, we force any pending updates to the
reservoir vector to be applied on each iteration, and so because there
isn’t a huge pending todo list to work through at the end, we can avoid
overflowing the stack.

Sadly, this code doesn’t seem very idiomatic; mostly because of the
third reason above. However, I’d best leave that to another post…
