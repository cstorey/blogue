---
date: '2017-05-30'
orig_url: https://tech.labs.oliverwyman.com/blog/2017/05/30/die-hard-statefully/
title: Die-hard Statefully
description: Puzzling quickcheck
---
After reading [Solving the Water Jug Problem from Die Hard 3 with TLA+ and Hypothesis](http://hypothesis.works/articles/how-not-to-die-hard-with-hypothesis/), I figured it'd be amusing to reproduce it in Rust as [diehard-rs](https://github.com/cstorey/diehard-rs), along with it's [quickcheck](https://docs.rs/quickcheck/0.4.2/quickcheck/) library.
<!--more-->

The post above describes the problem quite well, so there's little need for me to reproduce it here.

However, where hypothesis provides a rather nice mechanism for creating command based property tests, we have to do a little extra work ourselves, and effectively create a little language, and associated interpreter. This approach is useful when testing stateful systems (albeit ones that we can usefully seperate from the rest of a potentially noisy system) and has been used to find errors in [LevelDB](http://www.quviq.com/google-leveldb/) and [Riak](https://www.youtube.com/watch?v=x9mW54GJpG0) amongst others.

The core of the test should be reasonably easy; we create a default state; apply each command in turn to the state, and if we have finished, we return false.  False? Hang on.

We return false because quickcheck is normally used to find the smallest input that causes an assertion to fail, wheras we want to find the smallest input that meets our finishing criteria; so we have to invert the logic involved; and pretend to quickcheck that our test has failed.

```rust
quickcheck! {
    fn diehard(xs: Vec<Op>) -> bool {
        // println!("{:?}", xs);
        let mut st = State::default();
        for o in xs {
            st.apply(o);
            st.assert_invariants();
            if st.finished() { return false; }
        }
        return true
    }
}
```

Now, the reason that this kind of property testing is so quick to solve the problem is down to two points:

* Short cut evaluation
* Shrinking

We short-cut evaluation of the sample data; so a working subsequence within the input will abort input as soon as the problem is solved. It's more than likely that if we were to say, not notice that the problem had been solved ...

... so whenever our state machine finds that the big jug contains exactly 4 liters of water, we break out of the loop and return false to quickcheck to indicate that it's a match (normally, this would mean a test failure).

This is something of a conceit, too, as we very much rely on the fact we can generate long sequences of random steps, and hope that a valid subsequence exists somewhere; which vastly increases the chances of this succeeding.

The second part of the secret sauce here, and what makes quickcheck and friends so effective as testing tools, is automatic reduction of test cases.  Quite often, when quickcheck first manages to find a failing test case, it'll be somewhere in the middle of a rather absurdly large amount of randomly generated data. And manually digging through a large amount of (usually) meaningless data is rarely fun.

So, quickcheck will then proceed to remove items from the input sequence (in the case of our `Vec<Op>` input sequence), and re-apply them to see if the input still provokes the failure. In our case, we can see that any steps in the sequence after we have terminate will be ignored, and thus are safe to throw away (although quickcheck has to try this the old fashioned way; just by removing an item and trying the test without it).

Also, we can easily intuit that most operations early in the sequence will have little to impact on a later terminating state, as the range of states in our model is reasonably limited, but also because any subsequence containing `EmptyBigJug` and `EmptySmallJug` steps (but containing both) will empty both jugs, and thus reset us back to our starting state. So intuitively, anything before this pair can be safely removed too.

There are likely other puzzles that we could apply this to, such as the [n-queens puzzle](https://en.wikipedia.org/wiki/Eight_queens_puzzle). But, as we've mentioned, the efficacy of this approach relies on how we encode the problem. We want to take advantage of the backtracking offered to optimise the search, as the range of possible states for other puzzles can be somewhat larger. Hence, you'd likely want to be able to provide more hints to the engine as to how to execute the search.
