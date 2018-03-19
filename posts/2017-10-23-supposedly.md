---
date: '2017-10-23'
orig_url: 'https://tech.labs.oliverwyman.com/blog/2017/11/08/supposedly-a-property-test-library/'
title: "Supposedly a Property test library"
description: 'Supposing that my code works...'
---


Over the past few weeks, I've been inspired to create a new property testing library for rust, very much inspired by the work in [hypothesis](http://hypothesis.works/). <!--more-->

Why use suppositions over say, [quickcheck](https://github.com/BurntSushi/quickcheck)? For one, this takes inspiration from [hypothesis](http://hypothesis.works) and [theft](https://github.com/silentbicycle/theft). While it's still in it's early days, the generator system (inspired by [hypothesis' generators](http://hypothesis.works/articles/compositional-shrinking/) means that you don't need a seperate shrinking mechanism for each datatype.

To give an example, I ported my previous [die-hard puzzle](https://tech.labs.oliverwyman.com/blog/2017/05/30/die-hard-statefully/) to this library, and while it's [fairly similar](https://github.com/cstorey/suppositions/blob/fe9883a8577ae15dc831941e932ef98c480df4d6/examples/die-hard.rs), there are a few changes:

```rust
fn ops() -> Box<GeneratorObject<Item = Op>> {
    let g = one_of(consts(Op::FillSmallJug))
        .or(consts(Op::FillBigJug))
        .or(consts(Op::EmptySmallJug))
        .or(consts(Op::EmptyBigJug))
        .or(consts(Op::SmallToBig))
        .or(consts(Op::BigToSmall));
    Box::new(g)
}

fn main() {
    let gen = vecs(ops()).mean_length(1000);
    let config = CheckConfig::default().num_tests(10000);
    config.property(gen)
        .check(|xs| {
            debug!("Testing: {:?}", xs);
            let mut st = State::default();
            for o in xs.iter() {
                st.apply(o);
                st.assert_invariants();
                if st.finished() {
                    debug!("Success! {:?}", st);
                    return Err(st);
                }
            return Ok(());
        });
    panic!("No solution found")
}
```

The main changes are that we don't need a custom `Arbitrary` instance for the `Op` type, as we can build it using combinators, and that the test is expressed as a rust closure, rather than requiring a free function.

In terms of what's implemented, we have generators for most of rust's primitive types, combinators for same, and a fair few examples. It's far from complete though, so what's offered is more in the spirit of a technical preview to interested parties than a full fledged release. Feel free to have a look at [suppositions on crates.io](https://crates.io/crates/suppositions) and poke about the [suppositions repository](https://github.com/cstorey/suppositions). But I'd welcome input on the design and implementation.
