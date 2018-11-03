---
date: '2018-11-03'
title: "Brevity in Failure"
# description: 'Whether to rub errors in your face...'
description: 'When to just die quietly in a corner...'
---
I've been writing a fairly significant amount of [Go](https://golang.org/) for work recently, and found that it's basically workable. But the limited expressiveness of the language, particularly around error handling, does throw a few roadblocks in the way.<!--more-->

## Go

Go's approach to simplicity (in the sense of there's one way to do something) is clearly a big driver for the language's design. The most obvious approach of this how idiomatic Go handles errors. Here's one example [from golang.org](https://talks.golang.org/2012/splash.article#TOC_16.):
```go
f, err := os.Open(fileName)
if err != nil {
    return err
}
```

The claimed value here, in contrast to languages with exceptions, is that because errors are just first class values using structured control flow operators, they are much easier to reason about. And this much is very true. However, with a function that does a significant amount of work, (as in [`cmd/build.doBuild`](https://github.com/wagoodman/dive/blob/951d2c7b711f569f872c4f437d40dfa88895e9a4/cmd/build.go#L26-L47) from [dive](https://github.com/wagoodman/dive)), between about a third to a half of it is made up with boilerplate error handling.

Unfortunately, because the Go compiler lacks a warning for un-used `error` values, unless you know the types for everything used in a function, it can be easy to confuse a function that just has some side effect, with an un-checked error. There are external tools to fill this need thankfully, such as [`errcheck`](https://github.com/kisielk/errcheck).

## The trade-off

It's completely reasonable to expect people to test for certain (predictable) error conditions and either compensate or fail (eg: a missing configuration file might be entirely optional), but more often than not we want to treat error conditions as fatal, at least within a certain scope (eg: per http request).

I've generally found it best to minimise how many places that you have to deal with error conditions, and try to have those _specialise_ in handling and maybe recovering from those errors.

For example, functions that only talk about a [single level of abstraction](https://markhneedham.com/blog/2009/06/12/coding-single-level-of-abstraction-principle/), tend to make code easier to read and to understand, as they tend to separate the outcome they support from how that happens. But because these are generally implemented in terms of other functions that may fail, we have to allow for that, and handle each one explicitly. To me, this forces you to write boilerplate that repetitious, and impacts clarity.

## Rust

[Rust](https://doc.rust-lang.org/1.0.0/book/error-handling.html)'s [error handling](https://blog.burntsushi.net/rust-error-handling/) is visually a lot more compact^[although as you might [guess](/posts/2016-01-27-rules-based-mio-chat-example.html), I have an [obvious](http://cez-desktop.local:8000/posts/2017-05-30-die-hard-statefully.html) [bias](http://cez-desktop.local:8000/posts/2017-10-23-supposedly.html)]. So, the above example would just be:

```rust
let mut f = File::open(file_name)?;
```

Personally, I think that the use of a `?` operator^[Older versions used a `try!()` macro, rather than the `?` operator] offers a good compromise. It's effectively syntactic sugar for something like:

```rust
let mut f = match File::open(file_name) {
    Ok(f) => f,
    Err(e) => return Err(e)
}
```

Ie: check the result, if it's an error return it, or else just carry on with the function.

The rust compiler also has a mechanism to warn when certain types go-unused, as is the case for `Result<T, E>` values that can carry either a result value or an error.

Given what we've said above about there only being a few places where we want to really care about errors, this makes a reasonable trade-off between being explicit (as in Go) and the implicit approach taken with exceptions. It's possible to see that the operation can fail, but it doesn't interrupt the visual flow of the code.

This is a good example of the kind of simplicity that rust emphasises, the sense of embodying a single concept well where you need it, and having it be unobtrusive otherwise.

## Conclusion

Given that Go takes an awful lot from C, I can understand why it's chosen to throw error handling right in your face, but the absence of a built-in compiler warning for un-used error results does seem like an odd choice. However, the [proposed error handling](https://go.googlesource.com/proposal/+/master/design/go2draft-error-handling.md) improvements for Go 2 do look promising.