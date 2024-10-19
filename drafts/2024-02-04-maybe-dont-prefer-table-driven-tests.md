---
date: '1970-01-01'
orig_url: null
title: "Why you shouldn't always prefer table driven tests"
description: '… and a little refactoring wouldn't go amiss, either.'
---

Notes:
* [Why are non-DRY specs more maintainable?](https://thedailydeveloper.substack.com/p/why-are-non-dry-specs-more-maintainable) — the notion of “locality”.

<!--
TODO:
* Do I consistently use example vs scenario?

-->

Dave Cheney's piece [Prefer Table Driven Tests](https://dave.cheney.net/2019/05/07/prefer-table-driven-tests) has been quite influential in parts of the go community, and certainly where I work. However, like any tool, can be abused, like the [proverbial nail](https://en.wiktionary.org/wiki/if_all_you_have_is_a_hammer,_everything_looks_like_a_nail).

<!--more-->

* What makes Dave's example work?

Dave Cheney's article work, and has become appopriately popular, because it's a very clean, simple, demonstrative example. You can read it, and get an immediate sense of the mechanics involved. But that deserves to be broken down a little more.

* Table driven tests are fine for describing input/output relationships between data

```go
// Include Dave's example here
```

The tests show you the inputs to a function, and the result of a function. And all the interesting examples can fit on a single (reasonably sized) line. And ultimately, it's good example, because that's _all it does_. There are no database or cache lookups, or interactions with external systems or modules. You can see the inputs to the function, it's output, and that's all you need to care about.


* "eye-tracker", or don't make me scroll multiple times to understand a single case.

And most importantly, it's _really easy to read_ and see what's going on. The test can easily fit on a single screen with a reasonable font size. On top of that, it's still easy to quickly identify where the setup, or assertion phases of the tests are.

* However, complexity can spiral

That said, an important part of the learning process for any tool is can be to try and use it in places where it doesn't quite fit, and learning from it. However, that process of getting feedback from the code you're writing (ie: is it easy to write if you do X vs Y?) is a skill in itself, and not one I think we're great at passing on to our peers.

* A terrible, but representative example

Let's say, we need to test a handler in a microservice. Now, this handler has several different scenarios where there's a some common setup, but some important differences between each case. Eg: in one scenario we may need to query data from another service, for example. And it gets worse when we have outcomes besides the return value, such as commanding a third party service to frob a widget or something.

This gets especially bad when we need to vary the setup, or assertions for different examples. At this point, I've seen folks start to introduce function literals into their tables. As an aside, this can get especaily bad if you're using a test double library that's based arround expect/verify, where you effectively have assertions in your setup phase, rather than capturing and later asserting on those interactions.

```go
// Terrible example goes here
```

So in the worst case, we've got:

* Some data for the request
* A setup function
* An assertion function

With reasonable formatting, that'll take up at least ~10 lines for each case, and it's quite likely that the main test body is 10, 20 lines long or even more.

This means, that in order to read through a specific test case, you'll need to:

* Start reading the main test body.
* Find where the test case is in the file, and start reading though it's setup function.
* Try and find where you were in the main test body, until you need to lookup the passed in request inputs.
* Return to the test case, and find the inputs.
* Again, return to the main test body, and carry on on reading…
* until you run into the per-case assertion function,
* So you scroll _back_ to the test case, and read through that.

Assuming that doesn't all fit on a single screen, that's at least four times you'll need to scroll between the test function and the specific example. If you're cunning with "Find usages" in your IDE, you may be able to automate some of this, but that replaces the scrolling with picking though a list of candidate results, and it won't always be clear which result corresponds with your specific example.


So, what do we do instead? We can return to that old staple tool, functions. This will be much easier with an IDE with decent refactoring support, so functionality like inline/extract variable, or inline/extract function. But just be aware, it's best to start by changing one or two secenarios at first, so you can get the feel for it.

The first step will be to extract your main test body into it's own function. like so:

```go
// of extracted test body function
func testScenario(t *testing.T, example myScenario) {
  // …
}
```

Then, we can start to give each individual case it's own top level function (or potentially sub-test, if needs be) and call teh test body function with each specific example struct:

```go
func TestOne(t *testing.T) {
  testScenario(t, myScenario {
    // …
  })
}
```

At this point, we'll need to inline `testScenario` into your top level tests. That'll leave you with something like this:

```go
func TestOne(t *testing.T) {
  scenario := myScenario {
    // …
  }
  // body of testScenario here
}
```

Sadly, this next part will be quite annoying, it's pretty much the inverse of the [replace method with method object](https://refactoring.guru/replace-method-with-method-object) as you'll need to:

* Define local variables for each struct field, and replace references to that field with the local (with any luck, the extract local refactoring on a field reference will work).
* Get rid of the struct initialisation, and directly initialize each variable instead.

So you'll have something like:

```go
func TestOne(t *testing.T) {
  input1 := "foo"
  input2 = "bar"
  expectedResult := "baz"

// body of testScenario here
// using input1 etc directly
}
```

Once you've done one or two of these, you'll probably want to undo the last two steps, and shuffle the order, so you end up creating locals from your struct in `testScenario`, then inline `testScenario`, then manually replace the struct with initializers for the local variables.

This gets more awkward if we have setup/assertion functions, as you'll need to start inlining them into each individual case, taking care to handle any variable name collisions, etc.

```go
// example
```

Once you've done two or three scenarios, while the process can be quite mechanical, you'll get a feel for which bits of the test mean what for the code we're testing. So at this point it's prudent to see if you can extract helper functions. These might not correspond exactly to parts of the original test body, but might even include parts of your example-specific setup / assertion function literals. And don't forget to give them well considered names.

You might object that with all this inlining, there's a good chance that these helper functions probably won't fit on the same screen as the test it's called from. And you'd not be wrong, either! However, IDEs usually make it pretty easy to navigate from a caller function to the callee, and back again, and that's usually easier and quicker than all the scrolling we had to do earlier.

Once you've done this process for all of your scenarios, then hopefully, you'll have a set of tests that are self contained, and clearly describe their role via good naming.
