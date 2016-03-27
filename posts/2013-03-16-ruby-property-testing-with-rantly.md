---
date: '2013-03-16'
orig_url: http://www.lshift.net/blog/2013/03/16/ruby-property-testing-with-rantly
title: Ruby Property testing with Rantly
---
<div class="content" html="http://www.w3.org/1999/xhtml">

At LShift, we tend to be big fans of functional programming, and in
particular I’ve found ideas from languages like Clojure and Haskell do
influence how I use more mainstream languages such as Ruby.

One technology that’s been useful to us on a current project is
[<span>QuickCheck</span>](http://www.haskell.org/haskellwiki/Introduction_to_QuickCheck2)-alike
for Ruby, [<span>Rantly</span>](https://github.com/hayeah/rantly).
Briefly, rather than testing a module in your code by taking a set of
(hopefully representative) examples of use and demonstrating that they
produce the correct (usually pre-calculated) output, you can have the
library generate input data and compare the results to a model.

<span id="more-1567"></span>

So a fairly simple (and useful) example would be checking message
serialisation. One problem we’ve had to solve is encode commands and
responses to communicate with a remote service. Rather than just using
Marshal or YAML to do this, we want to ensure that we can version
messages correctly, and check that if say, the remote service changes
it’s data model, then we can still interoperate with earlier versions of
the client.

So, for our basic example, we’ll be encoding messages as JSON and
checking two properties:

-   That serialised messages are correctly representable as JSON without
    loss of information.
-   That deserialising a serialised message gives us back what we
    put in.

The example code is up on github, as
[<span>cstorey/rantly-example</span>](https://github.com/cstorey/rantly-example).
Our first example is:

         let (:message) { # 1
             lambda { |r| DoSomething.new id: r.uuid, name: r.string } # 2
         }
         it "should be usefully representable as JSON" do
             property_of(&message).check do |msg| # 3
              json = msg.as_json
              JSON.parse(JSON.unparse(json)).should == json # 4
             end
         end

So, there’s a couple of things to note here

1.  First, we declare how to generate our input data. We’re using a
    lambda here because we need to have a different example object for
    each iteration that Rantly makes, not just per example (which rspec
    does by default)
2.  Rantly passes a context to the block as a parameter, and we use that
    to generate sub-elements of our input (in this case, we define the
    <span>uuid</span> generator further up the file).
3.  Inside the test block, we pass our generator (declared as \#1.) to
    <span>property\_of</span>, and then <span>check</span> repeatedly
    calls the generator, and then passes that to our check block.
4.  And finally within the <span>check</span> block, we can use ordinary
    rspec expectations, or even mocks if appropriate. Here, we are just
    testing for an identity
    ([Hamsterdam](https://github.com/atomicobject/hamsterdam), which
    we’re using to create value objects, gives us value-equality
    for free).

So, you might well be thinking that these tests are trivial, and in a
sense they are, but there is definitely value in them. For example, in
the case of schema versioning, then you can define generators for
different versions of the <span>DoSomething</span> message, and validate
that say, we can parse them into an object that is meaningful within our
domain model, and that they do still accurately reflect the original
intent of the sender.

It’s also possible to build up more complex models as well, modelling
operations as well as code (even up to the point of defining a little
language for data persistence operations), but that’s something to look
at next time.

</div>
