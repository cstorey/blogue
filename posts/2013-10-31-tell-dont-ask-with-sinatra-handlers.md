---
date: '2013-10-31'
orig_url: http://www.lshift.net/blog/2013/10/31/tell-dont-ask-with-sinatra-handlers
title: Tell don’t ask with Sinatra handlers
---
<div class="content" html="http://www.w3.org/1999/xhtml">

In Bigwig, in order to keep our code neat and well factored, we’ve tried
to adhere to the principle of tell, don’t ask as much as we can.
However, one place this can be difficult is within a handler for an HTTP
request (we’re using Sinatra for that).

<span id="more-2122"></span>

Why? Well, generally, things are simple ehough if you only have to worry
about the happy path, as in [this gist of
strawman.rb](https://gist.github.com/cstorey/3b08f95add9f166e3211#file-strawman-rb).

However, things get a bit more awkward if you need to handle certain
kind of failure, eg: An invalid form submission, or services being
temporarily unavaliable because of a network partition. Sadly, in
reality, things can be somewhat more akward, and handler methods like
this tend to accrete cases if we’re not careful. However, we can make
things neater by pulling out the application logic into a seperate
interactor object (I’ve also heard it described as a use-case object).
So, for an example somewhat inspired from our API code, see
[handler.rb](https://gist.github.com/cstorey/3b08f95add9f166e3211#file-handler-rb),
and
[interactor.rb](https://gist.github.com/cstorey/3b08f95add9f166e3211#file-interactor-rb).

So we’ve got the HTTP adapter in ProvisionAPIHandler, and an interactor
in ProvisionRequest which takes a plain ruby dictionary representing the
request, checks that it makes sense, and passes it into the model.
Naturally, you can go the whole hog–I’ve been kind of lazy here, but
your domain model (the `prov` object) could have a `#render_on` method
which takes a view renderable as JSON, for example. Also, it makes it
far easier to keep each method small, and thus it makes it obvious when
it’s time to refactor (unlike a method with more than thirty lines which
never gets refactored because it can seem kinda scary).

An additional advantage is that if we want to expose the same options
via another mechanism, eg: via a command line tool, then it’d be trivial
to take our adapters and wrap them with say,
[Thor](https://github.com/erikhuda/thor). Because we make no assumptions
about the environment in our interactor object, it’s easy to re-use it.

</div>
