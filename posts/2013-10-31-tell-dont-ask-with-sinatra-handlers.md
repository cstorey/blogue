---
date: '2013-10-31'
title: Tell don’t ask with Sinatra handlers
description: Putting your actions in their place.
---
In Bigwig, in order to keep our code neat and well factored, we’ve tried
to adhere to the principle of tell, don’t ask as much as we can.
However, one place this can be difficult is within a handler for an HTTP
request (we’re using Sinatra for that).

Why? Well, generally, things are simple enough if you only have to worry
about the happy path, as in [this
strawman.rb](https://gist.github.com/cstorey/3b08f95add9f166e3211#file-strawman-rb):

```ruby
class Strawman < Sinatra::base
  post "/foo" do
      begin
        data = validate params
        create_from! data
        redirect_to "/foo/#{data[:id]}"
      rescue InvalidData => e
        render :some_form, locals: { errors: e.errors }
      rescue SemanticError =>  e
        render :some_form, locals: { alert: e.description }
      rescue ServiceUnavaliable => e
        render :sorry_please_come_back_later
      end
  end
end
```

However, things get a bit more awkward if you need to handle certain
kind of failure, eg: An invalid form submission, or services being
temporarily unavaliable because of a network partition. Sadly, in
reality, things can be somewhat more akward, and handler methods like
this tend to accrete cases if we’re not careful. However, we can make
things neater by pulling out the application logic into a seperate
interactor object (I’ve also heard it described as a use-case object).
So, for an example somewhat inspired from our API code, see
[handler.rb](https://gist.github.com/cstorey/3b08f95add9f166e3211#file-handler-rb):

```ruby
class ProvisionAPIHandler < Sinatra::Base
    post "/heroku/resources" do
        require_valid_credentials!
        provision_request.process(json_data, self)
    end

    def provisioned_okay results
        content_type :json
        body render_to_json(results)
    end

    def request_nonsensical reason
        content_type :json
        status 400
        body render_to_json({error: reason})
    end

    def service_unavaliable
        content_type :json
        status 503
        body render_to_json({error: HELPFUL_ERROR})
    end

    private
    def provision_request
            @provision_request  ||= ProvisionRequest.new(...)
    end

    def json_data
        @json_data ||= JSON.parse(request.body.read)
    end
end
```

And [interactor.rb](https://gist.github.com/cstorey/3b08f95add9f166e3211#file-interactor-rb):

```ruby
class ProvisionRequest
  def process request, responder
      when_not_semantically_valid request[:kind], request[:amount] do |error|
        responder.request_nonsensical error
        return
      end

      begin
        prov = domain_model.provision_service! request[:kind], request[:amount]
      rescue SomeKindOfError => e
        responder.service_unavaliable
      else
        responder.provisioned_okay {resource_id: prov.id, locator: prov.location}
      end
  end

  # ...
end
```

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
