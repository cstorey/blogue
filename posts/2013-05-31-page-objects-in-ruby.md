---
date: '2013-05-31'
orig_url: http://www.lshift.net/blog/2013/05/31/page-objects-in-ruby
title: Page objects in Ruby
description: Your UI is a domain, too.
---
<div class="content" html="http://www.w3.org/1999/xhtml">

Because “clicking a button” should not exist in your domain language.

When we write tests, we want to be able to write them in a way which is
relevant to the problem the application (or sub-component) solves,
rather than how it is implemented. So, if you get an urge to
[reimplement natural
numbers](https://www.lshift.net/blog/2011/11/26/randomly-testing-ruby),
then you want to avoid testing them in terms of the objects under the
hood for several reasons. A big one being that the tests will be coupled
to the implementation, meaning that they will likely break if you change
how the code works.

<span id="more-1777"></span>

Rather than that, it’s almost always best to test a module in terms of
how it interacts with the outside world, and preferably in terms of how
it is used by the client code. No doubt this is old hat for a lot of
readers, but let’s see how this applies to system tests for a web-based
application.

During the course of writing an experimental project, I needed to write
some tests to describe [importing a deck of
cards](https://github.com/cstorey/srsrb/blob/de13a2645adeed2ff1cf32ce63cd96852f222e17/spec/09_system/skeleton_spec.rb#L185-L197):

    it "should be possible to import an Anki deck preserving history" do
      importer = browser.get_import_page
      importer.upload hangul_anki
      card_list = browser.list_cards
      expect(card_list).to have(41).cards
      card = card_list.card_with_question '아'
      expect(card).to have(3).card_fields
      expect(card['Hangul']).to be == '아'
      expect(card['Romanized']).to be == 'a'
      # We have this here for completeness; we do not actually however support sound.
      expect(card['Sound']).to be == '[sound:ko_vx_1.mp3]'
    end

So, this tries to tell us a story about what a user would do, albeit
with a few concessions to the fact we only expect developers to read it.
In this case, we import an flashcard deck called `hangul_anki`
([Anki](http://ankisrs.net/) being the source of the data), we verify we
have enough cards, and that we find the data we expect in each of the
fields for the card.

This kind of approach has a few benefits, such as looser coupling
between the tests and the markup (so it’s harder for UI changes to break
your tests). Also, because you’ve defined an abstraction over your
application, It is a lot easier to re-use code between tests of
different features.

On the other hand, it’s still a bit more code that you need to maintain,
and you need to have confidence that your driver code isn’t faking
results, which we can obtain by making the driver code do as little as
possible.

For the driver code, I’ve chosen a design which slightly resembles the
repository pattern, but the returned objects (`importer` and `card` in
this case) are stateless, and tie back to a central capybara driven
browser session.

The main code for the page objects is to be found in
[`spec/review_browser.rb`](https://github.com/cstorey/srsrb/blob/master/spec/review_browser.rb).
Every time you request a page from the top-level `ReviewBrowser` object,
[we tell the browser to do something, and then parse the resulting
page](https://github.com/cstorey/srsrb/blob/de13a2645adeed2ff1cf32ce63cd96852f222e17/spec/review_browser.rb#L56-L59):

      class ReviewBrowser
        include RSpec::Matchers
        def initialize app
          self.app = app
          self.browser = Capybara::Session.new(:rack_test, app)
        end

        def list_cards
          browser.visit '/editor/'
          parse
        end

        def parse
          id = browser.find("div.page[1]")[:id]
          fail "No id (#{id.inspect}) found in page:n" + browser.html unless id
          case id
          when 'question-page'
            QuestionPage.new(browser, self)
          #…
          when 'card-editor-list-page'
            CardEditorListPage.new(browser, self)
          #…
          else
            fail "No page id recognised: #{id}"
          end
        end
      #…
    end

And so, when we visit the page containing the list of cards, and we want
to verify how many items there are; we use
`expect(card_list).to have(41).cards` where `CardEditorListPage#cards`
(apparently, naming isn’t my strongest skill) is defined as such:

    class CardEditorListPage < Page
      def cards
        browser.all('.card')
      end
    end

Where the `Page` class is mostly just:

      class Page
        include RSpec::Matchers
        def initialize browser, parent
          self.browser = browser
          self.parent = parent
        end
      end

So, really, we’re just using plain old-fashioned ruby to write an API
wrapper for our application, that allows us to keep our tests isolated
from the details of the implementation, and hence help make our code
easier to change.

</div>
