---
date: '2013-07-31'
orig_url: http://www.lshift.net/blog/2013/07/31/cleaning-tables-with-sequel
title: Emptying the depths of your database with Sequel.
---
<div class="content" html="http://www.w3.org/1999/xhtml">

When writing tests for an application which involves a database, one of
the first things you need to do is ensure sure that all of database
integration tests start from a known state. Now, understandably, there
are already libraries that can do this for us, such as the [Database
Cleaner](https://github.com/bmabey/database_cleaner) gem. However, when
we started the project, I didn’t really see the value in importing a
\~1000 line library when about 10 or so lines of Ruby would do the
trick, and result in a helper which was quickly comprehensible without
having to hunt down the defining Gem.\
 <span id="more-1865"></span>

So, after a few iterations, I ended up
using [Sequel](http://sequel.rubyforge.org/)‘s database reflection code
and the [Topological
Sort](http://ruby-doc.org/stdlib-2.0/libdoc/tsort/rdoc/TSort.html)
module in the ruby standard library. Breaking it down, we want to:

-   Delete all of the application data from the database
-   Not violate any foreign key definitions
-   Exclude certain tables (such those used to track schema versions, or
    reference data)

Toplogical sort is a really quite useful tool from Graph Theory, which
means we can take a directed graph of table dependencies (derived from
foreign keys), and derive a safe order to clean the tables in.

The code and a short example of it’s usage can be found in [this
gist](https://gist.github.com/cstorey/6126229).

One thing that might not be obvious from the code is why we reverse the
sorted order before executing it. The library we use  sorts from the
children to the parents (i.e.: from the referenced table to the table
containing the attribute), whereas if we do that, we delete the data in
a table that may still referenced from another which will violate our
data integrity constraints. So, rather than dropping and re-adding our
data integrity constraints, it’s easier to just reverse the ordering to
parent → child.

Granted, the one minor flaw with this example is that it does not yet
handle circular relationships in the schema. In this case, we’ll compute
an incorrect order, and end up violating some foreign keys. Fixing this
would mean:

-   Computing a graph in terms of per-column references
-   Locating an appropriate point in the graph with nullable references
-   Using UPDATE at that point instead of DELETE.

All of these things would add a degree of complexity; and part of the
benefit of having a small solution like this (even though it may be
technically duplicated lines of code) is that you needn’t worry about
that, or the added risk of bugs, where you don’t need it.

</div>
