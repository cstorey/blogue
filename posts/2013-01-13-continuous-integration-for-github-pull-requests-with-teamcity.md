---
date: '2013-01-13'
orig_url: http://www.lshift.net/blog/2013/01/13/continuous-integration-for-github-pull-requests-with-teamcity
title: Continuous Integration for Github Pull Requests with Teamcity
---
<div class="content" html="http://www.w3.org/1999/xhtml">

Most developers with an interest in open source software these days have
seen the Github interface for handling pull requests, and relatedly,
[Travis CI’s support for pull
requests](http://about.travis-ci.org/blog/announcing-pull-request-support/).
And so we thought it’d be useful to have something similar for our
internal CI system.

<span id="more-1431"></span>

So, Github has the rather nice (albeit poorly advertised) feature that
whenever a pull request is raised, they add a reference for both the
HEAD revision of the remote branch, and a commit which merges said HEAD
revision into master. For example:

    : ceri@misssplendid; git ls-remote origin | fgrep pull
    9d1b88f3a67d2704e08b142b776136ddb1bdf3c1    refs/pull/1/head
    454fd4ab3d439e5b10c68b98ea60fbf7a0bf81cb    refs/pull/1/merge
    80b11080005b5176564c369e1b4053268d3d4efb    refs/pull/2/head
    2222b016401e0b3dc9e7455aeeeeef9a2c4a3b26    refs/pull/2/merge
    358197ab037e5f50417ed7f5ea3d977e53a08006    refs/pull/3/head
    371e7f88898217a72d5bd8322304bc1fc6b8e8ee    refs/pull/3/merge
    : ceri@misssplendid;

Now, it’s possible to [Configure git to fetch pull
requests](https://gist.github.com/3342247) so you can check them out
locally, but we can use this same trick to configure Teamcity (or
another build CI system) to build and test our pull request applied to
master.

Assuming that we already have a configuration to build the master branch
of a project, then under “Version Control Settings”, and within the
appropriate “VCS root”, add the following to the “Branch Specification”
field:

    +:refs/pull/*/merge

Once that’s saved, Teamcity will start polling all references for merged
pull requests from github. Then the next time someone pushes to a branch
referenced by a pull request, then it should show up as a branch under
the build in Teamcity and (if configured to do so) be automatically
built.

We haven’t gotten around to shaving the [Github commit status
API](https://github.com/blog/1227-commit-status-api) integration Yak
just yet, but we’d love to hear if anyone has.

</div>
