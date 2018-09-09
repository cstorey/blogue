---
date: '2018-09-07'
orig_url: null
title: "Streaming Approximate Histograms"
description: 'Self Driving Buckets'
---

This week, one of my co-workers gave a short talk on monitoring with prometheus. So, naturally, my thoughts ended up towards how to compute histograms over streaming data.<!--more-->

In monitoring terms, [histograms](https://prometheus.io/docs/practices/histograms/) are commonly used to monitor latency, and allow you to answer questions like how long did the slowest 1% of database queries take^[The 99th percentile]. Most often you can compute a histogram over a fixed set of data, but in monitoring it's useful for an application to publish statistics in real time.

We could use a sliding window over an incoming stream of samples, but that requires us to store all of the samples in the window, and recompute a histogram every time someone asks for it. So, we would really like to have histogram that doesn't need to store recent samples, and doesn't take up much space.

Most monitoring libraries either have a fixed set of buckets, of require that you specify them. Given that we often want to monitor lots of different kinds, from individal request latency to how long a batch job takes, this can often result in a lot of manual tuning being required, which we'd really rather avoid.

One common streaming implementation is found in the article [A Streaming Parallel Decision Tree Algorithm](http://jmlr.csail.mit.edu/papers/v11/ben-haim10a.html), which describes a way to build [decision tree classifiers](https://en.wikipedia.org/wiki/Decision_tree_learning). However, a key component of the algorthm is a streaming histogram.

At it's core, we want to model a set of histogram bars (or buckets), but where most histogram data structure require you to pre-configure the buckets, we don't want to make the user specify what boundaries to use. We do need to know how many buckets to keep, though.

## Insertion

We can describe the histogram bars as pairs of the middle point of the bars, and their heights. So to start off with, when we are given a data point, we can make a one unit high bar, and either place it on top of an existing bar, or insert it between others.

However, when we reach the limit of how many buckets we want, we need to find a way to compress the new extra bucket into the rest. This approach will find the two closest buckets, and merge them.

![Merging buckets $P_i$ and $P_{i+1}$](/images/2018-09-07-streaming-histograms/merging.svg)

In this example, we've found that the closest two buckets are $P_i$ (1 item) and $P_{i+1}$ (3 items).

We find a new center for the new bucket, by taking an average of the old bucket centerpoints weighted by their heights, so merging $P_a$ and $P_b$ results in a bucket $P_m$ $3/4$ of the original distance from the first, and $1/4$ from the second.

We remove both of them (as shown by the dashed line) and replace them with the combined bin shown between them.

## Summation

So, we will typically want to estimate how many samples are less than or equal to a given value, let's call it $b$.

If we imagine that the two buckets as vertical lines at their centerpoints with hight proportional to the number of items in that bucket, we can can draw a trapezoid with them:

![Cumulative sum calculation at $b$](/images/2018-09-07-streaming-histograms/binning.svg)

It's easy enough to see that we need to add up the counts at $P_0$, $P_1$ and so on, but things get a little bit more complicated when we find the closest bin left of $b$ (ie: $P_i$). We assume that half the samples at $P_i$ will be to it's left, shown by the green hatched area in the diagram above.

Because $b$ is between $P_i$ and $P_{i+1}$, we need to calculate how much each will influence the total count at $b$. We do this by imagining a trapezoid between $P_i$ and $P_{i+1}$, but then slicing it off at $b$ (shown in purple). We can then estimate the number of samples between $P_i$ and $b$ by looking at the height of the middle of the trapezoid.

## Implementation and shortcomings.

As I was messing around, I've got the very beginnings of some rust code in [streaming-histograms](https://github.com/cstorey/streaming-histograms-rs/blob/master/src/lib.rs). It's not production ready by any means, but it's a start. There's also the [VividCortex implementation](https://www.vividcortex.com/blog/2013/07/08/streaming-approximate-histograms/), which is (presumably) used in their product.

However, as I was working on this, it occurred to me that there's a major shortcoming when using this with Prometheus. Prometheus describes a histogram as a series of cumulative, each with the count less than or equal to a given threshold. Importantly though, the bucket is part of the label, and because Prometheus creates a time series per set of labels, an implementation that produces bucket thresholds that change will result in an an arbitrarily large number of time-series. And unfortunately, Prometheus isn't really setup for high cardinality metrics, so this gets to be really expensive.

So that leaves us with the open question–how can get get the best of both worlds–having a self tuning histogram with a fixed number of buckets, that doesn't result in an excessive number of time-series in our monitoring?
