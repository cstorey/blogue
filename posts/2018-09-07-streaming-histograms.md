---
date: '2018-09-07'
orig_url: null
title: "Streaming Histograms"
description: 'Witty subheading'
---

This week, one of my co-workers gave a short talk on monitoring with prometheus. So, naturally, my thoughts ended up towards how to compute histograms over streaming data.<!--more-->  

In monitoring terms, [histograms](https://prometheus.io/docs/practices/histograms/) are commonly used to monitor latency, and allow you to answer questions like how long did the slowest 1% of database queries take^[The 99th percentile]. Most often you can compute a histogram over a fixed set of data, but in monitoring it's useful for an application to publish statistics in real time.

We could use a sliding window over an incoming stream of samples, but that requires us to store all of the samples in the window, and recompute a histogram every time someone asks for it. So, we would really like to have histogram that doesn't need to store recent samples, and doesn't take up much space.

Most monitoring libraries either have a fixed set of buckets, of require that you specify them. Given that we often want to monitor lots of different kinds, from individal request latency to how long a batch job takes, this can often result in a lot of manual tuning being required, which we'd really rather avoid.

One common streaming implementation is found in the article [A Streaming Parallel Decision Tree Algorithm](http://jmlr.csail.mit.edu/papers/v11/ben-haim10a.html), which describes a way to build [decision tree classifiers](https://en.wikipedia.org/wiki/Decision_tree_learning). However, a key component of the algorthm is a streaming histogram. 

At it's core, we want to model a set of histogram bars (or buckets), but where most histogram data structure require you to pre-configure the buckets, we don't want to make the user specify what boundaries to use. We do need to know how many buckets to keep, though.

We can describe the histogram bars as pairs of the middle point of the bars, and their heights. So to start off with, when we are given a data point, we can make a one unit high bar, and either place it on top of an existing bar, or insert it between others.

However, when we reach the limit of how many buckets we want, we need to find a way to compress the new extra bucket into the rest. This approach will find the two closest buckets, and merge them. 

If we imagine that the two buckets as vertical lines at their centerpoints with hight proportional to the number of items in that bucket, we can can draw a trapezoid with them: (DIAGRAM).

We find a new center for the new bucket, by taking an average of the old bucket centerpoints weighted by their heights, so merging a bucket with a single item with a bucket with 9 items will be 9/10 of the original distance from the first, and 1/10 from the second.

... TODO: How do we get <= buckets from centroids

![...](/images/2018-09-07-streaming-histograms/trapezoid.svg)
![...](/images/2018-09-07-streaming-histograms/binning.svg)


... Algorithm can place items randomly either side of a centroid, but algorithm assumes they are evenly distributed.


... [VividCortex implementation](https://www.vividcortex.com/blog/2013/07/08/streaming-approximate-histograms/)
