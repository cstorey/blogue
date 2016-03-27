---
date: '2013-09-28'
orig_url: http://www.lshift.net/blog/2013/09/28/precise-scheduling-with-rabbitmq
title: Precise scheduling with RabbitMQ
---
<div class="content" html="http://www.w3.org/1999/xhtml">

On a project recently, we needed to be able to process jobs
asynchronously, but we also needed to be able to specify that they
should be run at a certain point in the future. We also needed to be
able to implement exponential backoff on failure. We initially tried to
integrate [Sidekiq](http://sidekiq.org/), but unfortunately it turned
out to not be a good fit for the way we structured the code base.

<span id="more-1971"></span>

Now, RabbitMQ is very [commonly](http://www.celeryproject.org/)
[used](http://www.rabbitmq.com/tutorials/tutorial-two-python.html) as a
work queue. Sadly for us, because AMQP was originally designed as a
protocol for integrating various systems, scheduling wasn’t baked into
the core protocol. Thankfully, it’s extensible enough that this turns
out not to be a problem.

One suggestion that often gets suggested is to use a [dead letter
exchange](http://yuserinterface.com/dev/2013/01/08/how-to-schedule-delay-messages-with-rabbitmq-using-a-dead-letter-exchange/).
In this case, it’s not possible to specify per message when we want the
job to be processed. It may well be able to implement this using a
series of cascading work / retry queues, but I can see the potential for
it becoming quite complex quite quickly.

So, alternatively, we decided to make use of the fact that when you use
message acknowledgements, the client can delay sending an
acknowledgement to a message, as long as you can still send the response
over the same channel (AMQP sub-divides a TCP connection into channels)
the message was delivered on. This usually means that you can hold a
claim on a job for as long as the TCP connection to the broker stays up.

So, the very simplest approach one can take here is to add a message
header when sending the message that states when we want the job to be
processed, and look for that on the receiving side. If we take a very
simple example, where we publish via the default exchange to a queue
named in the variable `a_queue_name`:

    broker = Bunny.new(broker_address)
    # ...
    ch = broker.default_channel

    headers = { perform_job_at: scheduled_job_time.to_f }
    ch.default_exchange.publish(a_message, routing_key: a_queue_name, headers: headers)

And in our worker, we can pull it out like so:

    work_queue = broker.default_channel.queue(a_queue_name)
    work_queue.subscribe do |delivery_info, metadata, body|
        scheduled_job_time = metadata.fetch(:headers, {}).fetch(:perform_job_at, Time.now)
    # ... Process job.
    end

However, this is of course, only half of the story. We know when we want
to process the job, but we don’t yet have a method of delaying it. A
degenerate approach would be call `Kernel#sleep` in the subscribe block
until the received item is due, and then process it, but you would then
quite clearly experience [Head of line
blocking](http://en.wikipedia.org/wiki/Head-of-line_blocking), so a job
to be processed very soon, but that arrives after a job that is to be
processed far in the future, would be delayed until the first job had
been processed.

So, to make this work reasonably, we need to use a priority queue that
supports access from multiple threads, i.e. the thread in which the
`Queue#subscribe` block runs in, and the thread that actually does the
work.

We ended up writing our own variant of [ruby’s Queue
class](https://github.com/ruby/ruby/blob/v1_9_3_429/lib/thread.rb#L140)
that wraps a [priority queue](https://rubygems.org/gems/PriorityQueue)
for safe concurrent access, as well as blocking any consumer until a job
is due.

So, you end up with a consumer implementation that looks roughly like
this:

    scheduler = SchedulingQueue.new
    work_queue = broker.default_channel.queue(a_queue_name)
    work_queue.subscribe do |delivery_info, metadata, job_data|
        scheduled_job_time = metadata.fetch(:headers, {}).fetch(:perform_job_at, Time.now)
        # Optionally decode the message body into a domain object
        scheduler.add_job(perform_job_at, job_data)
    end

    loop do
        the_work_item = scheduler.pop
        do_amazing_things_with(the_work_item)
    end

Now, as it stands, we don’t yet support backoff for failed jobs. That
will require a few extra moving parts, which we’ll introduce another
time.

Edit: Thanks to a [twitter
exchange](https://twitter.com/crstry/status/384028735626027008) with
[Sean T Allen](https://twitter.com/SeanTAllen) I was reminded that this
really is only suitable for systems with low throughput. In our case, we
have very few jobs outstanding at any one time (the most we’ve seen is
six), and so performance isn’t a huge problem for us. In different
circumstances we’d have maybe implemented a database-backed service
which fed jobs into RabbitMQ when they become due, but in this case,
would been too costly in terms of developer time.

</div>
