---
date: '2018-12-18'
title: "Avoiding lost updates in PostgreSQL logs"
description: 'Ensuring the past holds no nasty surprises'
---

One of the several things I'm [idly fiddling with](https://github.com/cstorey) is the notion of using [events as the primary record](https://martinfowler.com/eaaDev/EventSourcing.html) for an application. And to do this, you need a [commit log](https://engineering.linkedin.com/distributed-systems/log-what-every-software-engineer-should-know-about-real-time-datas-unifying) implementation of some description. <!--more-->

# Background

Whilst there are already [several](https://kafka.apache.org/) [implementations](https://eventstore.org/) of this idea, the effort of deploying and managing a sizable new chunk of software for a hobby project would (for now) just put me off. I'm a big fan of [PostgreSQL](https://www.postgresql.org), and I've already made a start on a [log implementation](https://github.com/cstorey/pg-queue/) running on top of it.

The main log storage table looks like this:

```sql
CREATE TABLE IF NOT EXISTS logs (
    seq_id BIGSERIAL PRIMARY KEY,
    written_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    data BYTEA NOT NULL
);
```

This gives us an `seq_id` column, auto-populated with a monotonically increasing sequence. The consumers use this to figure out what they haven't seen using `SELECT * WHERE seq_id > $last_seen_id`. ... _making use of the assumption that if $a.seq_id < b.seq_id$, $a$ has happened-before $b$.

# The problem
The current implementation is perfectly fine when there is only a single producer, when you start to have multiple producers logging to the same place using batched operations (transactions), it it turns out that this can fail quite badly.

PostgreSQL will provide [snapshot isolation](https://en.wikipedia.org/wiki/Snapshot_isolation) by default, so we might assume that multiple writes to the [sequence](http://www.neilconway.org/docs/sequences/) for the `seq_id` column from different transactions would conflict (since you would end up with two transactions writing to the same row and column). However, as an optimisation, the effects of the [nextval()](https://www.postgresql.org/docs/9.6/functions-sequence.html) function (ie: incrementing the sequence number) will never be rolled back, even if the transaction aborts.

However, the main issue is that the order in which rows have their value of `seq_id` assumed may not be the same as which they commit. For example:

![Sequence diagram for faulty event sequence](/images/2018-12-18-postgres-logs/Ordered Logs.svg)

* Client _A_: Starts a transaction, allocates sequence number 1.
* Client _B_: Starts a transaction, allocates sequence number 2.
* _B_: Writes their log entry at 2 and commits the transaction.
* Consumer _C_: Looks for the next unseen value in the log. Finds record 2 from B.
* _A_: Writes their log entry at 1 and commits the transaction
* _C_: Looks for the next entry after 2, and finds nothing.

At this point, we can see the because _A_'s record 1 was observable to _C_ only _after_ it had seen 2, C assumes it's already been seen, and thus never gets read. It would be possible for the consumer to track the items it has already seen, and re-scan the able for items it hasn't seen yet, but we'd then have the problem of seeing item 2 before item 1. When we need to have a total ordering^[We could relax this constraint, but it usually seems easiest to start with strong constraints, and loosen them when needed.] over all items though, this isn't appropriate.

# Approaches
## Naive serialization

We can enforce a total ordering over all records by manually implementing the counter ourselves, either by finding the row with the greatest sequence number, adding one to it and using that for our new record. However, because PostgreSQL will only make records visible to other transactions once they have already been committed, and there is an implicit uniqueness constraint on the `primary key` column (so no two records can have the same primary key value), we end up serializing around the id assignment.

If this was running entirely in memory, this wouldn't be so bad, since the time between assign-id → committed is quite short. However, PostgreSQL needs to (quite reasonably) ensure that any committed data is persistent in the event of a crash, so it needs to flush any writes to disk, before they are committed, and thus visible elsewhere. This means that the critical path now looks like assign-id → disk flush → committed. Since flushing to disk is by far the slowest operation here, it becomes our bottleneck.

Based on some very cursory experiments on a desktop machine, a reasonably decent SSD can only support about 600 ops/s for flush operations, which becomes the limit of how many batches we can write per second. However, for most applications, coalescing multiple records into a single batch will add more complexity than we really want to deal with.

## A quick visit to [CORFU](http://dl.acm.org/citation.cfm?id=2535930)

The CORFU distributed log mechanism (as summarized on [the morning paper](https://blog.acolyer.org/2017/05/02/corfu-a-distributed-shared-log/)) is designed to build out a distributed replicated log on commodity storage. Aside from having a dedicated in-memory sequencer service, it assumes is that storage units contain set of immutable, atomic registers, each of can be written to once and only once. Each register is allocated to a dense sequence of natural numbered positions (ie: 1, 2, 3, 4, etc), so we have both a total ordering of items, and we know that once all items at position $p < n$ are filled, that history is fixed.

The sequencer is in charge assigning positions to clients. However, in because clients might crash before they have fully written their record, and because [consistency is "impossible"](https://www.the-paper-trail.org/post/2008-08-13-a-brief-tour-of-flp-impossibility/), we need to ensure that we can make progress if one client fails between having the sequence number allocated and fully writing it's log entry.

While the protocol would normally run over a cluster of storage units, we can assume a single storage unit:

![Hole filling in CORFU](/images/2018-12-18-postgres-logs/CORFU Commits.svg)

As above, Client A has crashed before writing it's record at position 1, and client B has written at position 2. In this case^[... and I can't claim it's entirely faithful to the original], the consumer notices that position 2 was written before position 1, and runs a recovery protocol. In this case, it writes a dummy record at 1. Because we can't tell whether a client is running slowly or just crashed, we may to pause before we fill the hole.

This approach means we have have a high watermark, which marks the tail end of the log, and a low watermark, where we know all entries have been filled, and are guaranteed to be visible to all clients.

## Taking advantage of Postgres' MVCC

Interestingly, Postgres' implementation of multi-version concurrency control uses a very similar watermark scheme. As described in [How Postgres Makes Transactions Atomic](https://brandur.org/postgres-atomicity), each record is stored with a `xmin` and `xmax`, which determine the span of transactions a record is visible for. There are even [transaction id and snapshot](https://www.postgresql.org/docs/10/functions-info.html#FUNCTIONS-TXID-SNAPSHOT) functions that allow us to find the current transaction id, and which transactions are visible in the current snapshot.

But as we've found, commits don't commit in the same order that they start, or sequence numbers are allocated. On the other hand, we don't _need_ to have all written items be immediately visible to all consumers, as we mentioned above with having a low watermark for records becoming visible to everyone.

We want to only read a record if we know that no current transaction can commit anything that would have "happened before" a record we have read.

We want to ensure we don't read anything where there is an an event that has logically happened before (been allocated a sequence number), but isn't yet visible.

So the main functions of interest are `txid_current()`, `txid_current_snapshot()` and `txid_snapshot_xmin(txid_snapshot)`. We can explicitly record the transaction id against each record using `txid_current()` when we [insert the record](https://github.com/cstorey/pg-queue/blob/955708df92084d887559271c81fbbf007ae900c0/src/schema.sql#L20), and then use `txid_snapshot_xmin(txid_current_snapshot())` to [ensure we only read what is visible to all clients](https://github.com/cstorey/pg-queue/blob/955708df92084d887559271c81fbbf007ae900c0/src/lib.rs#L28-L32).

# Conclusions

So with a little knowledge of how postgres handles transactions (albeit after a substantial detour), we've managed to create a totally ordered log, that isn't constrained by the flush throughput of the disk (taking advantage of Postgres group commit), and ensures that consumers will not skip records.

## Drawbacks

The main downside is that because we're effectively using a global property of each database (the transaction id), and pessimistically only ever read what is visible to all transactions, then a long running transaction can cause readers to stall, even though we are writing new records. This especially a risk when the same database is used for both interactive use (usually short lived requests) and reporting (often using longer running transaction batches).

However, for my usage this seems okay; my main constraints are around familiarity and ease of operation than minimising surprises for other folks down the line. We might well be able to rectify this by using the [`pg_stat_activity`](https://www.postgresql.org/docs/9.6/monitoring-stats.html#PG-STAT-ACTIVITY-VIEW) or the [`pg_locks`](https://www.postgresql.org/docs/9.6/view-pg-locks.html) views to infer which processes are accessing the log and use that to derive a watermark, but because the transaction ids exposed there don't have the epoch marker provided by `txid_current()` and friends, we need to worry about relying further on details of the database that might well change over time, as well as accounting for transaction ids that are known to wrap around.
