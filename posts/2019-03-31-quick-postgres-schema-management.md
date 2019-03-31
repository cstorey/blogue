---
date: '2019-03-31'
title: "Quick database change management without external tools"
description: 'Ad-hoc schema management, but less so'
---

Most applications use persistent data in some form or another, and because the data itself will change more slowly than the application code, we'll often need to change it's representation over time to match changes in the application.<!--more-->

Recently for work, I've been looking at improving our schema management tools around Cassandra, and as part of that I've been considering what the smallest possible thing that would work for schema management is.

## The core

At it's core, we want a few things:

 * Check what changes have already been applied,
 * Deduce which changes still need to be applied.
 * Apply them, and record that.

Some other systems (such as [flyway](https://flywaydb.org/)) also provide additional features, such as:

 * Verify that applied changes haven't changed.
 * Describing schema changes in your favourite programming language

Verification ensures that the actual state of the database hasn't diverged from what what is in source control. Whilst it's possible to deploy systems without this feature, the absence of the feature means there's a greater risk of production accidents. For example, if someone mistakenly updates a schema change that has already been deployed, then the application might end up asking for things the database doesn't actually have.

Whilst verification means that you can still edit deployed schemas in source control, it provides a fail safe, so we know there's something wrong while deploying the application, rather than from server runtime errors.

Many systems also provide a way to describe database schema changes in the programming language of the day. While this allows developers to avoid having to dig into the details of the underlying database immediately, in my experience it's best to expose the details sooner rather than later. It's also not truly essential to solving the problem.

## Feature testing

One approach I've used before is to reflect on the current database state (via the [system catalog tables](https://www.postgresql.org/docs/10/catalogs.html)), and use explicit tests to see if we should apply a change, as in [schema.sql from pg-queue](https://github.com/cstorey/pg-queue/blob/bfc507e983315d2da31785ec2d1a9e1a3da9b93d/src/schema.sql).


```sql
DO $$
    BEGIN
        IF NOT EXISTS (
            SELECT true FROM pg_attribute
            WHERE attrelid = 'logs'::regclass
            AND attname = 'tx_id'
            AND NOT attisdropped
        ) THEN
            ALTER TABLE logs ADD COLUMN tx_id BIGINT DEFAULT txid_current();
        END IF;
    END
$$;
```

However, this is less than ideal, since we've ended up with more logic to test if we need to apply the change, than to just apply it. It also relies heavily on the details of the PostgreSQL catalog, which whilst useful, ends up rather verbose.

## A solution

I ended up running onto this on a recent personal project, so figured I'd take an another run at this.

So for example, PostgreSQL provides the [PL/pgSQL](https://www.postgresql.org/docs/10/plpgsql.html), which allows us to express procedural in the database without the need for a seperate driver program. This allows querying the database, as well as [executing dynamic commands](https://www.postgresql.org/docs/10/plpgsql-statements.html#PLPGSQL-STATEMENTS-EXECUTING-DYN). PL/pgSQL doesn't support first class functions, or otherwise passing blocks of statements around as values, so the best we can do is to store them as text.

All of this code can be found as [persistence.sql](https://github.com/cstorey/rustbucks/blob/7f3c2096381827eea93b946d887d16ce4df7b736/app/src/persistence.sql).

Firstly, we need a way to record what's been applied, so far:

```sql
CREATE TABLE IF NOT EXISTS change_log (
    id TEXT PRIMARY KEY,
    md5_digest TEXT
);
```

We use `CREATE TABLE IF NOT EXISTS` since we'll be evaluating this script every time we want to apply any changes, so we need it to be idempotent. `CREATE TABLE` will fail if the table already exists by default, but we're confident we can make any needed changes separately.

Next we have the core of the operation, the `apply_change` function:
```sql
CREATE FUNCTION apply_change(change_id text, change_sql text) RETURNS void AS $$
-- We need to declare local variables at the top, hence this DECLARE block.
DECLARE
    digest TEXT := md5(change_sql);
    known_digest TEXT;
BEGIN
    -- See if the change has already been applied.
    SELECT md5_digest INTO known_digest FROM change_log
        WHERE change_log.id = change_id;

    -- Just log the digest of any applied version, and our calculated digest.
    RAISE NOTICE 'Name: %; Known: %; current:%;', change_id, known_digest, digest;

    -- Check whether it's been applied. If it's NULL, it still needs applying.
    IF known_digest IS NULL THEN
        RAISE NOTICE 'Will apply change % with digest %', change_id, digest;
    -- Otherwise check if the change is different, and fail loudly if so.
    ELSIF known_digest != digest THEN
        RAISE EXCEPTION 'Digest for change % has changed from % to %',
            change_id, known_digest, digest;
    -- If they are the same, we should skip it.
    ELSE
        RAISE NOTICE 'Change % with digest % already applied', change_id, digest;
        RETURN;
    END IF;

    -- Actually apply the change
    EXECUTE change_sql;

    --- Note that we've performed the changes, assuming they succeeded.
    INSERT INTO change_log (id, md5_digest) VALUES (change_id, digest);
END;
$$ LANGUAGE 'plpgsql';
```

And an example of usage would be:
```sql
SELECT apply_change('0001 my change id', $$
    CREATE TABLE documents (
        id TEXT,
        body jsonb NOT NULL,
        PRIMARY KEY(id)
    );
$$);
```

We pass in a human readable name for the change, but then quote the actual change description as text within `$$` delimiters.

## Applicability

As mentioned, the main reason I went for this approach is that it ended up being quicker for me to quickly script something, than it would have been to find a more fully fledged migration management. However, this is effectively a prototype, so it's okay if we have a few things which are slightly inelegant.

Whilst it works, though, for anything larger or longer term I'd want a more flexible implementation with better tooling.
