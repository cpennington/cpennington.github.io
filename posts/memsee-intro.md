---
title: A Tale of Two Leaks: An Introduction to Memsee
published: 2015-02-10
---

This is the first of a 3-part series of posts detailing my investigations into
two separate memory leaks in [edx-platform][]. In this post, I'll describe [memsee][],
a tool built by [Ned][] and me during a previous memory investigation which can help to
provide insight while diagnosing a leak.

<!--more-->

# Design

Memsee is intended as a tool to interactively investigate memory usage in a snapshot
taken from a Python process. It is built to load in a memory dump into a SQLite database,
and then provide a REPL for querying that database, naming objects for future investigation,
and performing various graph-based queries to determine how objects are related.

The basic SQLite schema used is quite simple: it consists of a table for objects (named `obj`)
and a table of references between objects (named `ref`). The schema for the two tables is
reproduced below:

~~~ sql
CREATE TABLE obj (
    address int primary key,  -- the memory address of the object
    type text,                -- the type of the python object
    name text,                -- the name of the python object (for things like functions and classes)
    value text,               -- the value of the object (for strings and ints)
    size int,                 -- the amount of memory allocated to the object
    len int,                  -- the length of the object (for lists, etc)
    mark int,                 -- whether the object is reachable from
                              --   the root (used during memsee garbage collection)
    repr text                 -- a rough approximation of the python repr of the object
                              --   (limited by the information that is dumped by meliae)
);
CREATE TABLE ref (
    parent int,               -- the memory address of the object holding the reference
    child int                 -- the memory address of the referenced object
);
~~~

# Commands

## Starting Memsee

~~~ bash
> python memsee.py [DATABASE]
~~~

Start `memsee`. If `DATABASE` is supplied, then connect to that database on startup.
Will not create the database if it doesn't exist.

## Setting Up A Database

### create

~~~ bash
::> create DATABASE
~~~

Create a new memsee database to work in, and connect to it.

### read

~~~ bash
::> read FILE
~~~

Read a [meliae][] memory dump into the active database as a
new generation.

### open

~~~ bash
::> open FILE
~~~

Connect to a previously created memsee database.


## Inspecting objects

### select

~~~ sql
::> select * from obj where type = 'dict' limit 10

#          address type name value   size   len mark   repr
-------- --------- ---- ---- ----- ------ ----- ------ ----------
#0.0      39981760 dict ∘    ∘       1048    13 ∘      dict
#0.1     118896112 dict ∘    ∘       1048    10 ∘      dict
#0.2      31427024 dict ∘    ∘       1048    20 ∘      dict
#0.3      31172288 dict ∘    ∘       3352    29 ∘      dict
#0.4      31463552 dict ∘    ∘        664     8 ∘      dict
#0.5      16117648 dict ∘    ∘       3352    71 ∘      dict
#0.6      20850512 dict ∘    ∘       1048    10 ∘      dict
#0.7      16117360 dict ∘    ∘      12568   144 ∘      dict
#0.8      16503472 dict ∘    ∘       1048     7 ∘      dict
#0.9      16281728 dict ∘    ∘       3352    41 ∘      dict
#0.10     16246816 dict ∘    ∘      12568   235 ∘      dict
~~~

Execute a SQL select query against the connected memsee database.

## Substitutions

Memsee will perform a number of substitutions in select commands that make traversing
the object graph easier.

### Children

`&`, when appended to a memory address, means "the address of all child objects",
and could be understood as

~~~ sql
1234&  <==>  (select child from ref where parent = 1234)
~~~

This suffix can be repeated to traverse multiple level of the object hierarchy.

### Parents

Similar to `&`, `^` selects the memory address of parents of the target address.

~~~ sql
1234^  <==>  (select parent from ref where child = 1234)
~~~

## Tree Traversal

### path

The `path` command searches from one set of objects to another, following references
from the currently selected set until it finds a path to the destination set.
It prints the objects in the first path that is found.

~~~ bash
::> path from "QUERY" to "QUERY" [reversed]
~~~

The `reversed` argument causes the traversal to follow references backwards (from
child to parent).

#### Example

~~~ bash
::> path from "address = 110810832" to "address in 0&" reversed
Added 162 paths to newly discovered nodes
Added 224 paths to newly discovered nodes
Added 73884 paths to newly discovered nodes
Added 73698 paths to newly discovered nodes
Added 114334 paths to newly discovered nodes
Added 40842 paths to newly discovered nodes
Added 38 paths to newly discovered nodes
Added 38 paths to newly discovered nodes
Added 76 paths to newly discovered nodes
Added 304 paths to newly discovered nodes
Added 304 paths to newly discovered nodes
Added 1634 paths to newly discovered nodes
Added 1520 paths to newly discovered nodes
Added 18278 paths to newly discovered nodes
Added 567302 paths to newly discovered nodes
#                 address type                 name                 value     size   len mark  repr
-------- ---------------- -------------------- -------------------- ------- ------ ----- ----- ----------
#0.0            110810832 XMLModuleStore       ∘                    ∘           64     ∘ ∘     XMLModuleS
#0.1            109985944 list                 ∘                    ∘          104     3 ∘     list
#0.2            111043728 dict                 ∘                    ∘         1048    12 ∘     dict
#0.3            110780112 MixedModuleStore     ∘                    ∘           64     ∘ ∘     MixedModul
#0.4            111033776 dict                 ∘                    ∘          280     1 ∘     dict
#0.5            110809360 LibraryToolsService  ∘                    ∘           64     ∘ ∘     LibraryToo
#0.6            111031056 dict                 ∘                    ∘          280     5 ∘     dict
#0.7            111034352 dict                 ∘                    ∘         3352    41 ∘     dict
#0.8            110779280 LmsModuleSystem      ∘                    ∘           64     ∘ ∘     LmsModuleS
#0.9            111050032 dict                 ∘                    ∘         3352    26 ∘     dict
#0.10           110783296 module               open_ended_grading.u ∘           56     ∘ ∘     open_ended
#0.11            12611920 dict                 ∘                    ∘       786712  6069 ∘     dict
#0.12            12619664 dict                 ∘                    ∘         3352    73 ∘     dict
#0.13     140244885039992 module               sys                  ∘           56     ∘ ∘     sys
#0.14            13270096 dict                 ∘                    ∘         3352    54 ∘     dict
#0.15          1294669280 frame                ∘                    wait       512     ∘ ∘     frame
~~~


This series continues in <a href="/posts/memsee-pt1.html">Part 1</a> where I describe tracking
down a single large memory leak.

[edx-platform]: https://github.com/edx/edx-platform
[meliae]: https://pypi.python.org/pypi/meliae
[memsee]: https://github.com/nedbat/memsee
[Ned]: http://nedbatchelder.com/
