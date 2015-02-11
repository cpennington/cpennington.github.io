---
title: A Tale of Two Leaks (Part 1)
date: 3000-01-01
---

Over the past several weeks, we've been contending with several memory issues on edx.org.
The first manifested as a sudden increase in the resting memory footprint of our production
web-processes. The second presented as a classic memory leak while doing offline grading
of an entire course. In this post, I'll go though the steps and tools we used to identify
the causes of the first of these leaks.

<!--more-->

Instrumentation
===============

The first step in any debugging task is gathering information. In our case, this was tricky
initially, because python has no built-in facility for exporting information about what is
using memory in a running process. So, the first step was to integrate such a facility into
[edx-platform][].

In previous investigations of memory usage, I've found [meliae][] to be an extraordinarily useful
tool for gathering statistics about the objects resident in memory in a python process.
To integrate it into [edx-platform][], I added a signal handler during process startup.

~~~ {.python .pre-scrollable .pre-x-scrollable}
# lms/wsgi.py
import openedx.core.operations
openedx.core.operations.install_memory_dumper()

# openedx/core/operations.py
import os
import signal
import tempfile

from datetime import datetime
from meliae import scanner


def dump_memory(signum, frame):
    """Dump memory stats for the current process to a temp directory. Uses the meliae output format."""
    scanner.dump_all_objects('{}/meliae.{}.{}.dump'.format(tempfile.gettempdir(), datetime.now().isoformat(), os.getpid()))

def install_memory_dumper(dump_signal=signal.SIGPROF):
    """
    Install a signal handler on `signal` to dump memory stats for the current process.
    """
    signal.signal(dump_signal, dump_memory)
~~~

This signal handler defaults to using `SIGPROF` as the signal to trigger the memory dump, because
that signal isn't already in use by [gunicorn][] for worker management. However, other runtime
environments might need other signals (I wasn't able to figure out how to get [uWSGI][] to pass
signals into its worker processes during my testing).

With this instrumentation in hand, we were able to collect memory dumps from processes exhibiting
both of the different memory leaks.

Stable Memory Increase
======================

The first memory "leak" seemed less like a classical leak than unintentionally large memory use.
Our production workers (which used to sit at ~1.2GB of memory) started to consume ~1.8GB during
a steady state instead. Fortunately, we were able to reproduce the same issue on staging, as well,
shortly after a worker had started. With the help of our devops team, I collected a memory dump
and began analysis.

The primary tool I used for debugging both of these memory leaks was [memsee][], a small python
command written by [Ned] and I for a previous memory leak investigation. In a nutshell, memsee
is a thin wrapper around a [SQLite][] that adds additional methods and shortcuts for navigating
the set of objects and references that [meliae][] dumps out.

I've recreated my investigative process (with some dead-ends elided) in memsee below:

~~~ {.bash .pre-scrollable .pre-x-scrollable}
> python memsee.py
# Initialized a database to store the dump
::> create stage-dump.db

# Import the dump data
::> read stage-dump.meliae
Reading.......................................................................................
..............................................................................................
..............................................................................................
..............................................................................................
..........................................
Marking top objects... 2940
4121623 (4.1M) objects and 14969737 (15.0M) references totalling 1220387792 (1.2G) bytes (542.6s)

# Find out which objects are most common
::> select count(*), type from obj group by type order by 1;

<snip>
#0.1068        5982 datetime.datetime
#0.1069        6227 DictFieldData
#0.1070        7192 SequenceDescriptorWi
#0.1071        8263 weakref
#0.1072       12928 cell
#0.1073       14485 float
#0.1074       14618 CustomTagDescriptorW
#0.1075       18210 int
#0.1076       18810 VideoDescriptorWithM
#0.1077       26786 CapaDescriptorWithMi
#0.1078       29140 DiscussionDescriptor
#0.1079       37534 code
#0.1080       39182 VerticalDescriptorWi
#0.1081       49247 function
#0.1082       82098 tuple
#0.1083      141493 InheritanceKeyValueS
#0.1084      141493 KvsFieldData
#0.1085      147720 ScopeIds
#0.1086      148333 BlockUsageLocator
#0.1087      382851 list
#0.1088      404395 unicode
#0.1089     1006998 str
#0.1090     1306709 dict
~~~

Observe that there are a lot of KvsFieldData objects resident in memory. Let's find
out what's referencing them.
Some notes about memsee syntax:

  - `path` takes the form `path from "$where_clause" to "$where_clause"`. The first clause
    specifies which objects to begin the search from, and the second specifies which objects
    to terminate on.
  - `0&` means 'the children of the object at address 0', and 'address 0' is a virtual
    object that is assumed to be a parent of anything in the system that doesn't already
    have a parent.
  - `path` only returns the first path it finds from one set of objects to another.

~~~ {.bash .pre-scrollable .pre-x-scrollable}
::> path from "address in 0&" to "type = 'KvsFieldData'"

Added 20000 paths to newly discovered nodes
Added 12361 paths to newly discovered nodes
Added 4462 paths to newly discovered nodes
Added 19960 paths to newly discovered nodes
Added 39083 paths to newly discovered nodes
Added 84670 paths to newly discovered nodes
Added 209591 paths to newly discovered nodes
Added 231865 paths to newly discovered nodes
Added 291043 paths to newly discovered nodes
Added 240986 paths to newly discovered nodes
Added 104132 paths to newly discovered nodes
Added 107793 paths to newly discovered nodes
Added 263869 paths to newly discovered nodes
Added 3392646 paths to newly discovered nodes
Added 3502907 paths to newly discovered nodes
#                 address type                 name                       value                 size        len mark       repr
-------- ---------------- -------------------- -------------------------- --------------- ---------- ---------- ---------- ----------
#0.0           1294669280 frame                ∘                          wait                   512          ∘ ∘          frame
#0.1             13270096 dict                 ∘                          ∘                     3352         54 ∘          dict
#0.2      140244885039992 module               sys                        ∘                       56          ∘ ∘          sys
#0.3             12619664 dict                 ∘                          ∘                     3352         73 ∘          dict
#0.4             12611920 dict                 ∘                          ∘                   786712       6069 ∘          dict
#0.5             47884224 module               xmodule.modulestore.django ∘                       56          ∘ ∘          xmodule.mo
#0.6             47993568 dict                 ∘                          ∘                     3352         31 ∘          dict
#0.7             88346448 MixedModuleStore     ∘                          ∘                       64          ∘ ∘          MixedModul
#0.8             88505440 dict                 ∘                          ∘                     1048         12 ∘          dict
#0.9             24983952 dict                 ∘                          ∘                    12568        230 ∘          dict
#0.10           109863312 XMLModuleStore       ∘                          ∘                       64          ∘ ∘          XMLModuleS
#0.11           110265552 dict                 ∘                          ∘                     1048         17 ∘          dict
#0.12           110264976 dict                 ∘                          ∘                     3352         62 ∘          dict
#0.13          1023398416 CourseDescriptorWith ∘                          ∘                       64          ∘ ∘          CourseDesc
#0.14          1023827696 dict                 ∘                          ∘                     1048         15 ∘          dict
#0.15          1023398352 KvsFieldData         ∘                          ∘                       64          ∘ ∘          KvsFieldDa
~~~

Ok, so this `KvsFieldData` is being held in memory by the `XMLModuleStore`. Thats not too surprising (we expect
many XModules to be held in memory by that Modulestore, in fact). However, in the original investigation, I
a different module (not `xmodule.modulestore.django`, as listed above) was holding on to the pointer to
`MixedModuleStore`. This should never happen, because the `MixedModuleStore` is supposed to be a single instance,
used globally.

~~~ {.bash .pre-scrollable .pre-x-scrollable}
# Let's look at the two XMLModuleStores.
::> select * from obj where type = 'XMLModuleStore'
#                 address type                 name                 value                size        len mark       repr
-------- ---------------- -------------------- -------------------- -------------- ---------- ---------- ---------- ----------
#1.0            110810832 XMLModuleStore       ∘                    ∘                      64          ∘ ∘          XMLModuleS
#1.1            109863312 XMLModuleStore       ∘                    ∘                      64          ∘ ∘          XMLModuleS

# We can find the path to the root for each of them
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
#                 address type                 name                 value                 size        len mark       repr
-------- ---------------- -------------------- -------------------- --------------- ---------- ---------- ---------- ----------
#0.0            110810832 XMLModuleStore       ∘                    ∘                       64          ∘ ∘          XMLModuleS
#0.1            109985944 list                 ∘                    ∘                      104          3 ∘          list
#0.2            111043728 dict                 ∘                    ∘                     1048         12 ∘          dict
#0.3            110780112 MixedModuleStore     ∘                    ∘                       64          ∘ ∘          MixedModul
#0.4            111033776 dict                 ∘                    ∘                      280          1 ∘          dict
#0.5            110809360 LibraryToolsService  ∘                    ∘                       64          ∘ ∘          LibraryToo
#0.6            111031056 dict                 ∘                    ∘                      280          5 ∘          dict
#0.7            111034352 dict                 ∘                    ∘                     3352         41 ∘          dict
#0.8            110779280 LmsModuleSystem      ∘                    ∘                       64          ∘ ∘          LmsModuleS
#0.9            111050032 dict                 ∘                    ∘                     3352         26 ∘          dict
#0.10           110783296 module               open_ended_grading.u ∘                       56          ∘ ∘          open_ended
#0.11            12611920 dict                 ∘                    ∘                   786712       6069 ∘          dict
#0.12            12619664 dict                 ∘                    ∘                     3352         73 ∘          dict
#0.13     140244885039992 module               sys                  ∘                       56          ∘ ∘          sys
#0.14            13270096 dict                 ∘                    ∘                     3352         54 ∘          dict
#0.15          1294669280 frame                ∘                    wait                   512          ∘ ∘          frame


::> select * from obj where address = 110783296;
#                 address type                 name                           value              size        len mark       repr
-------- ---------------- -------------------- ------------------------------ ------------ ---------- ---------- ---------- ----------
#5.0            110783296 module               open_ended_grading.utils       ∘                    56          ∘ ∘          open_ended


::> path from "address = 109863312" to "address in 0&" reversed
Added 162 paths to newly discovered nodes
Added 224 paths to newly discovered nodes
Added 74284 paths to newly discovered nodes
Added 74098 paths to newly discovered nodes
Added 114983 paths to newly discovered nodes
Added 43125 paths to newly discovered nodes
Added 28538 paths to newly discovered nodes
Added 70566 paths to newly discovered nodes
Added 257640 paths to newly discovered nodes
Added 3690598 paths to newly discovered nodes
#                 address type                 name                 value              size        len mark       repr
-------- ---------------- -------------------- -------------------- ------------ ---------- ---------- ---------- ----------
#1.0            109863312 XMLModuleStore       ∘                    ∘                   64          ∘ ∘          XMLModuleS
#1.1             88367256 list                 ∘                    ∘                  104          3 ∘          list
#1.2             88505440 dict                 ∘                    ∘                 1048         12 ∘          dict
#1.3             88346448 MixedModuleStore     ∘                    ∘                   64          ∘ ∘          MixedModul
#1.4             47993568 dict                 ∘                    ∘                 3352         31 ∘          dict
#1.5             47884224 module               xmodule.modulestore. ∘                   56          ∘ ∘          xmodule.mo
#1.6             12611920 dict                 ∘                    ∘               786712       6069 ∘          dict
#1.7             12619664 dict                 ∘                    ∘                 3352         73 ∘          dict
#1.8      140244885039992 module               sys                  ∘                   56          ∘ ∘          sys
#1.9             13270096 dict                 ∘                    ∘                 3352         54 ∘          dict
#1.10          1294669280 frame                ∘                    wait               512          ∘ ∘          frame


~~~

And there it is, the reason for our memory growth: we're loading XML courses twice into memory, rather than once.
But why is `open_ended_grading.utils` holding on to an `XMLModuleStore`? Tracing through the chain of references,
can see that it is holding an `LmsModuleSystem` as an attribute (the intervening `dict` is the modules `__dict__`
attribute). The `LmsModuleSystem` in turn, has a `LibraryToolsService`, which points to a `MixedModuleStore`, which holds
the `XMLModuleStore`.

To confirm that we understand the source of the leak, we can release what we think is the errant modulestore,
and see how much memory it was using.

~~~ {.bash .pre-scrollable .pre-x-scrollable}
# Release all references to the XMLModuleStore
::> delete from ref where child = 110810832
162 rows deleted

# Garbage collect objects
::> gc
4121623 (4.1M) objects, 14972515 (15.0M) references, 1220387792 (1.2G) total bytes
Marked 2940 top level objects
Marked 13840 objects at depth 1
Marked 6546 objects at depth 2
Marked 2322 objects at depth 3
Marked 9517 objects at depth 4
Marked 13059 objects at depth 5
Marked 7879 objects at depth 6
Marked 50066 objects at depth 7
Marked 78560 objects at depth 8
Marked 83168 objects at depth 9
Marked 72434 objects at depth 10
Marked 25461 objects at depth 11
Marked 19124 objects at depth 12
Marked 13437 objects at depth 13
<snip>
Marked 2 objects at depth 431
Marked 8 objects at depth 432
Marking complete
Deleted 1897434 objects
2224189 (2.2M) objects, 14972515 (15.0M) references, 653444692 (653.4M) total bytes
~~~

Releasing that modulestore freed 650Mb of memory, which accounts for the amount of unexpected memory use we
observed on production.

The next question was why the `open_ended_grading.utils` was creating a separate copy of the ModuleStores, rather than
the global copy. The answer turns out to be that the `LmsModuleSystem` was created during import, which happened
while the global `ModuleStore` was being initialized. As a result, it couldn't retrieve the as-yet incomplete
global copy of the `ModuleStore`, and created a new one instead.

[The solution](https://github.com/edx/edx-platform/pull/6892) to the leak involved removing the static creation of `LmsModuleSystem` from the old OpenEndedResponse
code.

In part 2, I'll detail how I debugged linear memory growth in our offline grading process.

[edx-platform]: https://github.com/edx/edx-platform
[meliae]: https://pypi.python.org/pypi/meliae
[memsee]: https://github.com/nedbat/memsee
[Ned Batchelder]: http://nedbatchelder.com/
[gunicorn]: http://gunicorn.org/
[uWSGI]: https://uwsgi-docs.readthedocs.org/en/latest/