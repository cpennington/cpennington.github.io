---
title: A Tale of Two Leaks: An Incremental Leak
published: 2015-02-17
edited: 2015-02-19
---

In [part 1][], we saw a memory leak that was in
some sense static: The memory use was unintentional/undesired, but it
was only allocated once, and didn't get worse as the process continued.
On the other hand, the second leak was a true leak in the classic sense:
the memory use appeared to be unbounded.

<!--more-->

Adam, the lead sustaining engineer at [edX][], and Ed, our Devops lead,
were able to identify that the leak happened during our bulk-grading
operations. During grading, we loop through every student in a single
and then loop through every gradable [XBlock][] to identify whether
we've already scored that XBlock, and if not, we score the student
on that block. Then we aggregate all of those grades based on the
course's grading policy.

Adam was able to narrow down the problem by creating a test case that
graded a single student repeatedly. That test showed the same unbounded
memory growth we observed in the overall process. Using [objgraph][],
he was able to identify that each time the student was graded,
a constant number of `CombinedSystems` were created and not released.
This was suspicious, as those objects were intended to be ephemeral
objects intended only to combine the attributes of `DescriptorSystem`
and `ModuleSystem` into a single object.

Adam was also to dump the processes memory with [meliae][], after it had
leaked memory, so we were able to dig more into the particulars of
the `CombinedSystems` that were still held in memory.

# Investigating the Memory Dump

Once we had a dump, I was able to begin investigating with [memsee][].
My first attempt was to use the `path` command that pointed to the errant
pointer in [part 1][]. However, all of those attempts timed out before
they found any path from the root to an `CombinedSystem`.

~~~ sql
::> select * from obj where type = 'CombinedSystem';
#                 address type                 name                 value                                                              size        len mark       repr
-------- ---------------- -------------------- -------------------- ------------------------------------------------------------ ---------- ---------- ---------- ----------
#1.0            270528592 CombinedSystem       ∘                    ∘                                                                    64          ∘ ∘          CombinedSy
#1.1            256545552 CombinedSystem       ∘                    ∘                                                                    64          ∘ ∘          CombinedSy
#1.2            239994960 CombinedSystem       ∘                    ∘                                                                    64          ∘ ∘          CombinedSy
<snip>
#1.1021         210278800 CombinedSystem       ∘                    ∘                                                                    64          ∘ ∘          CombinedSy
#1.1022         196178640 CombinedSystem       ∘                    ∘                                                                    64          ∘ ∘          CombinedSy
#1.1023         179498384 CombinedSystem       ∘                    ∘                                                                    64          ∘ ∘          CombinedSy
#1.1024         166197904 CombinedSystem       ∘                    ∘                                                                    64          ∘ ∘          CombinedSy
#1.1025         149917968 CombinedSystem       ∘                    ∘                                                                    64          ∘ ∘          CombinedSy
~~~

Picking an arbitrary `CombinedSystem`, we can find out what's pointing
to it.

~~~ sql
::> parents 149917968
(151461744, u'dict', None, None, 3352, 45)
::> parents 151461744
(149920464, u'LmsModuleSystem', None, None, 64, None)
~~~

The first `dict` is just `__dict__` from the the `LmsModuleSystem`.
Given that the `CombinedSystems` are just supposed to be pointing to
a `DescriptorSystem` and `ModuleSystem`, it's suspicious that a
`CombinedSystem` is being held in memory in turn by an `LmsModuleSystem`.

We can keep tracking the parents upwards:

~~~ sql
::> parents 149920464
(166197904, u'CombinedSystem', None, None, 64, None)
(151176912, u'dict', None, None, 280, 1)
(150230496, u'dict', None, None, 1048, 9)
::> parents 166197904
(166753712, u'dict', None, None, 3352, 45)
::> parents 166753712
(166196048, u'LmsModuleSystem', None, None, 64, None)
~~~

It looks like we have a chain of `LmsModuleSystem -> CombinedSystem
-> LmsModuleSystem`. (A side note about python memory management:
`CombinedSystem` appears as a direct parent of `LmsModuleSystem`, with
no intervening `dict` because `CombinedSystem` defines its attributes
using `__slots__`. This is a good strategy for ephemeral objects, as
it saves you from allocating additional dictionaries.)

# Designing a Fix

Looking at the relevant `edx-platform` code, there's only one place where
`CombinedSystems` are constructed:

~~~ python
# edx-platform/common/lib/xmodule/xmodule/x_module.py

class XModuleMixin(XBlockMixin):
    """
    Fields and methods used by XModules internally.

    Adding this Mixin to an :class:`XBlock` allows it to cooperate with old-style :class:`XModules`
    """

    ...

    @property
    def runtime(self):
        return CombinedSystem(self.xmodule_runtime, self._runtime)
~~~

`XModuleMixin` is used added as a base class for all XBlocks used in
`edx-platform`. So, whatever is capturing `CombinedSystem` in
`LmsModuleSystem`, it's coming from a call to `.runtime`.

The constructor for `ModuleSystem` (the base-class for `LmsModuleSystem`)
takes a `descriptor_runtime` argument:

~~~ python
# edx-platform/common/lib/xmodule/xmodule/x_module.py

class ModuleSystem(MetricsMixin, ConfigurableFragmentWrapper, Runtime):
    def __init__(self, ..., descriptor_runtime, ...):
        ...
        self.descriptor_runtime = descriptor_runtime
~~~

This seems like a good candidate for something that would be storing
a `CombinedSystem`. In fact, looking at `module_render.py`, which is
the primary entry point in the LMS for working with XBlocks, we see
that we're passing `descriptor.runtime` in to that argument:

~~~ python
# edx-platform/lms/djangoapps/courseware/module_render.py

def get_module_system_for_user(...):
    system = LmsModuleSystem(
        ...
        descriptor_runtime=descriptor.runtime,
        ...
    )
~~~

We find the last piece of the puzzle by looking at what happens to the
`LmsModuleSystem` once it's constructed.

~~~ python
# edx-platform/lms/djangoapps/courseware/module_render.py

def get_module_for_descriptor_internal(user, descriptor, ...):
    (system, field_data) = get_module_system_for_user(
        user=user,
        descriptor=descriptor,
        ...
    )

    descriptor.bind_for_student(system, field_data)  # pylint: disable=protected-access

# edx-platform/common/lib/xmodule/xmodule/x_module.py

    def bind_for_student(self, xmodule_runtime, field_data):
        self.xmodule_runtime = xmodule_runtime

~~~

So, now consider what happens during grading, when we have the same
XBlock being bound to different users (or re-bound to the same user)
over the course of the grading session:

~~~ python
# Before grading step

        +----------+
        |descriptor|
        +----------+

# LmsModuleSystem(descriptor_runtime=descriptor.runtime)  ## get_module_system_for_user

        +----------+   +---------------+
        |descriptor|   |CombinedSystem |
        +----------+   +-^-------------+
                         |
                         |descriptor_runtime
                         |
                       +-+-------------+
                       |LmsModuleSystem|
                       +---------------+

# self.xmodule_runtime = xmodule_runtime   ## bind_for_student

        +----------+   +---------------+
        |descriptor|   |CombinedSystem |
        +------+---+   +-^-------------+
               |         |
xmodule_runtime|         |descriptor_runtime
               |         |
               |       +-+-------------+
               +------->LmsModuleSystem|
                       +---------------+

# LmsModuleSystem(descriptor_runtime=descriptor.runtime)  ## get_module_system_for_user                                                              

        +----------+   +---------------+
        |descriptor|   |CombinedSystem |
        +------+---+   +-^-------------+
               |         |
xmodule_runtime|         |descriptor_runtime
               |         |
               |       +-+-------------+
               +------->LmsModuleSystem|
                       +-^-------------+
                         |
                         |_runtime
                         |
                       +-+-------------+
                       |CombinedSystem |
                       +-^-------------+
                         |
                         |descriptor_runtime
                         |
                       +-+-------------+
                       |LmsModuleSystem|
                       +---------------+

# self.xmodule_runtime = xmodule_runtime   ## bind_for_student

        +----------+   +---------------+
        |descriptor|   |CombinedSystem |
        +------+---+   +-^-------------+
               |         |
xmodule_runtime|         |descriptor_runtime
               |         |
               |       +-+-------------+
               |       |LmsModuleSystem|
               |       +-^-------------+
               |         |
               |         |_runtime
               |         |
               |       +-+-------------+
               |       |CombinedSystem |
               |       +-^-------------+
               |         |
               |         |descriptor_runtime
               |         |
               |       +-+-------------+
               +------->LmsModuleSystem|
                       +---------------+

~~~

In the end, we're building up a chain of `LmsModuleSystems` and
`CombinedSystems`, and never releasing them.

The initial fix to this was to extract actual `DescriptorSystem` from
the `CombinedSystem`, and passing that to the `LmsModuleSystem`. That
ensures that the references to the previous `LmsModuleSystem` is released
when `xmodule_runtime` is re-bound. The code to make that change is
in [this pr](https://github.com/edx/edx-platform/pull/6930/files).

A more robust fix would be for `ModuleSystem` to expect a pointer to a
descriptor, rather than the descriptor runtime, so that it can extract
the `DescriptorSystem` from the `CombinedSystem` itself.

[part 1]: /posts/memsee-pt1.html
[XBlock]: https://github.com/edx/xblock
[edX]: https://www.edx.org/
[objgraph]: http://mg.pov.lt/objgraph/
[memsee]: https://github.com/nedbat/memsee
[meliae]: https://pypi.python.org/pypi/meliae
