---
title: Using Hypothesis
published: 2015-02-02
edited: 2015-02-11
---

[Hypothesis][] is a library for
property-based testing in Python.

Property-based testing is an alternative approach to unit testing
where rather than describing specific input values and the results
of executing operations on those values, you instead write a test
that should be true of *all* input data of a certain form, and then
let the test framework feed in data until it either finds an example
that fails, or runs out of attempts to do so. The grand-daddy of all
property-testing libraries is [QuickCheck][], but re-implementations
exist in many languages.

<!--more-->

This testing technique is most directly applicable to pure functions
(where the output of the function depends only on the output), but
can be used to generate test data for many other types of tests as well.

For instance, in the edX LMS, we have several functions to encode and
decode strings with `/` in them.

~~~ python
def _quote_slashes(match):
    """
    Helper function for `quote_slashes`
    """
    matched = match.group(0)
    # We have to escape ';', because that is our
    # escape sequence identifier (otherwise, the escaping)
    # couldn't distinguish between us adding ';_' to the string
    # and ';_' appearing naturally in the string
    if matched == ';':
        return ';;'
    elif matched == '/':
        return ';_'
    else:
        return matched


def quote_slashes(text):
    """
    Quote '/' characters so that they aren't visible to
    django's url quoting, unquoting, or url regex matching.

    Escapes '/'' to the sequence ';_', and ';' to the sequence
    ';;'. By making the escape sequence fixed length, and escaping
    identifier character ';', we are able to reverse the escaping.
    """
    return re.sub(ur'[;/]', _quote_slashes, text)


def _unquote_slashes(match):
    """
    Helper function for `unquote_slashes`
    """
    matched = match.group(0)
    if matched == ';;':
        return ';'
    elif matched == ';_':
        return '/'
    else:
        return matched


def unquote_slashes(text):
    """
    Unquote slashes quoted by `quote_slashes`
    """
    return re.sub(r'(;;|;_)', _unquote_slashes, text)
~~~

The tests for these functions list out specific input strings
to test on

~~~ python
TEST_STRINGS = [
    '',
    'foobar',
    'foo/bar',
    'foo/bar;',
    'foo;;bar',
    'foo;_bar',
    'foo/',
    '/bar',
    'foo//bar',
    'foo;;;bar',
]


@ddt
class TestQuoteSlashes(TestCase):
    """Test the quote_slashes and unquote_slashes functions"""

    @data(*TEST_STRINGS)
    def test_inverse(self, test_string):
        self.assertEquals(test_string, unquote_slashes(quote_slashes(test_string)))

    @data(*TEST_STRINGS)
    def test_escaped(self, test_string):
        self.assertNotIn('/', quote_slashes(test_string))
~~~

(In this snippet, `ddt` refers to [another useful testing library][ddt] which I might
cover in a later post.)

These tests seem fairly complete, but we might worry about whether we got all
of the combinations of `;`, `_`, and `/`, and all of their edge cases.

So, let's try changing the test to a property based test instead, and see
if we can let the computer find better test cases for us.

~~~ python
from hypothesis.testdecorators import given

class TestQuoteSlashes(TestCase):
    """Test the quote_slashes and unquote_slashes functions"""

    @given(str)
    def test_inverse(self, test_string):
        self.assertEquals(test_string, unquote_slashes(quote_slashes(test_string)))

    @given(str)
    def test_escaped(self, test_string):
        self.assertNotIn('/', quote_slashes(test_string))
~~~

Those tests run, and.... all pass.

~~~ bash
> python -m coverage run --rcfile=lms/.coveragerc ./manage.py lms test --verbosity=1 lms_xblock.test.test_runtime   --traceback --settings=test
========================================
 Running tests for lms
========================================
nosetests lms_xblock.test.test_runtime --id-file /home/cpennington/work/edx-platform/.testids/lms/noseids --xunit-file /home/cpennington/work/edx-platform/reports/lms/nosetests.xml --verbosity=1
Creating test database for alias 'default'...
.........
-----------------------------------------------------------------------------
9 tests run in 2.6 seconds (9 tests passed)
~~~

How uninteresting. Let's introduce a bug in the code, to see how the test failure manifests.

~~~ diff
     if matched == ';':
         return ';;'
     elif matched == '/':
-        return ';_'
+        return ';;'
~~~

Now, we get an interesting failure.

~~~ bash
1) FAIL: test_inverse (lms_xblock.test.test_runtime.TestQuoteSlashes)

   Traceback (most recent call last):
    /home/cpennington/.virtualenvs/edx-platform/local/lib/python2.7/site-packages/hypothesis/testdecorators.py line 41 in wrapped_test
      test(*(arguments + falsifying_example[0]), **falsifying_example[1])
    lms/djangoapps/lms_xblock/test/test_runtime.py line 22 in test_inverse
      self.assertEquals(test_string, unquote_slashes(quote_slashes(test_string)))
   AssertionError: '/' != ';'
~~~

But run the same test again, and it passes! That highlights one of the biggest issues with
property-testing, which is that it relies on generating enough input data to catch the bug.
As the space of input grows, so does the time it takes to explore it.

With Hypothesis, we can combat this by increasing the number of examples.

~~~ python
from hypothesis.testdecorators import given
from hypothesis.settings import Settings

class TestQuoteSlashes(TestCase):
    """Test the quote_slashes and unquote_slashes functions"""

    @given(str, verifier_settings=Settings(max_examples=1000))
    def test_inverse(self, test_string):
        self.assertEquals(test_string, unquote_slashes(quote_slashes(test_string)))

    @given(str, verifier_settings=Settings(max_examples=1000))
    def test_escaped(self, test_string):
        self.assertNotIn('/', quote_slashes(test_string))
~~~

Now the test consistently detects our bug (the number of examples and test timeout can both
be tuned to limit the amount of time spent in the test suite).

An advantage to this randomized testing over our fixed list of strings is that it will test
characters that we might not have thought to in our attempt at an exhaustive list. If we change
the set of characters used to encode the `/`, our tests won't need to change. However, the test
is still limited by what characters might be generated by the random data generator. If we
switch `@given(str)` to `@given(unicode)`, the test no longer identifies the bug, because
Hypothesis uses a data generator for `unicode` that includes only numbers and ascii characters (and
no symbols such as `/`). This seems like a questionable choice to me, but was perhaps made to limit
the search space to "text-like" strings. There is always a tradeoff between on breadth and depth
of the search that property-based testing makes, because there is a finite time to generate new
test data. By limiting the number of characters used to generate strings, we can expect to more
completely explore the space of a given string length.

One might also consider injecting generated strings into a list of `ddt` items.

~~~ python
    @data(*(TEST_STRINGS + generate_strings()))
    def test_inverse(self, test_string):
        self.assertEquals(test_string, unquote_slashes(quote_slashes(test_string)))

    @data(*(TEST_STRINGS + generate_strings()))
    def test_escaped(self, test_string):
        self.assertNotIn('/', quote_slashes(test_string))
~~~

This would give some of the advantages of using property based testing. However, one facility that this wouldn't
provide is test-case shrinking. Hypothesis, like QuickCheck before it, will attempt to reduce your
test cases for you, when it finds a failure, to find the smallest possible counterexample for the
property. This is important, especially when your test generation code can potentially produce very, very
large input data initially.

I think that Hypothesis may have a place in the edx testing ecosystem. The methods covered in this
post would benefit, and there are likely other properties that we could test as well, especially with a
little investment in data generation. For instance, we could generate random courses with the installed XBlocks,
and validate that import/export are inverses. We might also be able to test stateful code using Hypothesis'
stateful testing mechanism (which I hope to explore in a future post).

[Hypothesis]: https://github.com/DRMacIver/hypothesis
[QuickCheck]: https://hackage.haskell.org/package/QuickCheck
[ddt]: http://ddt.readthedocs.org/en/latest/