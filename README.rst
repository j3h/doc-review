Collaborative Document Review Web Application README
####################################################

This package provides a Web application for soliciting
paragraph-by-paragraph comments on a document. The application was
inspired by and re-uses the user interface and much of the JavaScript
code from the excellent <http://book.realworldhaskell.org/>.

The original Web application was part of a Docbook-based toolchain
that was used to produce Real World Haskell, and was written in Python
using Django. This implementation is intended to be as independent as
possible from the authoring system that is used to produce a document,
and to run in a wide variety of environments.

Quick Start
--------------------------------------------------

For most users, you can be up and running by following these steps:

1. Prepare your documents:

   * Add a ``<div>`` tag with ``class="chapter"`` around the content that you
     will want comments on.
   * Add an ``id`` attribute to all of the paragraphs that you wish to
     enable comments on.
2. Build and install using ``cabal install`` to obtain a doc-review
   executable::

    $ cabal install doc-review
3. Test your documents::

    $ doc-review --content-dir=$PATH_TO_YOUR_DOCUMENTS

   Comments you leave when testing will not be saved when the server
   is restarted.
4. Select your backend. Right now, that probably means the SQLite
   backend::

    $ doc-review --content-dir=$PATH_TO_YOUR_DOCUMENTS
       --store=sqlite:comments.db

   This command will create ``comments.db`` if necessary, and store all
   comments in that SQLite database.

5. Decide how you are going to run the server. Running this program as
   a daemon and configuring your Web server to use reverse proxying is
   the most straightforward solution.

Marking up documents
--------------------------------------------------

``doc-review`` will recursively traverse the directory specified by the
``--content-dir`` parameter looking for files with the extension
``.html``, ``.htm`` or ``.xhtml``. It will parse those files as HTML,
looking for paragraphs marked as commentable, and will store those
chapter definitions in the data store.

In order for a document to be commentable, you must load the comment
JavaScript into the document, by adding the following lines to the
``<head>`` of the document::

 <script type="text/javascript"
         src="/support/javascript/jquery.js"></script>
 <script type="text/javascript"
         src="/support/javascript/form.js"></script>
 <script type="text/javascript"
         src="/support/javascript/hsbook.js"></script>

You will likely want to (but by no means have to) reuse the CSS for
the comments::

 <link rel="stylesheet" type="text/css" href="/comments.css"/>

To mark a paragraph as commentable, it must be inside of a ``<div>``
with ``class="chapter"``. The choice of ``class="chapter"`` is for
compatibility with the Real World Haskell implementation.

The current implementation (like the Real World Haskell
implementation) depends on the ``id`` being unique across the full set
of documents that you want comments on. That means that if you have
two documents with paragraphs with ``id="sauteed-spinach"``, then
comments left on either of those paragraphs will be visible in both
documents. This can be useful if you have duplicated content, but if
the content is different, user (or author!) confusion can result.

Running the server
--------------------------------------------------

The server is a plain HTTP server that serves three kinds of files:

1. URLs under ``/comments/``, which serve the comments API. This is
what is accessed by the JavaScript in order to insert the comments to
the document and to save user comments. These are the only dynamic
URLs that will be served.

2. The URL will be checked against the files in the directory
specified by ``--content-dir=``. That static file will be served if
there is a match.

3. If no matching file was found, it will look for a matching file
in the directory specified by ``--static-dir=`` and serve that.

It is likely that you will be integrating this server into a larger
Web site. In that case, you will likely want to reverse proxy requests
from your main Web server to this server (using
e.g. <http://httpd.apache.org/docs/2.2/mod/mod_proxy.html>). If you
are running this server proxied by another Web server, you can serve
the content and other static files from that server to no ill effect
(those files are served by default for convenience).

Storage options
--------------------------------------------------

There are three kinds of storage backend implemented at this time:

In-memory
 Store the comments in the server process' memory (no persistent
 storage). This means that all comments will be lost when the server
 is restarted. This is useful for testing and as a reference
 implementation of the storage API.

Flat file
 Store the comments in flat files in the filesystem. The comments and
 other data are stored in a custom binary format. This backend is
 known to have race conditions and other non-ACID properties. It is
 not recommended that you use this store.

SQLite
 Store the comments in a SQLite database. This is the best option for
 production use at this time. That being said, this is not really a
 production-ready solution, because database errors (e.g. a
 SQLITE_BUSY timeout) will result in a 500 error being returned to the
 client.

The output to ``doc-review --help`` will indicate how to specify each
store type.

The store API is well defined and easy to implement and test. Patches
for data storage improvements are welcome!

Binary logging
..................................................

In addition to the storage options specified above, there is an
experimental binary logging option that will append a binary log
record of each store operation to a file in addition to applying it to
the store. This was implemented as a backup mechanism should the
primary store be corrupted, as replaying the operations from the log
should restore the contents of the data store.

Note that this option is not well tested, and may disappear in future
releases.

Implementation details
--------------------------------------------------

This section discusses some implementation details that may be useful
for examining the data in the database or implementing your own
storage backend. As always, the code is the best reference, but this
discussion should help you get started and serve as a rough
specification for what the code ought to do when it's not inherently
clear.

User sessions
..................................................

This server stores a session cookie for each browsing session that is
renewed on each request. The session cookie is used to look up the
user information to prefill when showing the add comment form. It is
also stored in the database so that the author/administrator can see
which comments came from which browser. It is a rather imprecise
mechanism, and easy to spoof (just send whatever session cookie you
want), but it is helpful for the user not to have to re-fill the form
fields. The session cookie expires after 11 days without visiting any
page on the site.

Test suite
..................................................

There is a test suite, which will be build when the parameter
``--flags=test`` is supplied to cabal-install. *The test suite only
tests the storage backends. The remainder of the code currently has no
automated tests.* The backends are tested using randomized testing for
consistency with each other as well as some relatively trivial, but
critical behavior.

The tests do not test concurrent access to the stores. There is no
specification of the behavior of the stores under concurrent
access. The SQLite and in-memory stores serialize access to the
backend between threads, so concurrency should not be an issue, but
the file-based backend may cause data loss under concurrent use. Tests
welcome.

To test the stores for consistency, the test suite creates two empty
stores of different types and then randomly generates store
operations. The store operations are performed to each store in turn,
checking that the operation returns the same result for both
stores. This does not show that the stores behave correctly, but it
does provide evidence that the implementations are consistent with
each other.

There are not many tests for correctness, but there are a few tests
that perform an operation with a specified effect on the backend and
then make observations that the desired effect has occurred. These
tests are run with each store in an empty state, and then a sequence
of randomized operations that perturb the store's state are
performed. The properties are once again checked. This process is
repeated. This should provide evidence that the specified properties
hold for the store without depending on it being in a particular state.

Future plans
--------------------------------------------------

As usual, there are a whole list of features and changes that I'd like
to make to this program. If a feature is important to you, or if you
have an idea for a new feature, please let me know. The best way is to
submit a patch!

Future plans include:

* Packaging the API as a library so that it can be integrated with
  other Haskell Web applications
* Features:

  * Add a link field to the comment form and backend storage so that
    commenters can provide a link to their blog or home page.
  * Better comment viewing tools:

    * View one user's annotations at a time
    * Add a widget that shows an overview of the comments on a
      document
    * Show where in the document the comments are

  * Have the server add the ``<script>`` and ``<link>`` tags to
    documents in the content directory automatically
  * Add a PostgreSQL storage backend
  * Add a MySQL storage backend
  * Make building SQLite support optional (so you do not need to build
    it unless you are using that backend)
  * Add support for handing proxy host headers so that URLs get
    generated properly (e.g. X-Forwarded-Host)
  * Remove the code for dropping OS privilege (it's not really
    trustworthy, so it's a little bit of a misfeature)
  * Add markup processing for the comments (e.g. Markdown in the user comments)

If you have feature requests, bug reports or other feedback, please
let me know!
