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
