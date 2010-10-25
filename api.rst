API
==================================================

If there is more than one page that is being commented on per server,
the id namespace is the same between documents.

jQuery and jQuery form need to be loaded before this script

HTTP
--------------------------------------------------

Files
..................................................

``/support/icons/throbber.gif``

Load comment
..................................................

Request::
  GET /comments/single/<id>/ HTTP/1.1

Response::
  HTTP/1.1 200 OK
  Content-type: text/html

  <HTML fragment to display for a single comment>

There should be a span.comment_error present in order to do
validation. The new comment form has to have controls with
``name="name"`` and ``name="comment"`` fields in order for the
validation to pass. The submission button should have
``name="submit"``.

Load comment counts
..................................................

Request::
  GET /comments/chapter/<chapter id>/count/

Response::
  HTTP/1.1 200 OK
  Content-type: text/json

  <{<comment id>:<count>} :: {String:Integer}>

Add new comment
..................................................

Request::
  POST /comments/submit/<id>/
  Content-type: x-www-urlencoded

  <Required fields: id, comment, name>

Response::
  HTTP/1.1 200 OK
  Content-type: text/html

  <the new comment HTML. Should be the same as
  /comments/single/<id>>

HTML
--------------------------------------------------

Adds a "Load all comments" link following ``div.toc>p``

Adds the chapter id to ``a#chapterfeed[@href]``

Looks for the first existing one of the following elements to find
the chapter id:

* ``div.preface``
* ``div.chapter``
* ``div.appendix``
* ``div.bibliography``

Adds comment nodes for each of:

* ``.chapter p[@id]``
* ``.chapter table[@id].equation``
* ``.chapter pre[@id]``

A comment node is identified by ``#comments_<comment id>``

Loads the comment counts for each comment and replaces the loading
message with the comment count. Any ``span.commenttoggle`` elements
that do not have a count returned get the text "No comments". ids that
are not found in the page are ignored.
