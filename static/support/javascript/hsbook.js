// API
// ==================================================
//
// Comment IDs need to be globally unique.
//
// jQuery and jQuery form need to be loaded before this script
//
// HTTP
// --------------------------------------------------
//
// Files
// ..................................................
//
// /support/icons/throbber.gif
//
// Load comment
// ..................................................
//
// GET /comments/single/<id>/
//
// HTTP/1.1 200 OK
// Content-type: text/html
//
// <HTML fragment to display for a single comment>
//
// There should be a span.comment_error present in order to do
// validation. The new comment form has to have controls with
// name="name" and name="comment" fields in order for the validation
// to pass. The submission button should have name="submit".
//
// Load comment counts
// ..................................................
//
// GET /comments/chapter/<chapter id>/count/
//
// HTTP/1.1 200 OK
// Content-type: text/json
//
// <{<comment id>:<count>} :: {String:Integer}>
//
// POST /comments/submit/<id>/
// Content-type: x-www-urlencoded
//
// <Required fields: id, comment, name>
//
// HTTP/1.1 200 OK
// Content-type: text/html
// <the new comment HTML. Should be the same as
// /comments/single/<id>>
//
// HTML
// --------------------------------------------------
//
// Adds a "Load all comments" link following div.toc>p
//
// Adds the chapter id to a#chapterfeed[@href]
//
// Looks for the first existing one of the following elements to find
// the chapter id:
//
// * div.preface
// * div.chapter
// * div.appendix
// * div.bibliography
//
// Adds comment nodes for each of:
//
// * .chapter p[@id]
// * .chapter table[@id].equation
// * .chapter pre[@id]
//
// A comment node is identified by #comments_<comment id>
//
// Loads the comment counts for each comment and replaces the loading
// message with the comment count. Any "span.commenttoggle"s that do
// not have a count returned get the text "No comments". ids that are
// not found in the page are ignored.

function qid(id) {
  return id.replace(/([.:])/g, "\\$1");
}

function beforeComment(formData, jqForm, options) {
  var form = jqForm[0];
  if (!form.comment.value) {
    $(options.target + " span.comment_error").empty().append(
      "<span class=\"comment_error\">Your comment is empty</span>");
    return false;
  }
  if (!form.name.value) {
    $(options.target + " span.comment_error").empty().append(
      "<span class=\"comment_error\">Please provide a name</span>");
    return false;
  }
  $(options.target + " span.comment_error").empty().after(
    "<img src=\"/support/icons/throbber.gif\" style=\"vertical-align: middle\"/>");
  $(options.target + " input[@name=submit]").attr("disabled", true);
}

function ajaxifyForm(id) {
  var q = qid(id);
  
  $("#form_" + q).ajaxForm({ beforeSubmit: beforeComment,
			     success: function() { ajaxifyForm(id); },
			     target: "#comments_" + q });
}

function toggleComment(id) {
  $("#toggle_" + qid(id)).nextAll().toggle();
  return false;
}

function loadComments(id) {
  $("#comments_" + qid(id)).load(location.protocol + "//" + location.host +
				 "/comments/single/" + id + "/", function() {
    ajaxifyForm(id);
  });
  return false;
}

function loadAllComments() {
  $("a.commenttoggle").each(function() {
    var id = $(this).attr("pid");
    if (id) {
      loadComments(id);
    }
  });
}

function getChapterId() {
    return $("div.preface, div.chapter, div.appendix, div.bibliography").attr("id");
}

$(document).ready(function() {
  function loading(id) {
    return " <span id=\"comments_" + id + "\" class=\"comment\">" +
      "<span pid=\"" + id + "\" class=\"commenttoggle\">Loading..." +
      "</span></span>";
  }
  $("div.toc>p")
    .after("<p style='display: none;'><a onclick='return loadAllComments()'>" +
	   "Load all comments (<b>slow</b>)</a></p>")
    .toggle(function() { $(this).nextAll().show("normal"); },
	    function() { $(this).nextAll().hide("normal"); })
    .hover(function() { $(this).fadeTo("normal", 0.8); },
	   function() { $(this).fadeTo("normal", 0.35); });
  $(".chapter p[@id]").each(function() {
    $(this).append(loading($(this).attr("id")));
  });
  $(".chapter table[@id].equation").each(function() {
    id = $(this).attr("id");
    $("#" + id + " tr").after('<tr><td colspan="4">' + loading($(this).attr("id")) + '</td></tr>');
  });
  $(".chapter pre[@id]").each(function() {
    $(this).after(loading($(this).attr("id")));
  });
  var chapid = getChapterId();
  $("#chapterfeed").attr("href",
			 $("#chapterfeed").attr("href") + chapid + "/");
  $.getJSON(location.protocol + "//" + location.host + "/comments/chapter/" +
	    chapid + "/count/", function(data) {
    $.each(data, function(id, item) {
      var s = item == 1 ? "" : "s";
      $("#comments_" + qid(id) + " span.commenttoggle").replaceWith(
        "<a class='commenttoggle' id='toggle_" + id + "' " +
	"pid='" + id + "' " +
	"onclick='return loadComments(\"" + id + "\")' " +
	"href='comments: show / hide'>" + item + " comment" + s + "</a>");
    });
    $("span.commenttoggle").each(function() {
      var id = $(this).attr("pid");
      $(this).replaceWith("<a class='commenttoggle' id='toggle_" + id + "' " +
			  "onclick='return loadComments(\"" + id + "\")' " +
			  "href='comment: add'>No comments</a>");
    });
  });
});
