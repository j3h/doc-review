function qid(id) {
  return id.replace(/([.:])/g, "\\$1");
}

function beforeComment(formData, jqForm, options) {
    var form = jqForm[0];
    clearStatus(options.target);
    if (!form.comment.value) {
        setError(options.target, "Your comment is empty");
        return false;
    }
    if (!form.name.value) {
        setError(options.target, "Please provide a name");
        return false;
    }
    $(options.target + " span.comment_error").after(
        "<img src=\"/support/icons/throbber.gif\" style=\"vertical-align: middle\"/>");
    $(options.target + " input[@name=submit]").attr("disabled", true);
    return true;
}

function clearStatus(target) {
    $(target + " input[@name=submit]").attr("disabled", false);
    $(target + " span.comment_error").empty().nextAll().remove();
}

function setError(target, errStr) {
    $(target + " span.comment_error").append(errStr);
}

function ajaxifyForm(id) {
  var q = qid(id);
  var target = "#comments_" + q;
  $("#form_" + q).ajaxForm({
      beforeSubmit: beforeComment,

      success: function() { ajaxifyForm(id); },

      error: function (_req, typ, exc) {
          clearStatus(target);
          switch (typ) {
          case 'error':
              setError(target, "Server error. Try again soon.");
              break;
          case 'timeout':
              setError(target, 'Request timed out. Try again in a few seconds.');
              break;
          default:
              setError(target, 'An unknown error occurred.');
              break;
          }
          return false;
      },

      target: target
  });
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
