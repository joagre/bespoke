import bespoke2 from "/js/bespoke2.js";

// uhtml.min.js must be imported in the HTML file before this script
const { html, render } = uhtml;

const message2 = {
  _messageIdToDelete: null,

  openDeleteMessageModal(event) {
    event.stopPropagation();
    // Extract message to delete
    message2._messageIdToDelete =
      event.currentTarget.getAttribute("data-message-id");
    // Update the message modal body
    let replyMessage = message2._replyMessages.find(
      replyMessage => replyMessage["id"] ===
        message2._messageIdToDelete);
    if (replyMessage == null) {
      document.getElementById("delete-message-body").innerHTML =
        "Do you really want delete this post?";
    } else {
      document.getElementById("delete-message-body").innerHTML =
        `Do you really want delete this message written by ${replyMessage["author"]}?`;
    }
    UIkit.modal("#delete-message-modal").show();
  },

  deleteMessage(event) {
    let messageId = null;
    if (message2._messageIdToDelete === bespoke2.peekMessageStack()) {
      messageId = -1;
    }

    async function updateServer() {
      try {
        // REST API: /delete_message
        const response = await fetch("/delete_message", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(message2._messageIdToDelete)
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        bespoke2.gotoPage(event, "message2.html", messageId);
        message2._messageIdToDelete = null;
      } catch (error) {
        console.error("Fetching failed:", error);
      }
    };

    updateServer();
  },

  toggleQuote(event) {
    event.preventDefault();
    let messageId = event.currentTarget.getAttribute("data-message-id");
    let parentMessageId = event.currentTarget.getAttribute("data-parent-message-id");
    let replyQuote = document.getElementById(`reply-quote-${messageId}`);
    let isHidden = replyQuote.hidden;
    let replyQuoteButton = document.getElementById(`reply-quote-button-${messageId}`);

    if (isHidden) {
      let replyQuoteBody = document.getElementById(`reply-quote-body-${messageId}`);
      let replyBody = document.getElementById(`reply-body-${parentMessageId}`);
      replyQuoteBody.innerHTML = replyBody.innerHTML;
      replyQuote.hidden = false;
      replyQuoteButton.setAttribute("uk-icon", "chevron-up");
    } else {
      replyQuote.hidden = true;
      replyQuoteButton.setAttribute("uk-icon", "chevron-down");
    }
  }
};

export default message2;

document.addEventListener("DOMContentLoaded", function() {
  bespoke2.init();

  function populatePage(parentMessage, rootMessageTitle, replyMessages) {
    let messageStackSize = bespoke2.messageStackSize();

    // Populate head title
    let pageTitle = "Post";
    if (messageStackSize > 1) {
      document.getElementById("head-title").textContent = "Post";
    } else {
      document.getElementById("head-title").textContent =
        `Reply [level: ${messageStackSize - 1}]`;
    }
    document.getElementById("head-title").textContent = "Post";

    // Populate header
    if (messageStackSize == 1) {
      document.getElementById("header-title").textContent = "Post";
      document.getElementById("header-reply-level").style.display = "none";
    } else {
      document.getElementById("header-title").textContent = "Reply";
      document.getElementById("header-reply-level").textContent =
        `[level: ${messageStackSize - 1}]`;
    }

    // Populate parent message
    document.getElementById("parent-title").innerHTML =
      rootMessageTitle;
    document.getElementById("parent-body").innerHTML =
      bespoke2.formatMarkdown(parentMessage["body"]);
    document.getElementById("parent-author").textContent =
      parentMessage["author"];
    document.getElementById("parent-age").textContent =
      bespoke2.formatSecondsSinceEpoch(parentMessage["created"]);
    document.getElementById("parent-replies").textContent =
      parentMessage["reply-count"];
    document.getElementById("parent-delete")
      .setAttribute("data-message-id", parentMessage["id"]);

    // Populate replies
    const repliesContainer = document.getElementById("replies");
    const replyTemplates =
          replyMessages.map(
            replyMessage => createReplyTemplate(
              parentMessage, replyMessage, replyMessages));
    render(repliesContainer, html`${replyTemplates}`);
  };

  function createReplyTemplate(parentMessage, message, replyMessages) {
    /*
      Reply message example:

      <!-- Reply 1 -->
      <article class="uk-article uk-margin-remove-top">
        <!-- Reply quote -->
        <div class="uk-text-meta quote" onclick="message2.toggleQuote(event)" data-message-id="1" data-parent-message-id="0">
          <span id="reply-quote-button-1" class="uk-icon-link" uk-icon="chevron-down"></span>
          In reply to author-0
          <div id="reply-quote-1" class="uk-card uk-card-body uk-card-default uk-card-small uk-text-meta uk-margin-small-bottom uk-margin-small-top custom-quote-padding" hidden>
            <div id="reply-quote-body-1" class="uk-margin-remove-first-child uk-margin-remove-last-child">reply-quote-body-1</div>
          </div>
        </div>
        <!-- Reply body -->
        <div id="reply-body-1" class="uk-margin-remove-first-child uk-margin-remove-last-child">reply-body</div>
        <!-- Reply Meta-data -->
        <div class="uk-article-meta uk-margin-top-remove">
          <div class="uk-flex uk-flex-between uk-flex-middle">
            <div>
              <span id="reply-author-1">reply-author</span> •
              <span id="reply-age-1">reply-age</span> •
              <button onclick="bespoke2.gotoPage(event)" data-page="message2.html" data-message-id="0" class="uk-icon-button" uk-icon="comments"></button>
              0
            </div>
            <div>
              <button onclick="message2.openDeleteMessageModal(event)" data-message-id="1" class="uk-icon-button" uk-icon="trash"></button>
              <button onclick="bespoke2.gotoPage(event)" data-page="add_reply2.html" data-message-id="0" class="uk-icon-button" uk-icon="reply"></button>
            </div>
          </div>
        </div>
      </article>
      <hr id="reply-divider-0" class="uk-margin-small uk-divider-icon">
    */

    // A reply quote is only added if someone replies on a reply
    let replyQuote = "";
    if (message["parent-message-id"] != null &&
        message["parent-message-id"] != parentMessage["id"]) {
      const replyQuoteButtonAttr = `reply-quote-button-${message["id"]}`;
      const replyQuoteAttr = `reply-quote-${message["id"]}`;
      const replyQuoteBodyAttr = `reply-quote-body-${message["id"]}`;
      const replyQuoteAuthor =
            replyMessages.find(
              replyMessage => replyMessage["id"] ==
                message["parent-message-id"])["author"];
      replyQuote = html`
        <!-- Reply quote -->
        <div class="uk-text-meta quote" onclick=${() =>
          message2.toggleQuote(event)
        }} data-message-id="${message["id"]}" data-parent-message-id="${message["parent-message-id"]}">
          <span id="${replyQuoteButtonAttr}" class="uk-icon-link" uk-icon="chevron-down"></span>
          In reply to ${replyQuoteAuthor}...
          <div id="${replyQuoteAttr}" class="uk-card uk-card-body uk-card-default uk-card-small uk-text-meta uk-margin-small-bottom uk-margin-small-top custom-quote-padding" hidden>
            <div id="${replyQuoteBodyAttr}" class="uk-margin-remove-first-child uk-margin-remove-last-child">
              <p>
                reply-quote-body-1
              </p>
            </div>
          </div>
        </div>
      `;
    }

    const age = bespoke2.formatSecondsSinceEpoch(message["created"]);
    const replyBodyAttr = `reply-body-${message["id"]}`;
    const replyDividerAttr = `reply-divider-${message["id"]}`;

    let replies = "";
    if (message["reply-count"] > 0) {
      replies = html`• <button onclick=${() => {
                  bespoke2.gotoPage(event, "message2.html", message["id"])
                }} class="uk-icon-button" uk-icon="comments"></button> ${message["reply-count"]}`;
    }

    return html`
      <article class="uk-article uk-margin-remove-top">
        ${replyQuote}
        <!-- Reply body -->
        <div id="${replyBodyAttr}" class="uk-margin-remove-first-child uk-margin-remove-last-child">
          ${bespoke2.uhtmlFormatMarkdown(message["body"])}
        </div>
        <!-- Reply meta-data -->
        <div class="uk-article-meta uk-margin-top-remove">
          <div class="uk-flex uk-flex-between uk-flex-middle">
            <div>
              ${message["author"]} •
              ${age}
              ${replies}
            </div>
            <div>
              <button onclick=${() => {
                message2.openDeleteMessageModal(event)
              }} data-message-id="${message["id"]}" class="uk-icon-button" uk-icon="trash"></button>
              <button onclick=${() => {
                bespoke2.gotoPage(event, "add_reply2.html", message["id"]);
              }} class="uk-icon-button" uk-icon="reply"></button>
            </div>
          </div>
        </div>
      </article>
      <hr id="${replyDividerAttr}" class="uk-margin-small uk-divider-icon">
    `;
  };

  async function updatePage() {
    try {
      // REST: Get parent message
      let response = await fetch("/lookup_messages", {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify([bespoke2.peekMessageStack()])
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      const data = await response.json();
      bespoke2.assert(data.length === 1, "Expected exactly one message");
      const parentMessage = data[0];

      // REST: Get root message title (maybe)
      let rootMessageTitle = parentMessage["title"];
      if (rootMessageTitle == null) {
        response = await fetch("/lookup_messages", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify([parentMessage["root-message-id"]])
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        const rootMessage = await response.json();
        bespoke2.assert(rootMessage.length === 1, "Expected exactly one message");
        rootMessageTitle = rootMessage[0]["title"];
      }

      // REST: Get reply messages
      response = await fetch("/lookup_recursive_messages", {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify(parentMessage["replies"])
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      message2._replyMessages = await response.json();

      populatePage(parentMessage, rootMessageTitle, message2._replyMessages);
    } catch (error) {
      console.error("Fetching failed:", error);
    }
  };

  updatePage();
});
