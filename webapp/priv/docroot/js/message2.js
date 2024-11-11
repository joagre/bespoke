// Import dependencies
import bespoke2 from "/js/bespoke2.js";

// uhtml.min.js must be imported in the HTML file before this script
const { html, render } = uhtml;

class Message2 {
  constructor() {
    this._dataLoaded = false;
    this._domReady = false;
    this._parentMessage = null;
    this._rootMessageTitle = null;
    this._replyMessages = [];
    this._messageIdToDelete = null;

    // Initialize the class
    this.init();
  }

  init() {
    bespoke2.initializeCookieState();
    document.addEventListener("DOMContentLoaded", () => {
      bespoke2.init();
      this._domReady = true;
      if (this._dataLoaded) {
        this._populatePage();
      }
    });
    this.loadData();
  }

  async loadData() {
    if (!document.cookie.includes("bespoke")) {
      console.log("Cookie not found, retrying in 1 second");
      setTimeout(() => this.loadData(), 1000);
      return;
    }

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
      this._parentMessage = data[0];

      // REST: Get root message title (maybe)
      this._rootMessageTitle = this._parentMessage["title"];
      if (this._rootMessageTitle == null) {
        response = await fetch("/lookup_messages", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify([this._parentMessage["root-message-id"]])
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        const rootMessage = await response.json();
        bespoke2.assert(rootMessage.length === 1,
                        "Expected exactly one message");
        this._rootMessageTitle = rootMessage[0]["title"];
      }

      // REST: Get reply messages
      response = await fetch("/lookup_recursive_messages", {
        method: "POST",
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify(this._parentMessage["replies"])
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      this._replyMessages = await response.json();
      this._dataLoaded = true;

      if (this._domReady) {
        this._populatePage();
      }
    } catch (error) {
      console.error("Fetching failed:", error);
    }
  }

  _populatePage() {
    const messageStackSize = bespoke2.messageStackSize();

    // Populate head title
    const headTitle = messageStackSize > 1 ? "Reply" : "Post";
    document.getElementById("head-title").textContent = headTitle;

    // Populate header
    const headerTitle = messageStackSize > 1 ? "Reply" : "Post";
    document.getElementById("header-title").textContent = headerTitle;

    if (messageStackSize > 1) {
      document.getElementById(
        "header-reply-level").textContent = `[level: ${messageStackSize - 1}]`;
    } else {
      document.getElementById("header-reply-level").style.display = "none";
    }

    // Populate parent message
    document.getElementById("parent-title").innerHTML = this._rootMessageTitle;
    document.getElementById("parent-body").innerHTML =
      bespoke2.formatMarkdown(this._parentMessage["body"]);
    document.getElementById("parent-author").textContent =
      this._parentMessage["author"];
    document.getElementById("parent-age").textContent =
      bespoke2.formatSecondsSinceEpoch(this._parentMessage["created"]);
    document.getElementById("parent-replies").textContent =
      this._parentMessage["reply-count"];
    document.getElementById("parent-delete")
      .setAttribute("data-message-id", this._parentMessage["id"]);

    // Populate replies
    const repliesContainer = document.getElementById("replies");
    const replyTemplates = this._replyMessages.map((replyMessage) =>
      this._createReplyTemplate(this._parentMessage, replyMessage,
                                this._replyMessages)
    );
    render(repliesContainer, html`${replyTemplates}`);
  }

  _createReplyTemplate(parentMessage, message, replyMessages) {
    // A reply quote is only added if someone replies to a reply
    let replyQuote = "";
    if (message["parent-message-id"] != null &&
        message["parent-message-id"] != parentMessage["id"]) {
      const replyQuoteButtonAttr = `reply-quote-button-${message["id"]}`;
      const replyQuoteAttr = `reply-quote-${message["id"]}`;
      const replyQuoteBodyAttr = `reply-quote-body-${message["id"]}`;
      const parentReplyMessage = replyMessages.find(
        (replyMessage) => replyMessage["id"] == message["parent-message-id"]
      );
      const replyQuoteAuthor =
            parentReplyMessage ? parentReplyMessage["author"] : "Unknown";
      replyQuote = html`
        <!-- Reply quote -->
        <div
          class="uk-text-meta quote"
          onclick=${(event) => this.toggleQuote(event)}
          data-message-id="${message["id"]}"
          data-parent-message-id="${message["parent-message-id"]}">
          <span id="${replyQuoteButtonAttr}" class="uk-icon-link" uk-icon="chevron-down"></span>
          In reply to ${replyQuoteAuthor}...
          <div
            id="${replyQuoteAttr}"
            class="uk-card uk-card-body uk-card-default uk-card-small uk-text-meta uk-margin-small-bottom uk-margin-small-top custom-quote-padding"
            hidden
          >
            <div id="${replyQuoteBodyAttr}" class="uk-margin-remove-first-child uk-margin-remove-last-child">
              <p>Loading...</p>
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
      replies = html`•
        <button
          onclick=${(event) => bespoke2.gotoPage(event, "message2.html", message["id"])}
          class="uk-icon-button"
          uk-icon="comments"
        ></button>
        ${message["reply-count"]}`;
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
              ${message["author"]} • ${age} ${replies}
            </div>
            <div>
              <button
                onclick=${(event) => this.openDeleteMessageModal(event)}
                data-message-id="${message["id"]}"
                class="uk-icon-button"
                uk-icon="trash"
              ></button>
              <button
                onclick=${(event) => addReply2.gotoAddReplyPage(event, message["id"], true)}
                class="uk-icon-button"
                uk-icon="reply"
              ></button>
            </div>
          </div>
        </div>
      </article>
      <hr id="${replyDividerAttr}" class="uk-margin-small uk-divider-icon">
    `;
  }

  openDeleteMessageModal(event) {
    event.stopPropagation();
    // Extract message to delete
    this._messageIdToDelete =
      event.currentTarget.getAttribute("data-message-id");
    // Update the message modal body
    const replyMessage = this._replyMessages.find(
      (replyMessage) => replyMessage["id"] === this._messageIdToDelete
    );
    const messageAuthor = replyMessage ? replyMessage["author"] : "this post";
    document.getElementById("delete-message-body").innerHTML =
      `Do you really want to delete this message written by ${messageAuthor}?`;
    UIkit.modal("#delete-message-modal").show();
  }

  deleteMessage(event) {
    const messageId =
          this._messageIdToDelete === bespoke2.peekMessageStack() ? -1 : null;

    const updateServer = async () => {
      try {
        // REST API: /delete_message
        const response = await fetch("/delete_message", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(this._messageIdToDelete)
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        bespoke2.gotoPage(event, "message2.html", messageId);
        this._messageIdToDelete = null;
      } catch (error) {
        console.error("Fetching failed:", error);
      }
    };

    updateServer();
  }

  toggleQuote(event) {
    event.preventDefault();
    const messageId = event.currentTarget.getAttribute("data-message-id");
    const parentMessageId =
          event.currentTarget.getAttribute("data-parent-message-id");
    const replyQuote = document.getElementById(`reply-quote-${messageId}`);
    const isHidden = replyQuote.hidden;
    const replyQuoteButton =
          document.getElementById(`reply-quote-button-${messageId}`);

    if (isHidden) {
      const replyQuoteBody =
            document.getElementById(`reply-quote-body-${messageId}`);
      const replyBody =
            document.getElementById(`reply-body-${parentMessageId}`);
      replyQuoteBody.innerHTML = replyBody.innerHTML;
      replyQuote.hidden = false;
      replyQuoteButton.setAttribute("uk-icon", "chevron-up");
    } else {
      replyQuote.hidden = true;
      replyQuoteButton.setAttribute("uk-icon", "chevron-down");
    }
  }
}

// Export the class instance
const message2 = new Message2();
export default message2;
