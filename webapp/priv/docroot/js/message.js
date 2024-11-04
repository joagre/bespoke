import bespoke from "/js/bespoke.js";

// uhtml.min.js must be imported in the HTML file before this script
const { html, render } = uhtml;

const message = {
  _messageId: null,

  openDeleteMessageModal(event) {
    event.stopPropagation();
    message._messageId = bespoke.peekMessageStack();
    document.getElementById("delete-message-title").textContent =
      document.getElementById('message-title').textContent;
    UIkit.modal("#delete-message-modal").show();
  },

  deleteMessage() {
    if (message._messageId == null) {
      console.error("No message ID to delete");
      return;
    }

    async function updateServer() {
      try {
        // REST API: /delete_message
        const response = await fetch("/delete_message", {
          method: "POST",
          headers: {
            "Content-Type": "application/json"
          },
          body: JSON.stringify(message._messageId)
        });
        if (!response.ok) {
          console.error(`Server error: ${response.status}`);
          return;
        }
        bespoke.popMessageStack();
        bespoke.navigateTo("topics.html");
      } catch (error) {
        console.error("Fetching failed:", error);
      }
    };
  }
};

export default message;

document.addEventListener("DOMContentLoaded", function() {
  bespoke.init();

  function populatePage(parentMessage, rootMessageTitle, replyMessages) {
    // Populate page title
    document.getElementById("page-title").textContent =
      bespoke.escapeHTML(rootMessageTitle);

    // Populate header
    document.getElementById("message-title").textContent =
      document.getElementById("page-title").textContent;
    document.getElementById("reply-depth").textContent =
      `[${bespoke.messageStackSize()}]`;

    // Populate parent message
    document.getElementById("message-body").textContent =
      bespoke.escapeHTML(parentMessage["body"]);
    document.getElementById("message-author").textContent =
      bespoke.escapeHTML(parentMessage["author"]);
    document.getElementById("message-created").textContent =
      bespoke.formatSecondsSinceEpoch(parentMessage["created"]);
    document.getElementById("message-reply-count").textContent =
      parentMessage["reply-count"];

    // Populate replies
    const repliesContainer = document.getElementById("replies");
    const replyTemplates = replyMessages.map(createReplyTemplate);
    render(repliesContainer, html`${replyTemplates}`);
  };

  function createReplyTemplate(message) {
    /*
      Reply message example:

      <div onclick="bespoke.gotoPage(event, 'message.html', 4716)" class="uk-card uk-card-default uk-card-small uk-card-body uk-padding-small uk-margin-small-bottom message-reply">
        <div>
          Nulla et diam id nisi aliquam maximus. Donec enim ante, placerat eu finibus in, cursus vitae ante. Duis ac tortor augue. Nullam at convallis libero.
          <div class="uk-text-meta uk-margin-small">
            <div class="uk-flex uk-flex-between uk-flex-middle">
              <div>
                Tony Rogvall •
                20d •
                <span uk-icon="comment"></span>
                189
              </div>
              <button onclick="bespoke.gotoPage(event, 'add_reply.html', 5076)" class="uk-icon-button" uk-icon="reply"></button>
            </div>
          </div>
        </div>
      </div>
    */
    const age = bespoke.formatSecondsSinceEpoch(message["created"]);
    return html`
      <div onclick=${() => {
            bespoke.gotoPage(event, "message.html", message["id"]);
          }} class="uk-card uk-card-default uk-card-small uk-card-body uk-padding-small uk-margin-small-bottom message-reply">
        <div>
          ${bespoke.escapeHTML(message["body"])}
          <div class="uk-text-meta uk-margin-small">
            <div class="uk-flex uk-flex-between uk-flex-middle">
              <div>
                ${bespoke.escapeHTML(message["author"])} •
                ${age} •
                <span uk-icon="comment"></span>
                ${message["reply-count"]}
              </div>
              <button onclick=${() => {
                    bespoke.gotoPage(event, "add_reply.html", message["id"]);
                  }} class="uk-icon-button" uk-icon="reply"></button>
            </div>
          </div>
        </div>
      </div>
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
        body: JSON.stringify([bespoke.peekMessageStack()])
      });
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      const data = await response.json();
      bespoke.assert(data.length === 1, "Expected exactly one message");
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
        bespoke.assert(rootMessage.length === 1, "Expected exactly one message");
        rootMessageTitle = rootMessage[0]["title"];
      }

      // REST: Get reply messages
      response = await fetch("/lookup_messages", {
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
      const replyMessages = await response.json();

      // Populate page
      populatePage(parentMessage, rootMessageTitle, replyMessages);
    } catch (error) {
      console.error("Fetching failed:", error);
    }
  };

  updatePage();
});
