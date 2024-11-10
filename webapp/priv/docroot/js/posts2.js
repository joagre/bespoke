// Import dependencies
import bespoke2 from "/js/bespoke.js";

// Ensure uhtml.min.js is imported in the HTML file before this script
const { html, render } = uhtml;

class Posts2 {
  constructor() {
    this.init();
  }

  init() {
    document.addEventListener("DOMContentLoaded", () => {
      bespoke2.init();
      bespoke2.clearMessageStack();
      this.updatePage();
      // Update page every minute
      setInterval(() => this.updatePage(), 60000);
    });
  }

  async updatePage() {
    try {
      // REST: Get root messages
      const response = await fetch("/list_root_messages");
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        // Optional: Display an error message on the page
        this.displayError("Failed to load messages. Please try again later.");
        return;
      }
      const rootMessages = await response.json();

      this.populatePage(rootMessages);
    } catch (error) {
      console.error("Fetching failed:", error);
      // Optional: Display an error message on the page
      this.displayError("An error occurred while fetching messages.");
    }
  }

  populatePage(rootMessages) {
    // Populate posts
    const postsContainer = document.getElementById("posts");
    if (rootMessages.length === 0) {
      postsContainer.innerHTML = "<p>No messages available.</p>";
      return;
    }
    const messageTemplates =
          rootMessages.map((message) => this.createMessageTemplate(message));
    render(postsContainer, html`${messageTemplates}`);
  }

  createMessageTemplate(message) {
    const age = bespoke2.formatSecondsSinceEpoch(message["created"]);
    return html`
      <div
        onclick=${(event) => {
          bespoke2.gotoPage(event, "message2.html", message["id"]);
        }}
        class="uk-card uk-card-default uk-card-small uk-card-body uk-padding-small uk-margin-small-bottom message-post"
      >
        ${message["title"]}
        <div class="uk-text-meta">
          ${message["author"]} • ${age} •
          <span uk-icon="comment"></span> ${message["reply-count"]}
        </div>
      </div>
    `;
  }

  displayError(message) {
    const postsContainer = document.getElementById("posts");
    postsContainer.innerHTML = `<p class="error-message">${message}</p>`;
  }
}

// Export the class instance
const posts2 = new Posts2();
export default posts2;
