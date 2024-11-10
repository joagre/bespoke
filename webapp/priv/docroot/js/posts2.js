import bespoke2 from "/js/bespoke.js";

// uhtml.min.js must be imported in the HTML file before this script
const { html, render } = uhtml;

document.addEventListener("DOMContentLoaded", () => {
  bespoke2.init();
  bespoke2.clearMessageStack();

  function populatePage(rootMessages) {
    // Populate posts
    const postsContainer = document.getElementById("posts");
    const messageTemplates = rootMessages.map(createMessageTemplate);
    render(postsContainer, html`${messageTemplates}`);
  };

  function createMessageTemplate(message) {
    /*
      Post message example:

      <div onclick="bespoke2.gotoPage(event, 'message2.html', 6767)" class="uk-card uk-card-default uk-card-small uk-card-body uk-padding-small uk-margin-small-bottom message-post">
        Vad hette egentligen Slas?
        <div class="uk-text-meta">
          Tony Rogvall •
          30d •
          <span uk-icon="comment"></span>
          189
        </div>
      </div>
    */
    const age = bespoke2.formatSecondsSinceEpoch(message["created"]);
    return html`
      <div onclick=${() => {
           bespoke2.gotoPage(event, "message2.html", message["id"]);
        }} class="uk-card uk-card-default uk-card-small uk-card-body uk-padding-small uk-margin-small-bottom message-post">
                ${message["title"]}
                <div class="uk-text-meta uk-margin-small-top">
                  ${message["author"]} • ${age} •
                  <span uk-icon="comment"></span> ${message["reply-count"]}
                </div>
               </div>
            `;
  };

  async function updatePage() {
    try {
      // REST: Get root messages
      const response = await fetch("/list_root_messages");
      if (!response.ok) {
        console.error(`Server error: ${response.status}`);
        return;
      }
      const rootMessages = await response.json();

      // Populate page
      populatePage(rootMessages);

      // Update page every minute
      setInterval(() => populatePage(rootMessages), 60000);
    } catch (error) {
      console.error("Fetching failed:", error);
    }
  };

  updatePage();
});
