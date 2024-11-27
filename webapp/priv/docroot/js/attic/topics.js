import bespoke from "/js/bespoke.js";

// uhtml.min.js must be imported in the HTML file before this script
const { html, render } = uhtml;

document.addEventListener("DOMContentLoaded", () => {
  bespoke.init();
  bespoke.clearMessageStack();

  function populatePage(rootMessages) {
    // Populate topics
    const topicsContainer = document.getElementById("topics");
    const messageTemplates = rootMessages.map(createMessageTemplate);
    render(topicsContainer, html`${messageTemplates}`);
  };

  function createMessageTemplate(message) {
    /*
      Topic message example:

      <div onclick="Bespoke.gotoPage(event, 'message.html', 6767)" class="uk-card uk-card-default uk-card-small uk-card-body uk-padding-small uk-margin-small-bottom message-topic">
        Vad hette egentligen Slas?
        <div class="uk-text-meta">
          Tony Rogvall •
          30d •
          <span uk-icon="comment"></span>
          189
        </div>
      </div>
    */
    const age = bespoke.formatSecondsSinceEpoch(message["created"]);
    return html`
      <div onclick=${() => {
           bespoke.gotoPage(event, "message.html", message["id"]);
        }} class="uk-card uk-card-default uk-card-small uk-card-body uk-padding-small uk-margin-small-bottom message-topic">
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
