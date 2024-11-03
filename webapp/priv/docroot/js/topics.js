import bespoke from "/js/bespoke.js";

// uhtml.min.js must be imported in the HTML file before this script
const { html, render } = uhtml;

document.addEventListener("DOMContentLoaded", () => {
  bespoke.init();
  bespoke.clearMessageStack();

  const topicsContainer = document.getElementById("topics");

  const populatePage = (rootMessages) => {
    const messageTemplates = rootMessages.map(createMessageTemplate);
    render(topicsContainer, html`${messageTemplates}`);
  };

  const createMessageTemplate = (message) => {
    /*
      Topic message example:

      <div onclick="Bespoke.pushMessageStack(6767); Bespoke.gotoPage(event, 'message.html')"
           class="uk-card uk-card-default uk-card-small uk-card-body uk-padding-small uk-margin-small-bottom message-topic">
        Vad hette egentligen Slas?
        <div class="uk-text-meta">
          Tony Rogvall • 30d •
          <span uk-icon="comment"></span> 189
        </div>
      </div>
    */
    const age = bespoke.formatSecondsSinceEpoch(message.created);
    return html`
      <div onclick=${() => {
           bespoke.pushMessageStack(message["id"]);
           bespoke.gotoPage(event, "message.html");
        }} class="uk-card uk-card-default uk-card-small uk-card-body uk-padding-small uk-margin-small-bottom message-topic">
                ${bespoke.escapeHTML(message["body"])}
                <div class="uk-text-meta">
                  ${bespoke.escapeHTML(message["author"])} • ${age} •
                  <span uk-icon="comment"></span> ${message["reply-count"]}
                </div>
               </div>
            `;
  };

  const fetchRootMessages = async () => {
    try {
      const response = await fetch("/list_root_messages");
      if (!response.ok) {
        throw new Error(`Server error: ${response.status}`);
      }
      const data = await response.json();
      populatePage(data);
      setInterval(() => populatePage(data), 60000);
    } catch (error) {
      console.error("Error fetching root messages:", error);
    }
  };

  fetchRootMessages();
});
