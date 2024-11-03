const { html, render } = uhtml;

document.addEventListener("DOMContentLoaded", function() {
    Bespoke.init();
    Bespoke.clearMessageStack();

    function populatePage(rootMessages) {
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

        render(document.getElementById("topics"), html`
          ${rootMessages.map(message => {
            const age = Bespoke.formatSecondsSinceEpoch(message["created"]);
            return html`
              <div onclick=${() => {
                Bespoke.pushMessageStack(message["id"]);
                Bespoke.gotoPage(event, "message.html");
              }} class="uk-card uk-card-default uk-card-small uk-card-body uk-padding-small uk-margin-small-bottom message-topic">
                ${message["body"]}
                <div class="uk-text-meta">
                  ${message["author"]} • ${age} •
                  <span uk-icon="comment"></span> ${message["reply-count"]}
                </div>
               </div>
            `;
          })}`
        );
    };

    fetch("/list_root_messages")
      .then(response => {
          if (!response.ok) {
              throw new Error(`Server error: ${response.status}`);
          }
          return response.json();
      })
      .then(data => {
          populatePage(data);
          setInterval(() => {
              populatePage(data);
          }, 60000);
      })
      .catch(error => console.error("Error:", error));
});
