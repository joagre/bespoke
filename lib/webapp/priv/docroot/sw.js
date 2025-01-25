const CACHE_NAME = "trusted-loader-v1";
const URLS_TO_CACHE = [
  "/loader.html",
  "/js/loader.js"
];

self.addEventListener("install", (event) => {
  event.waitUntil(
    caches.open(CACHE_NAME).then((cache) => {
      return Promise.all(
        URLS_TO_CACHE.map((url) =>
          cache.add(url).catch((error) => {
            console.error(`Failed to cache ${url}:`, error);
          }))
      );
    }).catch((error) => {
      console.error("Failed to open cache:", error);
    })
  );
});

self.addEventListener("fetch", (event) => {
  console.log(`Fetch event for: ${event.request.url}`);
  event.respondWith(
    caches.match(event.request).then((response) => {
      console.log(`Cache: ${response ? "HIT" : "MISS"} for ${event.request.url}`);
      return response || fetch(event.request);
    })
  );
});

self.addEventListener("install", () => {
  console.log("SW: install event");
});

self.addEventListener("activate", () => {
  console.log("SW: activate event");
});
