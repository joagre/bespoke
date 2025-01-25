const CACHE_NAME = "trusted-loader-v1";
const URLS_TO_CACHE = [
  "/loader.html",
  "/js/loader.js",
  "/sw.js"
];

self.addEventListener("install", (event) => {
  event.waitUntil(
    caches.open(CACHE_NAME).then((cache) => {
      return Promise.all(
        URLS_TO_CACHE.map(async (url) => {
          try {
            await cache.add(url);
            console.log(`Successfully cached: ${url}`);
          } catch (error) {
            console.error(`Failed to cache ${url}:`, error);
          }
        })
      );
    }).catch((error) => {
      console.error("Failed to open cache:", error);
    })
  );
});

self.addEventListener("fetch", (event) => {
  console.log(`Service Worker: Fetch event for: ${event.request.url}`);
  event.respondWith(
    caches.match(event.request).then((response) => {
      console.log(`Service Worker: Cache: ${response ? "HIT" : "MISS"} for ${event.request.url}`);
      return response || fetch(event.request).catch((error) => {
        console.error(`Service Worker: Network fetch failed for ${event.request.url}:`, error);
        return new Response("Offline and resource not available", {status: 503});
      });
    })
  );
});

// Just for debugging. Can be removed.
self.addEventListener("activate", () => {
  console.log("Service Worker: activate event");
});
