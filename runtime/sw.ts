/// <reference lib="webworker" />

// The service worker: keeps the one-file app on the phone.
//
// The app is used on a mountain with no signal. Without this, a refresh out
// there asks the network for index.html and gets nothing. With it, the first
// visit stores the file, every later load is answered from that copy, and
// whenever there is signal the copy is refreshed in the background for next
// time. Nothing else is cached: there is nothing else to cache.
//
// Plumbing only. It never reads what it stores.

// A module, so that this `self` shadows the worker global instead of
// colliding with it.
export {}
declare const self: ServiceWorkerGlobalScope

const CACHE = 'tram-ke-shell'

/** The app's own URL, which is also the service worker's scope. */
function shell(): string {
  return self.registration.scope
}

self.addEventListener('install', (event) => {
  event.waitUntil(
    caches
      .open(CACHE)
      .then((cache) => cache.add(new Request(shell(), { cache: 'reload' })))
      .then(() => self.skipWaiting()),
  )
})

self.addEventListener('activate', (event) => {
  event.waitUntil(self.clients.claim())
})

self.addEventListener('fetch', (event) => {
  const request = event.request
  if (request.method !== 'GET') return

  const url = new URL(request.url)
  const wantsShell =
    url.origin === self.location.origin &&
    (request.mode === 'navigate' || url.href === shell() || url.pathname.endsWith('/index.html'))
  if (!wantsShell) return

  event.respondWith(
    (async () => {
      const cache = await caches.open(CACHE)
      const stored = await cache.match(shell())

      const refresh = fetch(request)
        .then((response) => {
          if (response.ok) void cache.put(shell(), response.clone())
          return response
        })
        .catch(() => undefined)

      if (stored) {
        event.waitUntil(refresh)
        return stored
      }

      const fresh = await refresh
      return fresh ?? new Response(null, { status: 503 })
    })(),
  )
})
