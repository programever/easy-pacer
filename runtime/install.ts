// Makes the page installable and keeps it available offline.
//
// Two pieces, both plumbing. The manifest lets a phone put the app on its
// home screen and open it without browser chrome. The service worker keeps
// index.html on the phone so that a refresh with no signal still opens it.
// Neither runs under the dev server, where the file being cached is not the
// built one.

const icon =
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512">' +
  '<rect width="512" height="512" rx="96" fill="#101823"/>' +
  '<path d="M96 352 L176 240 L224 296 L304 160 L352 232 L416 352 Z" fill="#FF7A2F"/>' +
  '<circle cx="304" cy="160" r="22" fill="#EEF3F7"/>' +
  '</svg>'

export function install(): void {
  if (!import.meta.env.PROD) return

  const here = new URL('./', window.location.href).href

  const manifest = {
    name: 'Trạm Kế',
    short_name: 'Trạm Kế',
    start_url: here,
    scope: here,
    display: 'standalone',
    orientation: 'portrait',
    background_color: '#101823',
    theme_color: '#101823',
    icons: [
      {
        src: 'data:image/svg+xml,' + encodeURIComponent(icon),
        sizes: 'any',
        type: 'image/svg+xml',
        purpose: 'any',
      },
    ],
  }

  const link = document.createElement('link')
  link.rel = 'manifest'
  link.href = 'data:application/manifest+json,' + encodeURIComponent(JSON.stringify(manifest))
  document.head.appendChild(link)

  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.register('./sw.js').catch(() => {
      // An older browser, or a file:// open. The app still runs; it just is
      // not kept for the next refresh.
    })
  }
}
