import type { Ports } from './ports'

const KEY = 'tram-ke.v1'

/** Whatever was stored, handed to Elm as flags. Storage.Snapshot decodes it;
 *  a snapshot this layer cannot parse is simply absent, never half applied. */
export function read(): unknown {
  try {
    const raw = window.localStorage.getItem(KEY)
    return raw ? JSON.parse(raw) : null
  } catch {
    return null
  }
}

export function attach(ports: Pick<Ports, 'save' | 'clear'>): void {
  ports.save.subscribe((snapshot) => {
    try {
      window.localStorage.setItem(KEY, JSON.stringify(snapshot))
    } catch {
      // A full or disabled store must not take the race down with it.
    }
  })

  ports.clear.subscribe(() => {
    try {
      window.localStorage.removeItem(KEY)
    } catch {
      // As above.
    }
  })
}
