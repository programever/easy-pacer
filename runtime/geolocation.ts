import type { Ports } from './ports'

// One reading under tree cover can be tens of metres out, so listen for a few
// seconds and keep the most accurate sample seen. Still user initiated: the
// watch is torn down as soon as it answers.
const WINDOW_MS = 9000
const GOOD_ENOUGH_M = 12

export function attach(ports: Pick<Ports, 'requestGps' | 'gpsIn'>): void {
  ports.requestGps.subscribe(() => {
    if (!navigator.geolocation) {
      ports.gpsIn.send({ ok: false, code: 2 })
      return
    }

    let best: GeolocationPosition | null = null
    let settled = false
    let watchId: number | null = null
    let timer: ReturnType<typeof setTimeout> | null = null

    const stop = () => {
      if (watchId !== null) {
        try {
          navigator.geolocation.clearWatch(watchId)
        } catch {
          // A cleared watch that was never registered is not a failure.
        }
      }
      if (timer) clearTimeout(timer)
    }

    const settle = () => {
      if (settled) return
      settled = true
      stop()
      if (best) {
        ports.gpsIn.send({
          ok: true,
          lat: best.coords.latitude,
          lon: best.coords.longitude,
          accuracy: best.coords.accuracy,
          taken: Date.now(),
        })
      } else {
        ports.gpsIn.send({ ok: false, code: 3 })
      }
    }

    timer = setTimeout(settle, WINDOW_MS)
    watchId = navigator.geolocation.watchPosition(
      (position) => {
        if (!best || position.coords.accuracy < best.coords.accuracy) best = position
        if (best.coords.accuracy <= GOOD_ENOUGH_M) settle()
      },
      (error) => {
        if (best) return
        settled = true
        stop()
        const code = error?.code === 1 || error?.code === 3 ? error.code : 2
        ports.gpsIn.send({ ok: false, code })
      },
      { enableHighAccuracy: true, timeout: WINDOW_MS, maximumAge: 0 },
    )
  })
}
