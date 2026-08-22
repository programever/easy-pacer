// The wire contract, mirroring src/Runtime/Ports.elm and the decoders in
// src/Runtime/Gps.elm and src/Runtime/Gpx.elm.
//
// Elm 0.19 generates no types for this, so the mirror is maintained by hand.
// tests/wire/*.json is what keeps it honest: the same fixtures are decoded by
// the Elm tests and type-checked against the types below.

/** Codes are the browser's own: 1 denied, 2 unavailable, 3 timeout.
 *  Runtime.Gps decides what they mean; nothing here interprets them. */
export type GpsFailure = { ok: false; code: 1 | 2 | 3 }

export type GpsSuccess = {
  ok: true
  lat: number
  lon: number
  /** Metres, exactly as the sensor reported it. */
  accuracy: number
  /** Epoch milliseconds. */
  taken: number
}

export type GpsMessage = GpsSuccess | GpsFailure

export type GpxSample = { lat: number; lon: number; ele: number }
export type GpxWaypoint = { name: string; lat: number; lon: number }
export type GpxFailure = { ok: false; error: string }

export type GpxSuccess = {
  ok: true
  name: string
  samples: GpxSample[]
  waypoints: GpxWaypoint[]
}

export type GpxMessage = GpxSuccess | GpxFailure

export type SmsRequest = { phone: string; body: string }

export type Ports = {
  requestGps: Elm.OutgoingPort<null>
  gpsIn: Elm.IncomingPort<GpsMessage>
  parseGpx: Elm.OutgoingPort<string>
  gpxIn: Elm.IncomingPort<GpxMessage>
  /** `unknown` on purpose. The snapshot's shape belongs to Storage.Snapshot;
   *  naming it here would invite this layer to read it, and reading it is
   *  the beginning of deciding. */
  save: Elm.OutgoingPort<unknown>
  clear: Elm.OutgoingPort<null>
  copyText: Elm.OutgoingPort<string>
  openSms: Elm.OutgoingPort<SmsRequest>
}
