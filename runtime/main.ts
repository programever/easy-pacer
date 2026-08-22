import Main from '../src/Main.elm'
import '../styles/app.css'
import * as geolocation from './geolocation'
import * as gpx from './gpx'
import * as handoff from './handoff'
import { install } from './install'
import * as storage from './storage'
import type { Ports } from './ports'

// Everything under runtime/ is plumbing. It reads a sensor, parses XML,
// touches storage or hands off to another app. It never decides anything: no
// milestone is chosen here, no distance judged, no user-facing text built.
// That boundary is what keeps Elm's guarantee meaningful.

const node = document.getElementById('app')
const app = Main.init<Ports>({ node, flags: { snapshot: storage.read() } })

if (!app.ports) throw new Error('Main.elm exposed no ports')

// Elm omits an outgoing port that no Elm code ever commands, so a port
// declared in Runtime/Ports.elm can still be absent here. Attaching each
// group independently keeps one unfinished feature from unwiring the rest;
// the warning names the port so the gap stays visible rather than silent.
const ports = app.ports

function wire(name: string, required: (keyof Ports)[], attach: () => void): void {
  const missing = required.filter((port) => ports[port] === undefined)
  if (missing.length > 0) {
    console.warn(
      `runtime: ${name} not wired, Elm exposes no ${missing.join(', ')} port. ` +
        `Declared in Runtime/Ports.elm but never commanded from Elm, so the ` +
        `compiler dropped it.`,
    )
    return
  }
  attach()
}

wire('geolocation', ['requestGps', 'gpsIn'], () => geolocation.attach(ports))
wire('gpx', ['parseGpx', 'gpxIn'], () => gpx.attach(ports))
wire('storage', ['save', 'clear'], () => storage.attach(ports))
wire('handoff', ['copyText', 'openSms'], () => handoff.attach(ports))

install()
