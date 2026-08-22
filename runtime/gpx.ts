import type { GpxSample, GpxWaypoint, Ports } from './ports'

// XML to plain numbers. Smoothing, cumulative distance and ascent are decided
// in Core.App.Route, not here.
export function attach(ports: Pick<Ports, 'parseGpx' | 'gpxIn'>): void {
  ports.parseGpx.subscribe((text) => {
    try {
      const xml = new DOMParser().parseFromString(text, 'application/xml')
      if (xml.getElementsByTagName('parsererror').length) {
        ports.gpxIn.send({ ok: false, error: 'parsererror' })
        return
      }

      let nodes = xml.getElementsByTagName('trkpt')
      if (!nodes.length) nodes = xml.getElementsByTagName('rtept')

      const samples: GpxSample[] = []
      for (const node of Array.from(nodes)) {
        const ele = node.getElementsByTagName('ele')[0]
        samples.push({
          lat: parseFloat(node.getAttribute('lat') ?? ''),
          lon: parseFloat(node.getAttribute('lon') ?? ''),
          ele: ele ? parseFloat(ele.textContent ?? '') : 0,
        })
      }

      const waypoints: GpxWaypoint[] = []
      for (const point of Array.from(xml.getElementsByTagName('wpt'))) {
        const name = point.getElementsByTagName('name')[0]
        waypoints.push({
          name: name?.textContent ?? '',
          lat: parseFloat(point.getAttribute('lat') ?? ''),
          lon: parseFloat(point.getAttribute('lon') ?? ''),
        })
      }

      ports.gpxIn.send({ ok: true, name: '', samples, waypoints })
    } catch (error) {
      ports.gpxIn.send({ ok: false, error: String(error) })
    }
  })
}
