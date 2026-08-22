import type { Ports } from './ports'

// Handing text to another app. Core.App.Sos composes the message; this only
// carries it.
export function attach(ports: Pick<Ports, 'copyText' | 'openSms'>): void {
  ports.copyText.subscribe((text) => {
    if (navigator.clipboard) {
      navigator.clipboard.writeText(text).catch(() => {
        // Elm has already told the runner the copy was made. A silent failure
        // here leaves the message visible on screen to copy by hand.
      })
    }
  })

  ports.openSms.subscribe(({ phone, body }) => {
    // iOS and Android disagree on the separator before the body parameter.
    const isApple = /iPad|iPhone|iPod/.test(navigator.userAgent || '')
    const separator = isApple ? '&' : '?'
    window.location.href =
      'sms:' + phone.replace(/[^0-9+]/g, '') + separator + 'body=' + encodeURIComponent(body)
  })
}
