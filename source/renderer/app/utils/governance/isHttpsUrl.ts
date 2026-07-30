/**
 * Main rejects every non-https external URL and the rejection is fire-and-forget,
 * so a non-https link would silently do nothing. The renderer therefore offers a
 * link only for schemes main will actually open, and renders the rest as text.
 */
export function isHttpsUrl(url: string): boolean {
  try {
    return new URL(url).protocol === 'https:';
  } catch {
    return false;
  }
}
