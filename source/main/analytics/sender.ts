// Kept structural so sender checks can be tested without opening Electron.
export function isAnalyticsSender(
  event: { sender: unknown; senderFrame?: unknown },
  contents: { mainFrame: unknown; isDestroyed(): boolean },
  frameUrl: string,
  expectedUrl: string
): boolean {
  if (
    contents.isDestroyed() ||
    event.sender !== contents ||
    !event.senderFrame ||
    event.senderFrame !== contents.mainFrame
  )
    return false;
  try {
    const actual = new URL(frameUrl);
    actual.hash = '';
    const expected = new URL(expectedUrl);
    expected.hash = '';
    return actual.href === expected.href;
  } catch {
    return false;
  }
}
