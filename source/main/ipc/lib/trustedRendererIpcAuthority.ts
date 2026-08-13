import type {
  BrowserWindow,
  IpcMainEvent,
  WebContents,
  WebFrameMain,
} from 'electron';
import type { URL as NodeUrl } from 'url';
import type { IpcAuthorization } from '../../../common/ipc/lib/IpcChannel';
import { isTrustedDocumentUrl } from '../../windows/navigationPolicy';

type Binding = {
  generation: number;
  webContents: WebContents;
  trustedUrl: NodeUrl;
  frame: WebFrameMain | null;
  committedFrameId: string | null;
  invalidationListeners: Set<() => void>;
};

let generation = 0;
let binding: Binding | null = null;

const invalidate = (current: Binding | null): void => {
  if (!current) return;
  current.frame = null;
  current.committedFrameId = null;
  const listeners = [...current.invalidationListeners];
  current.invalidationListeners.clear();
  for (const listener of listeners) {
    try {
      listener();
    } catch (_error) {
      // One stale caller must not prevent the remaining requests from cleanup.
    }
  }
};

const isLiveFrame = (current: Binding, frame: WebFrameMain): boolean =>
  binding === current &&
  current.frame === frame &&
  !current.webContents.isDestroyed() &&
  !frame.detached &&
  !frame.isDestroyed() &&
  current.webContents.mainFrame === frame &&
  isTrustedDocumentUrl(frame.url, current.trustedUrl) &&
  frame.origin === getExpectedOrigin(current.trustedUrl);

const getExpectedOrigin = (trustedUrl: NodeUrl): string =>
  trustedUrl.protocol === 'file:' ? 'file://' : trustedUrl.origin;

const frameId = (processId: number, routingId: number): string =>
  `${processId}:${routingId}`;

export const bindTrustedRenderer = (
  window: BrowserWindow,
  trustedUrl: NodeUrl
): number => {
  invalidate(binding);
  const current: Binding = {
    generation: ++generation,
    webContents: window.webContents,
    trustedUrl,
    frame: null,
    committedFrameId: null,
    invalidationListeners: new Set(),
  };
  binding = current;

  window.webContents.on('did-start-navigation', (event) => {
    if (binding !== current || !event.isMainFrame || event.isSameDocument)
      return;
    invalidate(current);
  });
  window.webContents.on(
    'did-frame-navigate',
    (_event, url, status, _statusText, isMainFrame, processId, routingId) => {
      if (
        binding !== current ||
        !isMainFrame ||
        (trustedUrl.protocol.startsWith('http') &&
          (status < 200 || status >= 400)) ||
        !isTrustedDocumentUrl(url, trustedUrl)
      )
        return;
      current.committedFrameId = frameId(processId, routingId);
    }
  );
  window.webContents.on(
    'did-frame-finish-load',
    (_event, isMainFrame, frameProcessId, frameRoutingId) => {
      if (binding !== current || !isMainFrame) return;
      if (current.committedFrameId !== frameId(frameProcessId, frameRoutingId))
        return;
      const frame = window.webContents.mainFrame;
      if (
        frame.processId !== frameProcessId ||
        frame.routingId !== frameRoutingId ||
        !isTrustedDocumentUrl(frame.url, trustedUrl) ||
        frame.origin !== getExpectedOrigin(trustedUrl) ||
        frame.detached ||
        frame.isDestroyed()
      )
        return;
      current.frame = frame;
    }
  );
  const clear = () => {
    if (binding !== current) return;
    invalidate(current);
    binding = null;
  };
  window.webContents.once('destroyed', clear);
  window.webContents.once('render-process-gone', clear);
  window.once('closed', clear);
  return current.generation;
};

export const authorizeTrustedRenderer = (
  event: IpcMainEvent
): IpcAuthorization | null => {
  const current = binding;
  const frame = event.senderFrame;
  if (
    !current ||
    !frame ||
    event.sender !== current.webContents ||
    !isLiveFrame(current, frame)
  )
    return null;
  return {
    isCurrent: () => isLiveFrame(current, frame),
    onInvalidated: (listener) => {
      current.invalidationListeners.add(listener);
      return () => current.invalidationListeners.delete(listener);
    },
  };
};

export const isTrustedRendererEvent = (event: IpcMainEvent): boolean =>
  authorizeTrustedRenderer(event) !== null;

export const onTrustedRendererInvalidated = (
  listener: () => void
): (() => void) => {
  const current = binding;
  if (!current || !current.frame) {
    listener();
    return () => {};
  }
  current.invalidationListeners.add(listener);
  return () => current.invalidationListeners.delete(listener);
};

export const clearTrustedRendererForTests = (): void => {
  invalidate(binding);
  binding = null;
};
