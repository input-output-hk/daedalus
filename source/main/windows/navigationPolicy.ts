import path from 'path';
import { pathToFileURL, URL as NodeUrl } from 'url';
import type { App, BrowserWindow, WebContents } from 'electron';

export const ERR_ABORTED = -3;

export const getTrustedRendererUrl = (
  isDev: boolean,
  mainDirectory: string
): NodeUrl =>
  isDev
    ? new NodeUrl('http://127.0.0.1:8080/')
    : pathToFileURL(path.resolve(mainDirectory, '../renderer/index.html'));

export const isTrustedDocumentUrl = (
  candidate: string,
  trustedRendererUrl: NodeUrl
): boolean => {
  try {
    const parsed = new NodeUrl(candidate);
    const expected = new NodeUrl(trustedRendererUrl.href);
    parsed.hash = '';
    expected.hash = '';
    return parsed.href === expected.href;
  } catch (_error) {
    return false;
  }
};

export const installTrustedWindowNavigationPolicy = (
  webContents: WebContents,
  trustedRendererUrl: NodeUrl
): void => {
  const preventUntrustedMainFrameNavigation = (event: {
    url: string;
    preventDefault: () => void;
  }) => {
    if (!isTrustedDocumentUrl(event.url, trustedRendererUrl)) {
      event.preventDefault();
    }
  };

  webContents.on('will-navigate', preventUntrustedMainFrameNavigation);
  webContents.on('will-redirect', preventUntrustedMainFrameNavigation);
  webContents.on('will-frame-navigate', (event) => {
    if (
      !event.isMainFrame ||
      !isTrustedDocumentUrl(event.url, trustedRendererUrl)
    ) {
      event.preventDefault();
    }
  });
};

export const installGlobalPopupPolicy = (electronApp: App): void => {
  electronApp.on('web-contents-created', (_event, contents) => {
    contents.setWindowOpenHandler(() => ({ action: 'deny' }));
  });
};

export const loadTrustedRenderer = (
  window: BrowserWindow,
  trustedRendererUrl: NodeUrl
): Promise<void> => {
  installTrustedWindowNavigationPolicy(window.webContents, trustedRendererUrl);
  return window.loadURL(trustedRendererUrl.href);
};

export const bindWindowRecovery = <T>(
  createWindow: (locale: string, getBounds: () => T) => unknown,
  locale: string,
  getBounds: () => T
): (() => unknown) => () => createWindow(locale, getBounds);

export const shouldRecoverFailedLoad = (
  errorCode: number,
  isMainFrame: boolean
): boolean => isMainFrame && errorCode !== ERR_ABORTED;

export const installFailedLoadRecovery = (
  webContents: WebContents,
  recover: (event: Electron.Event) => void
): void => {
  webContents.on(
    'did-fail-load',
    (event, errorCode, _errorDescription, _validatedURL, isMainFrame) => {
      if (shouldRecoverFailedLoad(errorCode, isMainFrame)) recover(event);
    }
  );
};
