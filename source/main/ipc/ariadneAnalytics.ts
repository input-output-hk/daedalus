import { app, BrowserWindow, ipcMain } from 'electron';
import ElectronStore from 'electron-store';
import {
  ARIADNE_ANALYTICS_CONSENT,
  ARIADNE_ANALYTICS_EVENT,
} from '../../common/ipc/api';
import { AnalyticsOwner } from '../analytics/AnalyticsOwner';
import { analyticsConfig } from '../analytics/config';
import { postAnalytics } from '../analytics/transport';
import { isAnalyticsSender } from '../analytics/sender';
import { environment } from '../environment';
import { getShortCpuDescription } from '../../common/analytics/cpu';

let registeredCleanup: (() => void) | null = null;

export function registerAriadneAnalytics(
  window: BrowserWindow,
  expectedUrl: string
) {
  // Renderer recovery can create a replacement window before closing the old one.
  // Transfer ownership, rather than registering duplicate global IPC handlers.
  registeredCleanup?.();
  // Separate from generic renderer-accessible settings; UUID never crosses IPC.
  let storage: ElectronStore;
  const owner = new AnalyticsOwner(
    analyticsConfig(process.env, app.isPackaged),
    {
      read: () => {
        storage = new ElectronStore({ name: 'ariadne-analytics' });
        return storage.get(environment.network);
      },
      write: (value) => storage.set(environment.network, value),
    },
    {
      platform: process.platform,
      osVersion: environment.platformVersion,
      ram: environment.ram,
      cpu: getShortCpuDescription(environment.cpu[0]?.model),
      appVersion: environment.version,
      network: environment.network,
    },
    postAnalytics
  );
  const trusted = (event: Electron.IpcMainInvokeEvent) =>
    isAnalyticsSender(
      event,
      window.webContents,
      event.senderFrame?.url || '',
      expectedUrl
    );
  ipcMain.handle(ARIADNE_ANALYTICS_CONSENT, (event, command: unknown) =>
    trusted(event) ? owner.consent(command) : null
  );
  ipcMain.handle(
    ARIADNE_ANALYTICS_EVENT,
    (event, payload: unknown) => trusted(event) && owner.enqueue(payload)
  );
  const stop = () => owner.close();
  app.on('before-quit', stop);
  const dispose = () => {
    if (registeredCleanup !== dispose) return;
    registeredCleanup = null;
    stop();
    app.removeListener('before-quit', stop);
    window.removeListener('closed', dispose);
    ipcMain.removeHandler(ARIADNE_ANALYTICS_CONSENT);
    ipcMain.removeHandler(ARIADNE_ANALYTICS_EVENT);
  };
  registeredCleanup = dispose;
  window.once('closed', dispose);
}
