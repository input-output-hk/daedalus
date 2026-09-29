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

// App-owned registration. Rebinding a recovered renderer retains the owner,
// queue, consent generation and rolling dispatch budget.
export function createAriadneAnalyticsRegistration() {
  let owner: AnalyticsOwner | null = null;
  let detachWindow: (() => void) | null = null;
  let registered = false;
  let disposed = false;
  let binding = 0;

  const dispose = () => {
    disposed = true;
    binding++;
    detachWindow?.();
    detachWindow = null;
    owner?.close();
    app.removeListener('before-quit', dispose);
    if (registered) {
      ipcMain.removeHandler(ARIADNE_ANALYTICS_CONSENT);
      ipcMain.removeHandler(ARIADNE_ANALYTICS_EVENT);
      registered = false;
    }
  };

  const register = (window: BrowserWindow, expectedUrl: string) => {
    if (disposed) throw new Error('Analytics registration is closed');
    const currentBinding = ++binding;
    detachWindow?.();
    if (!owner) owner = createOwner();
    if (registered) {
      ipcMain.removeHandler(ARIADNE_ANALYTICS_CONSENT);
      ipcMain.removeHandler(ARIADNE_ANALYTICS_EVENT);
    } else app.once('before-quit', dispose);
    const trusted = (event: Electron.IpcMainInvokeEvent) =>
      !disposed &&
      currentBinding === binding &&
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
    registered = true;
    window.once('closed', dispose);
    detachWindow = () => window.removeListener('closed', dispose);
  };
  return { register, dispose };
}

function createOwner() {
  // Separate from generic renderer-accessible settings; UUID never crosses IPC.
  let storage: ElectronStore;
  return new AnalyticsOwner(
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
}
