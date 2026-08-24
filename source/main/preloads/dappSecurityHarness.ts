import { ipcRenderer } from 'electron';
import type { PrivilegedIpcManifestEntry } from '../ipc/privilegedIpcManifest';

const send = (entry: PrivilegedIpcManifestEntry): void => {
  const requestId = `${Date.now()}-${Math.random()}`;
  if (entry.transport === 'conversation') {
    ipcRenderer.send(entry.channel, {
      conversationId: requestId,
      isResponse: false,
      message: entry.channel,
    });
    return;
  }
  ipcRenderer.send(
    `${entry.channel}-${
      entry.receive === 'broadcast' ? 'broadcast' : 'request'
    }`,
    { requestId, message: entry.channel }
  );
};

ipcRenderer.on(
  'dapp-security-harness-send',
  (_event, entries: PrivilegedIpcManifestEntry[]) => {
    entries.forEach(send);
    ipcRenderer.send('dapp-security-harness-sent');
  }
);
