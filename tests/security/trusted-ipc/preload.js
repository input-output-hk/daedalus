const { ipcRenderer } = require('electron');

const request = (message) =>
  new Promise((resolve, reject) => {
    const requestId = crypto.randomUUID();
    const listener = (_event, envelope) => {
      if (envelope.requestId !== requestId) return;
      ipcRenderer.removeListener('trusted-ipc-probe-response', listener);
      if (envelope.isOk) resolve(envelope.response);
      else reject(envelope.response);
    };
    ipcRenderer.on('trusted-ipc-probe-response', listener);
    ipcRenderer.send('trusted-ipc-probe-request', { requestId, message });
  });

ipcRenderer.on('trusted-ipc-start', async () => {
  ipcRenderer.send('trusted-ipc-origin-probe');
  const results = await Promise.all([request('first'), request('second')]);
  ipcRenderer.send('trusted-ipc-results', results);
});
