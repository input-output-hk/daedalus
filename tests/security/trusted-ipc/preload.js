const { ipcRenderer } = require('electron');

const requestIncoming = ({ channel, transport, receive }) =>
  new Promise((resolve, reject) => {
    const isConversation = transport === 'conversation';
    const requestId = crypto.randomUUID();
    const responseChannel = isConversation ? channel : `${channel}-response`;
    const requestChannel = isConversation
      ? channel
      : `${channel}-${receive === 'broadcast' ? 'broadcast' : 'request'}`;
    const listener = (_responseEvent, envelope) => {
      const matches = isConversation
        ? envelope.conversationId === requestId && envelope.isResponse
        : envelope.requestId === requestId;
      if (!matches) return;
      ipcRenderer.removeListener(responseChannel, listener);
      const response = isConversation ? envelope.message : envelope.response;
      if (envelope.isOk) resolve(response);
      else reject(response);
    };
    ipcRenderer.on(responseChannel, listener);
    ipcRenderer.send(
      requestChannel,
      isConversation
        ? {
            conversationId: requestId,
            isResponse: false,
            message: channel,
          }
        : { requestId, message: channel }
    );
  });

ipcRenderer.on('trusted-ipc-start', async (_event, entries) => {
  ipcRenderer.send('trusted-ipc-origin-probe');
  const results = await Promise.all(entries.map(requestIncoming));
  ipcRenderer.send('trusted-ipc-results', results);
});

ipcRenderer.on('hostile-ipc-start', (_event, entries) => {
  for (const entry of entries) {
    const id = crypto.randomUUID();
    if (entry.transport === 'conversation') {
      ipcRenderer.send(entry.channel, {
        conversationId: id,
        isResponse: false,
        message: entry.channel,
      });
    } else {
      ipcRenderer.send(
        `${entry.channel}-${
          entry.receive === 'broadcast' ? 'broadcast' : 'request'
        }`,
        { requestId: id, message: entry.channel }
      );
    }
  }
  ipcRenderer.send('hostile-ipc-sent');
});

let outgoingPending = [];
ipcRenderer.on('setup-outgoing-ipc', (_event, entries) => {
  outgoingPending = [];
  for (const entry of entries) {
    const endpoint =
      entry.transport === 'conversation'
        ? entry.channel
        : `${entry.channel}-broadcast`;
    ipcRenderer.once(endpoint, (_requestEvent, envelope) => {
      outgoingPending.push({
        channel: entry.channel,
        transport: entry.transport,
        id:
          entry.transport === 'conversation'
            ? envelope.conversationId
            : envelope.requestId,
      });
      if (outgoingPending.length === entries.length)
        ipcRenderer.send('outgoing-ipc-pending', outgoingPending);
    });
  }
  ipcRenderer.send('outgoing-ipc-ready');
});

ipcRenderer.on('release-outgoing-ipc', () => {
  for (const pending of outgoingPending) {
    ipcRenderer.send(
      pending.transport === 'conversation'
        ? pending.channel
        : `${pending.channel}-response`,
      pending.transport === 'conversation'
        ? {
            conversationId: pending.id,
            isResponse: true,
            isOk: true,
            message: `${pending.channel}-ack`,
          }
        : {
            requestId: pending.id,
            isOk: true,
            response: `${pending.channel}-ack`,
          }
    );
  }
});

ipcRenderer.on('hostile-spoof-responses', (_event, pendingEntries) => {
  for (const pending of pendingEntries) {
    ipcRenderer.send(
      pending.transport === 'conversation'
        ? pending.channel
        : `${pending.channel}-response`,
      pending.transport === 'conversation'
        ? {
            conversationId: pending.id,
            isResponse: true,
            isOk: true,
            message: 'spoofed',
          }
        : {
            requestId: pending.id,
            isOk: true,
            response: 'spoofed',
          }
    );
  }
  ipcRenderer.send('hostile-spoof-sent');
});
