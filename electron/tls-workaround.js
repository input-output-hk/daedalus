import fs from 'fs';
import WebSocket from 'ws';

export const readCA = (path) => fs.readFileSync(path);

export const notify = (ca, succ, err) => {
  const ws = new WebSocket('wss://localhost:8090', { ca });
  ws.on('close', () => {
    setTimeout(() => {
      // reconnect
      ws.terminate();
      notify(ca, succ, err);
    }, 5000);
  });
  ws.on('error', err);
  ws.on('message', succ);
};
