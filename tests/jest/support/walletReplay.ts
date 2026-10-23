import * as http from 'http';
import type * as https from 'https';
import path from 'path';
import { TextEncoder } from 'util';
import type { AddressInfo, Socket } from 'net';
import type { RequestConfig } from '../../../source/renderer/app/api/common/types';

/**
 * One wallet API exchange recorded on a local testnet by the retired-DRep
 * withdrawal harness (tests/cluster/drep-withdrawal). The response body is the
 * wallet's bytes exactly as they arrived.
 */
export type RecordedExchange = {
  backend: 'upstream' | 'cip30';
  wallet: string;
  recorded: string;
  chain: { epoch: number; slot: number } | null;
  request: { method: string; path: string; body: unknown };
  response: { status: number; body: string };
};

export const loadRecordedExchange = (
  backend: RecordedExchange['backend'],
  name: string
): RecordedExchange =>
  require(
    path.join(__dirname, '../../mocks/retired-drep', backend, `${name}.json`)
  );

export type ReplayRoute = {
  method: string;
  path: RegExp;
  // 'hold' accepts the request and never answers, as a stalled wallet does.
  reply: RecordedExchange | 'hold';
};

export type WalletReplay = {
  config: RequestConfig;
  received: Array<{ method: string; path: string }>;
  close: () => Promise<void>;
};

/**
 * Provides the globals Daedalus's request layer expects from the renderer:
 * Node's https module, from which request.ts builds its agent when it loads,
 * and TextEncoder, which measures JSON request bodies and which jsdom lacks.
 * A spec calls this before it loads the API.
 */
export const provideRequestGlobals = (): void => {
  // The module object itself, so that startWalletReplay can redirect it.
  global.https = jest.requireActual('https');
  if (typeof global.TextEncoder === 'undefined') {
    global.TextEncoder = TextEncoder as typeof global.TextEncoder;
  }
};

/**
 * Serves recorded wallet responses on a loopback port and points Daedalus's
 * request layer at it. Only TLS is replaced: request.ts, its response parser
 * and the response bytes are the ones Daedalus handles in production.
 */
export const startWalletReplay = async (
  routes: Array<ReplayRoute>
): Promise<WalletReplay> => {
  const received: WalletReplay['received'] = [];
  const held: Array<http.ServerResponse> = [];
  const server = http.createServer((request, response) => {
    request.resume();
    request.on('end', () => {
      received.push({ method: request.method, path: request.url });
      const route = routes.find(
        (candidate) =>
          candidate.method === request.method &&
          candidate.path.test(request.url)
      );
      if (!route) {
        response.writeHead(501, { 'Content-Type': 'application/json' });
        response.end(
          JSON.stringify({
            code: 'no_recorded_exchange',
            message: `${request.method} ${request.url}`,
          })
        );
        return;
      }
      if (route.reply === 'hold') {
        held.push(response);
        return;
      }
      response.writeHead(route.reply.response.status, {
        'Content-Type': 'application/json',
      });
      response.end(route.reply.response.body);
    });
  });
  // Held requests and connections still in flight are cut when the replay
  // closes, so a spec never waits on a response the replay will not send.
  const sockets = new Set<Socket>();
  server.on('connection', (socket) => {
    sockets.add(socket);
    socket.on('close', () => sockets.delete(socket));
  });
  await new Promise<void>((resolve) => server.listen(0, '127.0.0.1', resolve));
  const spy = jest
    .spyOn(global.https as typeof https, 'request')
    .mockImplementation((options: https.RequestOptions | string | URL) => {
      if (typeof options === 'string' || options instanceof URL) {
        throw new Error('Expected wallet request options');
      }
      return http.request({ ...options, agent: false });
    });
  return {
    config: {
      hostname: '127.0.0.1',
      port: (server.address() as AddressInfo).port,
      ca: new Uint8Array(),
      cert: new Uint8Array(),
      key: new Uint8Array(),
    },
    received,
    close: async () => {
      spy.mockRestore();
      held.forEach((response) => response.destroy());
      sockets.forEach((socket) => socket.destroy());
      await new Promise<void>((resolve, reject) => {
        server.close((error) => (error ? reject(error) : resolve()));
      });
    },
  };
};
