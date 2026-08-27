import dns from 'dns';
import { once } from 'events';
import http from 'http';
import net from 'net';
import ipaddr from 'ipaddr.js';
import type { Session } from 'electron';

const PROXY_HOST = '127.0.0.1';

type Destination = Readonly<{
  host: string;
  port: number;
}>;

export const isPublicGuestIp = (value: string): boolean => {
  try {
    const address = ipaddr.parse(value);
    if (address instanceof ipaddr.IPv6 && address.isIPv4MappedAddress())
      return address.toIPv4Address().range() === 'unicast';
    return address.range() === 'unicast';
  } catch {
    return false;
  }
};

const destinationKey = ({ host, port }: Destination): string =>
  `${host.toLowerCase()}:${port}`;

const parseDestination = (
  authority: string,
  defaultPort = 443
): Destination | undefined => {
  if (
    typeof authority !== 'string' ||
    authority === '' ||
    /[/\\?#@]/u.test(authority)
  )
    return undefined;

  try {
    const url = new URL(`https://${authority}`);
    const host = url.hostname.replace(/^\[|\]$/gu, '');
    const port = Number(url.port || defaultPort);
    if (!host || !Number.isInteger(port) || port < 1 || port > 65535)
      return undefined;
    return { host, port };
  } catch {
    return undefined;
  }
};

const allowedDestinations = (
  allowedResourceOrigins: ReadonlySet<string>
): ReadonlySet<string> =>
  new Set(
    [...allowedResourceOrigins].map((origin) => {
      const url = new URL(origin);
      return destinationKey({
        host: url.hostname.replace(/^\[|\]$/gu, ''),
        port: Number(url.port || '443'),
      });
    })
  );

export class DappEgressPolicy {
  private readonly server: http.Server;

  private readonly allowed?: ReadonlySet<string>;

  private readonly allowHttpLoopback: boolean;

  private readonly sockets = new Set<net.Socket>();

  private closed = false;

  private constructor(
    server: http.Server,
    allowedResourceOrigins: ReadonlySet<string> | undefined,
    allowHttpLoopback: boolean
  ) {
    this.server = server;
    this.allowed = allowedResourceOrigins
      ? allowedDestinations(allowedResourceOrigins)
      : undefined;
    this.allowHttpLoopback = allowHttpLoopback;
    server.on('connect', (request, socket, head) => {
      this.handleConnect(request.url, socket as net.Socket, head).catch(() => {
        socket.destroy();
      });
    });
    server.on('request', (request, response) => {
      this.handleHttpRequest(request, response).catch(() => {
        if (!response.headersSent) response.writeHead(403);
        response.end();
      });
    });
  }

  static async install(
    guestSession: Session,
    allowedResourceOrigins?: ReadonlySet<string>,
    allowHttpLoopback = false
  ): Promise<DappEgressPolicy> {
    const server = http.createServer();
    const policy = new DappEgressPolicy(
      server,
      allowedResourceOrigins,
      allowHttpLoopback
    );

    try {
      server.listen(0, PROXY_HOST);
      await once(server, 'listening');
      const address = server.address();
      if (!address || typeof address === 'string')
        throw new Error('DApp egress proxy address unavailable');
      const proxy = `${PROXY_HOST}:${address.port}`;
      await guestSession.setProxy({
        mode: 'fixed_servers',
        proxyRules: `http=${proxy};https=${proxy}`,
        proxyBypassRules: '<-loopback>',
      });
      return policy;
    } catch {
      await policy.close();
      throw new Error('DApp egress policy unavailable');
    }
  }

  async close(): Promise<void> {
    if (this.closed) return;
    this.closed = true;
    this.sockets.forEach((socket) => socket.destroy());
    this.sockets.clear();
    if (this.server.listening) {
      this.server.close();
      await once(this.server, 'close');
    }
  }

  private async handleConnect(
    authority: string | undefined,
    client: net.Socket,
    head: Buffer
  ): Promise<void> {
    const destination = authority && parseDestination(authority);
    if (
      this.closed ||
      !destination ||
      (this.allowed && !this.allowed.has(destinationKey(destination)))
    ) {
      client.end('HTTP/1.1 403 Forbidden\r\n\r\n');
      return;
    }

    this.sockets.add(client);
    client.once('close', () => this.sockets.delete(client));
    const target = await this.connect(destination, false);
    if (this.closed) {
      target.destroy();
      client.destroy();
      return;
    }

    client.write('HTTP/1.1 200 Connection Established\r\n\r\n');
    if (head.length) target.write(head);
    client.pipe(target);
    target.pipe(client);
    const destroyPair = () => {
      client.destroy();
      target.destroy();
    };
    client.once('error', destroyPair);
    target.once('error', destroyPair);
  }

  private async handleHttpRequest(
    request: http.IncomingMessage,
    response: http.ServerResponse
  ): Promise<void> {
    if (!this.allowHttpLoopback || !request.url) throw new Error('Forbidden');
    const url = new URL(request.url);
    const destination = parseDestination(url.host, 80);
    if (
      url.protocol !== 'http:' ||
      url.username !== '' ||
      url.password !== '' ||
      !destination
    )
      throw new Error('Forbidden');

    const target = await this.connect(destination, true);
    const headers = { ...request.headers };
    delete headers['proxy-authorization'];
    delete headers['proxy-connection'];
    const upstream = http.request({
      hostname: destination.host,
      port: destination.port,
      method: request.method,
      path: `${url.pathname}${url.search}`,
      headers,
      agent: false,
      createConnection: () => target,
    });
    upstream.once('response', (upstreamResponse) => {
      response.writeHead(
        upstreamResponse.statusCode || 502,
        upstreamResponse.headers
      );
      upstreamResponse.pipe(response);
    });
    upstream.once('error', () => response.destroy());
    request.pipe(upstream);
  }

  private async connect(
    { host, port }: Destination,
    requireLoopback = false
  ): Promise<net.Socket> {
    const addresses = net.isIP(host)
      ? [{ address: host, family: net.isIPv6(host) ? 6 : 4 }]
      : await dns.promises.lookup(host, { all: true, verbatim: true });
    const candidates = addresses.filter(({ address }) => {
      if (requireLoopback)
        try {
          return ipaddr.process(address).range() === 'loopback';
        } catch {
          return false;
        }
      return isPublicGuestIp(address);
    });

    for (const candidate of candidates) {
      try {
        return await this.connectAddress(
          candidate.address,
          candidate.family,
          port
        );
      } catch {
        // Try only another address from this already validated DNS answer.
      }
    }
    throw new Error('DApp destination unavailable');
  }

  private async connectAddress(
    host: string,
    family: number,
    port: number
  ): Promise<net.Socket> {
    if (this.closed) throw new Error('DApp egress policy closed');
    const socket = net.createConnection({ host, family, port });
    this.sockets.add(socket);
    try {
      await once(socket, 'connect');
      socket.once('close', () => this.sockets.delete(socket));
      return socket;
    } catch (error) {
      this.sockets.delete(socket);
      socket.destroy();
      throw error;
    }
  }
}
