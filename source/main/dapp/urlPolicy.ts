import ipaddr from 'ipaddr.js';

export type ParsedDappUrl = Readonly<{
  href: string;
  origin: string;
}>;
export type DappUrlPolicy = Readonly<{
  allowHttpLoopback: boolean;
}>;

const isLoopbackHost = (hostname: string): boolean => {
  if (hostname.toLowerCase() === 'localhost') return true;
  try {
    return (
      ipaddr.process(hostname.replace(/^\[|\]$/gu, '')).range() === 'loopback'
    );
  } catch {
    return false;
  }
};

const parseSecureUrl = (value: string, protocols: readonly string[]): URL => {
  if (typeof value !== 'string') throw new Error('Invalid dApp URL');

  let url: URL;
  try {
    url = new URL(value);
  } catch {
    throw new Error('Invalid dApp URL');
  }

  if (
    !protocols.includes(url.protocol) ||
    url.username !== '' ||
    url.password !== '' ||
    url.hostname === '' ||
    url.origin === 'null'
  )
    throw new Error('Invalid dApp URL');

  return url;
};

export const parseDappUrl = (value: string): ParsedDappUrl => {
  const url = parseSecureUrl(value, ['https:']);
  return Object.freeze({ href: url.href, origin: url.origin });
};

export const canonicalizeDappOrigin = (
  value: string,
  policy: DappUrlPolicy = { allowHttpLoopback: false }
): string => {
  const url = parseSecureUrl(
    value,
    policy.allowHttpLoopback ? ['https:', 'http:'] : ['https:']
  );
  if (
    (url.protocol === 'http:' && !isLoopbackHost(url.hostname)) ||
    url.pathname !== '/' ||
    url.search !== '' ||
    url.hash !== ''
  )
    throw new Error('Invalid dApp origin');
  return url.origin;
};

export const parseDiagnosticsDappUrl = (
  value: string,
  policy: DappUrlPolicy
): ParsedDappUrl => {
  const url = parseSecureUrl(
    value,
    policy.allowHttpLoopback ? ['https:', 'http:'] : ['https:']
  );
  if (url.protocol === 'http:' && !isLoopbackHost(url.hostname))
    throw new Error('Invalid dApp URL');
  return Object.freeze({ href: url.href, origin: url.origin });
};

const resourceOrigin = (value: string): string => {
  const url = parseSecureUrl(value, ['https:', 'wss:']);
  return url.origin;
};

export const canonicalizeDappResourceOrigin = resourceOrigin;

export const isAllowedDappResourceUrl = (
  value: string,
  allowedOrigins: ReadonlySet<string>
): boolean => {
  try {
    return allowedOrigins.has(resourceOrigin(value));
  } catch {
    return false;
  }
};

export const isAllowedDiagnosticsResourceUrl = (
  value: string,
  policy: DappUrlPolicy
): boolean => {
  try {
    const url = parseSecureUrl(
      value,
      policy.allowHttpLoopback
        ? ['https:', 'wss:', 'http:']
        : ['https:', 'wss:']
    );
    return url.protocol !== 'http:' || isLoopbackHost(url.hostname);
  } catch {
    return false;
  }
};
