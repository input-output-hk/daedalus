export type ParsedDappUrl = Readonly<{
  href: string;
  origin: string;
}>;

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

export const canonicalizeDappOrigin = (value: string): string => {
  const url = parseSecureUrl(value, ['https:']);
  if (url.pathname !== '/' || url.search !== '' || url.hash !== '')
    throw new Error('Invalid dApp origin');
  return url.origin;
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
