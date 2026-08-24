import {
  canonicalizeDappOrigin,
  canonicalizeDappResourceOrigin,
  parseDappUrl,
} from './urlPolicy';

export type DappCatalogEntry = Readonly<{
  id: string;
  nameMessageId: string;
  iconAsset: string;
  entryUrlByNetworkGenesis: Readonly<Record<string, string>>;
  canonicalOrigin: string;
  allowedResourceOrigins: readonly string[];
  supportedWalletKinds: readonly string[];
  supportedExtensions: readonly number[];
}>;

export type ResolvedCatalogLaunch = Readonly<{
  catalogId: string;
  entryUrl: string;
  canonicalOrigin: string;
  allowedResourceOrigins: ReadonlySet<string>;
  windowTitle: string;
}>;

const localWindowTitle = (name: string): string => {
  if (
    typeof name !== 'string' ||
    name.trim() === '' ||
    Array.from(name).some((character) => {
      const code = character.charCodeAt(0);
      return code <= 31 || code === 127;
    })
  )
    throw new Error('Invalid local dApp title');
  return `${name.trim()} — Daedalus`;
};

export const resolveCatalogLaunch = (
  entry: DappCatalogEntry,
  networkGenesis: string,
  localName: string
): ResolvedCatalogLaunch => {
  if (!entry || typeof entry.id !== 'string' || entry.id === '')
    throw new Error('Invalid dApp catalog entry');

  const entryUrl = parseDappUrl(entry.entryUrlByNetworkGenesis[networkGenesis]);
  const canonicalOrigin = canonicalizeDappOrigin(entry.canonicalOrigin);
  if (entryUrl.origin !== canonicalOrigin)
    throw new Error('DApp catalog origin mismatch');

  const allowedResourceOrigins = new Set(
    entry.allowedResourceOrigins.map(canonicalizeDappResourceOrigin)
  );
  allowedResourceOrigins.add(canonicalOrigin);

  return Object.freeze({
    catalogId: entry.id,
    entryUrl: entryUrl.href,
    canonicalOrigin,
    allowedResourceOrigins,
    windowTitle: localWindowTitle(localName),
  });
};
