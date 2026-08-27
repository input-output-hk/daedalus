import { createHash } from 'crypto';
import type { DappCatalogEntry } from '../../common/types/dapp.types';
import {
  canonicalizeDappOrigin,
  canonicalizeDappResourceOrigin,
  parseDappUrl,
} from './urlPolicy';

export type { DappCatalogEntry } from '../../common/types/dapp.types';

export type ResolvedCatalogLaunch = Readonly<{
  catalogId: string;
  entryUrl: string;
  canonicalOrigin: string;
  allowedResourceOrigins: ReadonlySet<string>;
  windowTitle: string;
  catalogIdentity: string;
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

export const dappCatalogEntryIdentity = (entry: DappCatalogEntry): string =>
  createHash('sha256')
    .update(
      JSON.stringify({
        id: entry.id,
        nameMessageId: entry.nameMessageId,
        iconAsset: entry.iconAsset,
        entryUrlByNetworkGenesis: Object.entries(
          entry.entryUrlByNetworkGenesis
        ).sort(([left], [right]) => left.localeCompare(right)),
        canonicalOrigin: entry.canonicalOrigin,
        allowedResourceOrigins: [...entry.allowedResourceOrigins].sort(),
        supportedWalletKinds: [...entry.supportedWalletKinds].sort(),
        supportedExtensions: [...entry.supportedExtensions].sort(
          (left, right) => left - right
        ),
      })
    )
    .digest('hex');

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
    catalogIdentity: dappCatalogEntryIdentity(entry),
  });
};
