import { readFileSync } from 'fs';
import path from 'path';

const PACKAGE_CLUSTERS = [
  'mainnet',
  'mainnet-flight',
  'preprod',
  'preview',
] as const;
type DappPackageCluster = (typeof PACKAGE_CLUSTERS)[number];
type DappNetworkCluster = Exclude<DappPackageCluster, 'mainnet-flight'>;

export type DappRuntimeConfig = Readonly<{
  dappBrowserPolicy: unknown;
  dappSandboxPackageCluster: DappPackageCluster;
  dappNetwork: Readonly<{
    cluster: DappNetworkCluster;
    genesisFile: string;
    genesisHash: string;
  }>;
}>;

type RawDappRuntimeConfig = Partial<{
  dappBrowserPolicy: unknown;
  dappSandboxPackageCluster: unknown;
  dappNetwork: unknown;
}>;

export const readDappRuntimeConfig = (
  configPath: string | undefined
): DappRuntimeConfig => {
  try {
    if (!configPath || !path.isAbsolute(configPath))
      throw new Error('invalid path');
    const parsed: unknown = JSON.parse(readFileSync(configPath, 'utf8'));
    if (parsed === null || typeof parsed !== 'object' || Array.isArray(parsed))
      throw new Error('invalid config');

    const config = parsed as RawDappRuntimeConfig;
    const packageCluster = config.dappSandboxPackageCluster;
    const network = config.dappNetwork;
    if (
      typeof packageCluster !== 'string' ||
      !PACKAGE_CLUSTERS.includes(
        packageCluster as (typeof PACKAGE_CLUSTERS)[number]
      ) ||
      network === null ||
      typeof network !== 'object' ||
      Array.isArray(network)
    )
      throw new Error('invalid identity');
    const trustedPackageCluster = packageCluster as DappPackageCluster;
    const networkIdentity = network as Partial<{
      cluster: unknown;
      genesisFile: unknown;
      genesisHash: unknown;
    }>;

    const expectedCluster: DappNetworkCluster =
      trustedPackageCluster === 'mainnet-flight'
        ? 'mainnet'
        : trustedPackageCluster;
    if (
      networkIdentity.cluster !== expectedCluster ||
      networkIdentity.genesisFile !== 'genesis.json' ||
      typeof networkIdentity.genesisHash !== 'string' ||
      !/^[0-9a-f]{64}$/u.test(networkIdentity.genesisHash)
    )
      throw new Error('invalid network');

    return Object.freeze({
      dappBrowserPolicy: config.dappBrowserPolicy,
      dappSandboxPackageCluster: trustedPackageCluster,
      dappNetwork: Object.freeze({
        cluster: expectedCluster,
        genesisFile: path.resolve(path.dirname(configPath), 'genesis.json'),
        genesisHash: networkIdentity.genesisHash,
      }),
    });
  } catch {
    throw new Error('Invalid Daedalus configuration');
  }
};
