import https from 'https';
import { environment } from '../environment';
import { logger } from '../utils/logging';

export type MithrilNetworkConfig = {
  aggregatorEndpoint: string;
  genesisKeyUrl: string;
  ancillaryKeyUrl: string;
  genesisKey?: string;
  ancillaryKey?: string;
};

export const MITHRIL_NETWORK_CONFIG: Record<string, MithrilNetworkConfig> = {
  mainnet: {
    aggregatorEndpoint:
      'https://aggregator.release-mainnet.api.mithril.network/aggregator',
    genesisKeyUrl:
      'https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey',
    ancillaryKeyUrl:
      'https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/ancillary.vkey',
  },
  preprod: {
    aggregatorEndpoint:
      'https://aggregator.release-preprod.api.mithril.network/aggregator',
    genesisKeyUrl:
      'https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey',
    ancillaryKeyUrl:
      'https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/ancillary.vkey',
  },
  preview: {
    aggregatorEndpoint:
      'https://aggregator.pre-release-preview.api.mithril.network/aggregator',
    genesisKeyUrl:
      'https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey',
    ancillaryKeyUrl:
      'https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/ancillary.vkey',
  },
};

export function resolveNetworkConfig(): MithrilNetworkConfig {
  const network = String(environment.network);
  const config = MITHRIL_NETWORK_CONFIG[network];
  if (!config) {
    throw new Error(`Mithril not supported for network: ${network}`);
  }
  return config;
}

export function fetchText(url: string): Promise<string> {
  return new Promise((resolve, reject) => {
    const request = https.request(url, (response) => {
      const { statusCode } = response;
      if (!statusCode || statusCode < 200 || statusCode >= 300) {
        response.resume();
        reject(new Error(`Request failed with status ${statusCode}`));
        return;
      }

      let data = '';
      response.on('data', (chunk) => {
        data += chunk.toString();
      });
      response.on('end', () => resolve(data.trim()));
    });

    request.on('error', reject);
    request.end();
  });
}

export function normalizeVerificationKey(key: string): string {
  const trimmed = key.trim();
  if (/^[0-9a-fA-F]+$/.test(trimmed) && trimmed.length % 2 === 0) {
    return trimmed;
  }

  if (trimmed.startsWith('[')) {
    try {
      const bytes = JSON.parse(trimmed);
      if (Array.isArray(bytes)) {
        return bytes
          .map((value) => {
            const byte = Number(value);
            if (!Number.isFinite(byte) || byte < 0 || byte > 255) {
              throw new Error('Invalid byte value');
            }
            return byte.toString(16).padStart(2, '0');
          })
          .join('');
      }
    } catch (error) {
      logger.warn('MithrilBootstrapService: failed to parse verification key', {
        error,
      });
    }
  }

  return trimmed;
}

export async function resolveVerificationKeys(
  config: MithrilNetworkConfig
): Promise<void> {
  if (!config.genesisKey) {
    const raw = await fetchText(config.genesisKeyUrl);
    config.genesisKey = normalizeVerificationKey(raw);
  }
  if (!config.ancillaryKey) {
    const raw = await fetchText(config.ancillaryKeyUrl);
    config.ancillaryKey = normalizeVerificationKey(raw);
  }
}

export async function buildMithrilEnv(
  requireKeys: boolean
): Promise<NodeJS.ProcessEnv> {
  const config = resolveNetworkConfig();
  if (requireKeys) {
    await resolveVerificationKeys(config);
  }
  const env = {
    ...process.env,
    AGGREGATOR_ENDPOINT: config.aggregatorEndpoint,
  } as unknown as NodeJS.ProcessEnv;

  if (requireKeys && config.genesisKey && config.ancillaryKey) {
    env.GENESIS_VERIFICATION_KEY = config.genesisKey;
    env.ANCILLARY_VERIFICATION_KEY = config.ancillaryKey;
  } else {
    delete env.GENESIS_VERIFICATION_KEY;
    delete env.ANCILLARY_VERIFICATION_KEY;
  }

  return env;
}
