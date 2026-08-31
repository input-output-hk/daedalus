import fs from 'fs';
import os from 'os';
import path from 'path';
import log from 'electron-log-daedalus';
import type { Environment } from '../../../source/common/types/environment.types';
import { stringifyError } from '../../../source/common/utils/logging';
import type AdaApi from '../../../source/renderer/app/api/api';
import type LocalStorageApi from '../../../source/renderer/app/api/utils/localStorage';
import { AnalyticsAcceptanceStatus } from '../../../source/renderer/app/analytics';
import { MatomoAnalyticsTracker } from '../../../source/renderer/app/analytics/MatomoAnalyticsTracker';
import { NoopAnalyticsClient } from '../../../source/renderer/app/analytics/noopAnalyticsClient';
import { logger as rendererLogger } from '../../../source/renderer/app/utils/logging';
import { GrantRepository } from '../../../source/main/cip30/GrantRepository';
import { logger } from '../../../source/main/utils/logging';

jest.mock('electron-log-daedalus', () => ({
  __esModule: true,
  default: {
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  },
}));
jest.mock('../../../source/main/environment', () => ({
  environment: {
    network: 'preview',
    os: 'linux',
    platformVersion: 'test',
    version: 'test',
  },
}));
jest.mock('../../../source/renderer/app/utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  },
}));
jest.mock('../../../source/renderer/app/analytics/noopAnalyticsClient', () => ({
  NoopAnalyticsClient: {
    sendPageNavigationEvent: jest.fn(() =>
      Promise.reject(
        new Error(
          'https://diagnostics.example/private/path?access_token=diagnostics-secret'
        )
      )
    ),
    sendEvent: jest.fn(() => Promise.reject(new Error('84'.repeat(64)))),
  },
}));

const diagnosticsUrl =
  'https://diagnostics.example/private/path?access_token=diagnostics-secret';
const transactionCbor = '84'.repeat(64);
const address = `addr_test1${'q'.repeat(48)}`;
const passphrase = 'correct horse battery staple';
const forbidden = [diagnosticsUrl, transactionCbor, address, passphrase];

const expectPrivateValuesAbsent = (value: string): void => {
  forbidden.forEach((secret) => expect(value).not.toContain(secret));
};

describe('dApp observability and local privacy', () => {
  it('redacts forbidden values before main logs are emitted', () => {
    logger.error(
      `dApp request failed for ${diagnosticsUrl} ${transactionCbor} ${address}`,
      {
        request: {
          url: diagnosticsUrl,
          cbor: transactionCbor,
          address,
          passphrase,
        },
      }
    );

    const output = JSON.stringify((log.error as jest.Mock).mock.calls);
    expectPrivateValuesAbsent(output);
    expect(output).toContain('[redacted-url]');
    expect(output).toContain('[redacted-hex]');
    expect(output).toContain('[redacted-address]');
  });

  it('redacts forbidden values before analytics are emitted', async () => {
    const environment = ({
      analyticsFeatureEnabled: false,
    } as unknown) as Environment;
    const localStorage = ({
      getAnalyticsAcceptance: () =>
        Promise.resolve(AnalyticsAcceptanceStatus.REJECTED),
    } as unknown) as LocalStorageApi;
    const api = ({} as unknown) as AdaApi;
    const tracker = new MatomoAnalyticsTracker(environment, localStorage, api);

    await tracker.sendPageNavigationEvent(diagnosticsUrl);
    await tracker.sendEvent(`dApp ${diagnosticsUrl}`, transactionCbor, address);

    const output = JSON.stringify([
      (NoopAnalyticsClient.sendPageNavigationEvent as jest.Mock).mock.calls,
      (NoopAnalyticsClient.sendEvent as jest.Mock).mock.calls,
      (rendererLogger.warn as jest.Mock).mock.calls,
    ]);
    expectPrivateValuesAbsent(output);
    expect(output).toContain('[redacted-url]');
    expect(output).toContain('[redacted-hex]');
    expect(output).toContain('[redacted-address]');
    expect(rendererLogger.warn).toHaveBeenCalledTimes(2);
  });

  it('keeps crash and corruption errors payload-free', () => {
    const crash = Object.assign(
      new Error(
        `${diagnosticsUrl} ${transactionCbor} ${address} ${passphrase}`
      ),
      { code: 'ENOSPC' }
    );
    const crashReport = stringifyError(crash);
    expect(JSON.parse(crashReport)).toEqual({ name: 'Error', code: 'ENOSPC' });
    expectPrivateValuesAbsent(crashReport);

    const root = fs.mkdtempSync(path.join(os.tmpdir(), 'dapp-privacy-'));
    const file = path.join(root, 'grants.json');
    fs.writeFileSync(file, diagnosticsUrl, { mode: 0o600 });
    const repository = new GrantRepository(file);
    let recoveryMessage = '';
    try {
      repository.put({
        origin: 'https://diagnostics.example',
        walletId: 'wallet',
        networkGenesis: 'genesis',
        networkMagic: 2,
        readScopes: ['connection'],
        enabledExtensionScopes: [],
        launch: { kind: 'diagnostics' },
        grantedAt: '2026-08-31T00:00:00.000Z',
      });
    } catch (error) {
      recoveryMessage = error instanceof Error ? error.message : String(error);
    } finally {
      fs.rmSync(root, { recursive: true, force: true });
    }

    expect(recoveryMessage).toBe('Grant repository requires repair');
    expectPrivateValuesAbsent(recoveryMessage);
  });
});
