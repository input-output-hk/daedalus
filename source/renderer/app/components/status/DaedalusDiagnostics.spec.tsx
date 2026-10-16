import { IntlProvider } from 'react-intl';
import translations from '../../i18n/locales/en-US.json';
import type { ChainDamage } from '../../../../common/types/watchdog.types';
import DaedalusDiagnostics from './DaedalusDiagnostics';

const damage: ChainDamage = {
  kind: 'ledger_point_missing',
  slot: 200102375,
  lastGoodSlot: null,
  detectedAt: '2026-10-16T12:00:00.000Z',
};

const props = (chainDamage: ChainDamage | null) =>
  ({
    systemInfo: {
      platform: 'Linux',
      platformVersion: '6.6',
      cpu: 'cpu',
      ram: '16 GB',
      availableDiskSpace: '500 GB',
      hasMetHardwareRequirements: true,
      isRTSFlagsModeEnabled: false,
    },
    coreInfo: {
      daedalusVersion: '11.5.0',
      daedalusBuildNumber: '1',
      daedalusProcessID: '2',
      daedalusMainProcessID: '3',
      isBlankScreenFixActive: false,
      cardanoNodeVersion: '11.1.2',
      cardanoNodePID: 4,
      cardanoNodeUptime: '1s',
      cardanoWalletVersion: 'v1',
      cardanoWalletPID: 5,
      cardanoWalletUptime: '1s',
      cardanoWalletRestartCount: 0,
      cardanoWalletApiPort: 1,
      cardanoNetwork: 'mainnet',
      daedalusStateDirectoryPath: '/state',
    },
    cardanoNodeState: 'running',
    isNodeResponding: true,
    isNodeSyncing: false,
    isNodeInSync: true,
    isNodeTimeCorrect: true,
    nodeConnectionError: null,
    isConnected: true,
    isSynced: true,
    syncPercentage: 100,
    localTimeDifference: 0,
    isSystemTimeCorrect: true,
    isSystemTimeIgnored: false,
    isCheckingSystemTime: false,
    localTip: null,
    networkTip: null,
    chainDamage,
  }) as any;

// The copied text is built from the instance's props and intl context alone.
const diagnosticsText = (chainDamage: ChainDamage | null) => {
  const { intl } = new IntlProvider({
    locale: 'en-US',
    messages: translations,
  } as any).getChildContext();
  const diagnostics = new DaedalusDiagnostics(props(chainDamage));
  diagnostics.context = { intl };
  return diagnostics.getDiagnosticsText();
};

describe('DaedalusDiagnostics copied text', () => {
  it('reports no damage in the Blockchain data row', () => {
    expect(diagnosticsText(null)).toContain(
      'Blockchain data: No damage detected'
    );
  });

  it('reports the last detection in the Blockchain data row', () => {
    const time = new Date(damage.detectedAt).toLocaleString('en-US');
    expect(diagnosticsText(damage)).toContain(
      `Blockchain data: Damage detected at slot 200102375 (${time})`
    );
  });
});
