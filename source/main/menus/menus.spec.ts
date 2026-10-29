/**
 * @jest-environment node
 */
import { osxMenu } from './osx';
import { winLinuxMenu } from './win-linux';
import { WalletSettingsStateEnum } from '../../common/ipc/api';

jest.mock('electron', () => ({ shell: { openExternal: jest.fn() } }));
jest.mock('../environment', () => ({
  environment: {
    isWindows: false,
    isLinux: true,
    isMacOS: false,
    isDev: false,
    isTest: false,
    isMainnet: true,
    isBlankScreenFixActive: false,
    version: '0.0.0',
  },
}));
jest.mock('../ipc/control-ui-parts', () => ({ showUiPartChannel: {} }));
jest.mock('../ipc/lib/currentWindowSender', () => ({
  consumeIpcResponse: jest.fn(),
  currentWindowSender: {},
}));

type MenuEntry = {
  label?: string;
  type?: string;
  accelerator?: string;
  submenu?: Array<MenuEntry>;
};

const actions = {
  toggleBlankScreenFix: jest.fn(),
  openToggleRTSFlagsModeDialog: jest.fn(),
  openAboutDialog: jest.fn(),
  openDaedalusDiagnosticsDialog: jest.fn(),
  openSettingsPage: jest.fn(),
  openWalletSettingsPage: jest.fn(),
};
const translation = (key: string) => key;
const app = { quit: jest.fn() } as never;
const window = {} as never;

const builders: Array<[string, () => Array<MenuEntry>]> = [
  [
    'osxMenu',
    () =>
      osxMenu(
        app,
        window,
        actions,
        {},
        'en-US',
        true,
        WalletSettingsStateEnum.hidden,
        false,
        translation
      ),
  ],
  [
    'winLinuxMenu',
    () =>
      winLinuxMenu(
        app,
        window,
        actions,
        {},
        'en-US',
        true,
        WalletSettingsStateEnum.hidden,
        false,
        translation
      ),
  ],
];

describe.each(builders)('%s', (_name, build) => {
  const submenu = () => build()[0].submenu as Array<MenuEntry>;

  it('offers no entry for the removed redemption flow', () => {
    const items = submenu();
    expect(items.map(({ label }) => label).join()).not.toMatch(/redeem/i);
    const accelerators = items.map(({ accelerator }) => accelerator);
    expect(accelerators).not.toContain('Command+T');
    expect(accelerators).not.toContain('Ctrl+T');
  });

  it('puts no separator beside another separator or at either end', () => {
    const types = submenu().map(({ type }) => type === 'separator');
    expect(types[0]).toBe(false);
    expect(types[types.length - 1]).toBe(false);
    expect(
      types.some((isSeparator, index) => isSeparator && types[index + 1])
    ).toBe(false);
  });

  it('keeps About as the first entry', () => {
    expect(submenu()[0].label).toBe('daedalus.about');
  });
});
