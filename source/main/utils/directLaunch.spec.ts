import {
  DIRECT_LAUNCH_MARKER_ENV,
  resolveWatchdogLaunch,
  shouldRelaunchViaWatchdog,
} from './directLaunch';

const existing =
  (...files: Array<string>) =>
  (file: string) =>
    files.includes(file);

describe('shouldRelaunchViaWatchdog', () => {
  it('relaunches a packaged build started without the watchdog environment', () => {
    expect(shouldRelaunchViaWatchdog({ isProduction: true, env: {} })).toBe(
      true
    );
  });

  it('does not relaunch outside a production build', () => {
    expect(shouldRelaunchViaWatchdog({ isProduction: false, env: {} })).toBe(
      false
    );
  });

  it('does not relaunch when the watchdog environment is present', () => {
    expect(
      shouldRelaunchViaWatchdog({
        isProduction: true,
        env: { DAEDALUS_CLUSTER: 'mainnet' },
      })
    ).toBe(false);
  });

  it('does not relaunch twice', () => {
    expect(
      shouldRelaunchViaWatchdog({
        isProduction: true,
        env: { [DIRECT_LAUNCH_MARKER_ENV]: '1' },
      })
    ).toBe(false);
  });
});

describe('resolveWatchdogLaunch', () => {
  it('resolves the watchdog and config in the Windows install directory', () => {
    const dir = 'C:\\Program Files\\Daedalus Mainnet';
    expect(
      resolveWatchdogLaunch({
        platform: 'win32',
        startPaths: [`${dir}\\Daedalus Mainnet.exe`],
        fileExists: existing(
          `${dir}\\cardano-watchdog.exe`,
          `${dir}\\daedalus-config.json`
        ),
      })
    ).toEqual({
      command: `${dir}\\cardano-watchdog.exe`,
      args: ['--config', `${dir}\\daedalus-config.json`],
      cwd: dir,
    });
  });

  it('returns null on Windows when the config is missing', () => {
    const dir = 'C:\\Daedalus';
    expect(
      resolveWatchdogLaunch({
        platform: 'win32',
        startPaths: [`${dir}\\Daedalus.exe`],
        fileExists: existing(`${dir}\\cardano-watchdog.exe`),
      })
    ).toBeNull();
  });

  it('returns null on Windows when the watchdog is missing', () => {
    const dir = 'C:\\Daedalus';
    expect(
      resolveWatchdogLaunch({
        platform: 'win32',
        startPaths: [`${dir}\\Daedalus.exe`],
        fileExists: existing(`${dir}\\daedalus-config.json`),
      })
    ).toBeNull();
  });

  it('resolves the launcher named after the bundle on macOS', () => {
    const macOs = '/Applications/Daedalus Mainnet.app/Contents/MacOS';
    expect(
      resolveWatchdogLaunch({
        platform: 'darwin',
        startPaths: [`${macOs}/Frontend`],
        fileExists: existing(`${macOs}/Daedalus Mainnet`),
      })
    ).toEqual({
      command: `${macOs}/Daedalus Mainnet`,
      args: [],
      cwd: macOs,
    });
  });

  it('returns null on macOS outside an app bundle', () => {
    expect(
      resolveWatchdogLaunch({
        platform: 'darwin',
        startPaths: ['/usr/local/bin/electron'],
        fileExists: () => true,
      })
    ).toBeNull();
  });

  it('finds bin/daedalus above the Electron binary on Linux', () => {
    const root = '/opt/daedalus/mainnet';
    expect(
      resolveWatchdogLaunch({
        platform: 'linux',
        startPaths: [`${root}/libexec/bundle-electron/lib/electron/electron`],
        fileExists: existing(`${root}/bin/daedalus`),
      })
    ).toEqual({ command: `${root}/bin/daedalus`, args: [], cwd: root });
  });

  it('falls back to the later start path on Linux', () => {
    const root = '/home/u/.daedalus/mainnet';
    expect(
      resolveWatchdogLaunch({
        platform: 'linux',
        startPaths: [
          '/nix/store/abc-electron/lib/electron/electron',
          `${root}/libexec/daedalus-js`,
        ],
        fileExists: existing(`${root}/bin/daedalus`),
      })?.command
    ).toBe(`${root}/bin/daedalus`);
  });

  it('returns null on Linux when no entry point exists', () => {
    expect(
      resolveWatchdogLaunch({
        platform: 'linux',
        startPaths: ['/opt/x/electron'],
        fileExists: () => false,
      })
    ).toBeNull();
  });

  it('returns null on an unsupported platform', () => {
    expect(
      resolveWatchdogLaunch({
        platform: 'freebsd',
        startPaths: ['/x/electron'],
        fileExists: () => true,
      })
    ).toBeNull();
  });
});
