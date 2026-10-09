import path from 'path';
import fs from 'fs';
import { spawn } from 'child_process';

// Set on the watchdog we start so that it reaches Electron's environment. When
// Electron is started directly again with this present, no further relaunch is
// attempted.
export const DIRECT_LAUNCH_MARKER_ENV = 'DAEDALUS_DIRECT_LAUNCH';
const MAX_ANCESTOR_DEPTH = 8;
export type WatchdogLaunch = {
  command: string;
  args: Array<string>;
  cwd: string;
};
type ResolveOptions = {
  platform: string;
  // Locations to start the search from, most specific first
  startPaths: Array<string>;
  fileExists?: (filePath: string) => boolean;
};
type ShouldRelaunchOptions = {
  isProduction: boolean;
  env: Record<string, string | undefined>;
};

const defaultFileExists = (filePath: string): boolean => {
  try {
    return fs.statSync(filePath).isFile();
  } catch (e) {
    return false;
  }
};

/**
 * A relaunch is only attempted for a packaged build that was started without
 * the watchdog's environment and has not already been relaunched once.
 */
export const shouldRelaunchViaWatchdog = ({
  isProduction,
  env,
}: ShouldRelaunchOptions): boolean =>
  isProduction && !env.DAEDALUS_CLUSTER && !env[DIRECT_LAUNCH_MARKER_ENV];

const ancestors = (startPath: string): Array<string> => {
  const result: Array<string> = [];
  let current = path.posix.dirname(startPath);
  for (let i = 0; i < MAX_ANCESTOR_DEPTH; i++) {
    result.push(current);
    const parent = path.posix.dirname(current);
    if (parent === current) break;
    current = parent;
  }
  return result;
};

/**
 * Finds the platform's watchdog entry point, the same one the installer's
 * shortcuts or launcher scripts use. Returns null when it cannot be found.
 */
export const resolveWatchdogLaunch = ({
  platform,
  startPaths,
  fileExists = defaultFileExists,
}: ResolveOptions): WatchdogLaunch | null => {
  if (platform === 'win32') {
    // Electron sits next to the watchdog and its config in the install directory
    const dir = path.win32.dirname(startPaths[0] || '');
    const command = path.win32.join(dir, 'cardano-watchdog.exe');
    const config = path.win32.join(dir, 'daedalus-config.json');
    if (!fileExists(command) || !fileExists(config)) return null;
    return { command, args: ['--config', config], cwd: dir };
  }

  if (platform === 'darwin') {
    // <App>.app/Contents/MacOS/Frontend is Electron, and the launcher beside it
    // is named after the bundle
    const macOsDir = path.posix.dirname(startPaths[0] || '');
    const bundle = path.posix.dirname(path.posix.dirname(macOsDir));
    if (path.posix.basename(macOsDir) !== 'MacOS' || !bundle.endsWith('.app')) {
      return null;
    }
    const command = path.posix.join(
      macOsDir,
      path.posix.basename(bundle, '.app')
    );
    return fileExists(command) ? { command, args: [], cwd: macOsDir } : null;
  }

  if (platform === 'linux') {
    // <install root>/bin/daedalus is the entry point for every package format
    for (const startPath of startPaths) {
      for (const dir of ancestors(startPath)) {
        const command = path.posix.join(dir, 'bin', 'daedalus');
        if (fileExists(command)) return { command, args: [], cwd: dir };
      }
    }
  }

  return null;
};

/**
 * Starts the watchdog detached and returns true when it was spawned.
 */
export const spawnWatchdog = (
  launch: WatchdogLaunch,
  env: NodeJS.ProcessEnv = process.env
): boolean => {
  try {
    const childEnv: NodeJS.ProcessEnv = { ...env };
    childEnv[DIRECT_LAUNCH_MARKER_ENV] = '1';
    const child = spawn(launch.command, launch.args, {
      cwd: launch.cwd,
      detached: true,
      stdio: 'ignore',
      windowsHide: false,
      env: childEnv,
    });
    child.on('error', () => {});
    child.unref();
    return true;
  } catch (e) {
    return false;
  }
};
