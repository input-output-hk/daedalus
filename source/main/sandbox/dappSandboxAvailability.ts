import crypto from 'crypto';
import fs from 'fs';
import path from 'path';
import type EventEmitter from 'events';
import { BrowserWindow, session } from 'electron';
import type { Session, WebContents } from 'electron';

const MATRIX_REVISION = 'task-108-matrix-2026-08-18';
const CANARY_URL =
  'data:text/html;charset=utf-8,%3C!doctype%20html%3E%3Cmeta%20http-equiv%3D%22Content-Security-Policy%22%20content%3D%22default-src%20%27none%27%22%3E%3Ctitle%3EDaedalus%20sandbox%20canary%3C%2Ftitle%3E';
const CANARY_TIMEOUT_MS = 10_000;
const FORBIDDEN_SWITCHES = [
  '--disable-gpu-sandbox',
  '--disable-namespace-sandbox',
  '--disable-sandbox',
  '--disable-seccomp-filter-sandbox',
  '--disable-setuid-sandbox',
  '--in-process-gpu',
  '--no-sandbox',
  '--single-process',
] as const;
const STATUS_FIELDS: Record<string, true> = {
  Pid: true,
  NoNewPrivs: true,
  Seccomp: true,
  Seccomp_filters: true,
  CapEff: true,
};

export type DappSandboxUnavailableReason =
  | 'sandbox-bypass'
  | 'unsupported-package'
  | 'unsupported-host'
  | 'canary-failed'
  | 'cleanup-failed';

export type DappSandboxAvailability =
  | { readonly status: 'not-started' | 'checking' }
  | { readonly status: 'available' }
  | {
      readonly status: 'unavailable';
      readonly reason: DappSandboxUnavailableReason;
    };

export type DappSandboxAvailabilityOptions = {
  readonly isDevelopment: boolean;
  readonly cluster: string;
  readonly installRoot?: string;
};

type ProcEvidence = {
  pid: number;
  argv: string[];
  status: Record<string, string>;
  startTime: string;
  namespaces: Record<'pid' | 'user' | 'mnt', string>;
};

type PackageManifest = {
  schemaVersion?: unknown;
  packageFamily?: unknown;
  matrixRevision?: unknown;
  matrixRow?: unknown;
  distribution?: { id?: unknown; versionId?: unknown };
  supportState?: unknown;
  cluster?: unknown;
  helper?: { mode?: unknown; sha256?: unknown };
};

let availability: DappSandboxAvailability = Object.freeze({
  status: 'not-started',
});
let availabilityPromise: Promise<DappSandboxAvailability> | undefined;

export class DappSandboxUnavailableError extends Error {
  readonly reason: DappSandboxUnavailableReason | 'not-checked';

  constructor(reason: DappSandboxUnavailableReason | 'not-checked') {
    super('The dApp sandbox is unavailable');
    this.name = 'DappSandboxUnavailableError';
    this.reason = reason;
  }
}

const argumentIsForbidden = (argument: string): boolean =>
  FORBIDDEN_SWITCHES.some(
    (forbidden) =>
      argument === forbidden || argument.startsWith(`${forbidden}=`)
  );

export const hasSandboxBypass = (
  argv: readonly string[],
  environment: Readonly<Record<string, string | undefined>>
): boolean =>
  argv.some(argumentIsForbidden) ||
  Object.prototype.hasOwnProperty.call(environment, 'ELECTRON_DISABLE_SANDBOX');

const parseStatus = (statusText: string): Record<string, string> => {
  const result: Record<string, string> = {};
  statusText.split('\n').forEach((line) => {
    const separator = line.indexOf(':');
    if (separator === -1) return;
    const key = line.slice(0, separator);
    if (STATUS_FIELDS[key]) result[key] = line.slice(separator + 1).trim();
  });
  return result;
};

const parseProcessStartTime = (statText: string): string => {
  const commandEnd = statText.lastIndexOf(')');
  if (commandEnd === -1) throw new Error('invalid process stat');
  const startTime = statText
    .slice(commandEnd + 2)
    .trim()
    .split(/\s+/)[19];
  if (!/^\d+$/.test(startTime || '')) throw new Error('invalid process start');
  return startTime;
};

const readProcEvidence = (pid: number): ProcEvidence => {
  const procRoot = `/proc/${pid}`;
  return {
    pid,
    argv: fs
      .readFileSync(`${procRoot}/cmdline`)
      .toString('utf8')
      .split('\0')
      .filter(Boolean),
    status: parseStatus(fs.readFileSync(`${procRoot}/status`, 'utf8')),
    startTime: parseProcessStartTime(
      fs.readFileSync(`${procRoot}/stat`, 'utf8')
    ),
    namespaces: {
      pid: fs.readlinkSync(`${procRoot}/ns/pid`),
      user: fs.readlinkSync(`${procRoot}/ns/user`),
      mnt: fs.readlinkSync(`${procRoot}/ns/mnt`),
    },
  };
};

export const validateRendererEvidence = (
  mainEvidence: ProcEvidence,
  rendererEvidence: ProcEvidence
): boolean => {
  const chromiumType = rendererEvidence.argv.find((argument) =>
    argument.startsWith('--type=')
  );
  const filterCount = rendererEvidence.status.Seccomp_filters;
  return (
    !mainEvidence.argv.some(argumentIsForbidden) &&
    !rendererEvidence.argv.some(argumentIsForbidden) &&
    (chromiumType === '--type=renderer' || chromiumType === '--type=zygote') &&
    rendererEvidence.status.Pid === String(rendererEvidence.pid) &&
    rendererEvidence.status.NoNewPrivs === '1' &&
    rendererEvidence.status.Seccomp === '2' &&
    (filterCount === undefined || Number(filterCount) > 0) &&
    /^0+$/.test(rendererEvidence.status.CapEff || '') &&
    rendererEvidence.namespaces.pid !== mainEvidence.namespaces.pid
  );
};

const parseOsRelease = (): { id: string; versionId: string } | null => {
  try {
    const values: Record<string, string> = {};
    fs.readFileSync('/etc/os-release', 'utf8')
      .split('\n')
      .filter((line) => /^[A-Z_]+=/.test(line))
      .forEach((line) => {
        const separator = line.indexOf('=');
        values[line.slice(0, separator)] = line
          .slice(separator + 1)
          .replace(/^['"]|['"]$/g, '')
          .toLowerCase();
      });
    return values.ID && values.VERSION_ID
      ? { id: values.ID, versionId: values.VERSION_ID }
      : null;
  } catch {
    return null;
  }
};

const supportedHost = (
  host: { id: string; versionId: string },
  packageFamily: unknown
): string | null => {
  const key = `${host.id}-${host.versionId}`;
  const rows: Record<string, { row: string; packageFamily: string }> = {
    'ubuntu-24.04': { row: 'ubuntu-24.04', packageFamily: 'deb' },
    'ubuntu-26.04': { row: 'ubuntu-26.04', packageFamily: 'deb' },
    'debian-12': { row: 'debian-12', packageFamily: 'deb' },
    'debian-13': { row: 'debian-13', packageFamily: 'deb' },
    'fedora-43': { row: 'fedora-43', packageFamily: 'rpm' },
  };
  const match = rows[key];
  return match && match.packageFamily === packageFamily ? match.row : null;
};

const sha256 = (filePath: string): string =>
  crypto.createHash('sha256').update(fs.readFileSync(filePath)).digest('hex');

const validateProductionPackage = (
  cluster: string,
  configuredInstallRoot?: string
): DappSandboxUnavailableReason | null => {
  const expectedRoot = `/opt/daedalus/${cluster}`;
  const installRoot = configuredInstallRoot || process.env.ENTRYPOINT_DIR;
  if (!installRoot || path.resolve(installRoot) !== expectedRoot)
    return 'unsupported-package';

  try {
    if (fs.realpathSync(installRoot) !== expectedRoot)
      return 'unsupported-package';
    const manifestPath = path.join(
      expectedRoot,
      'share/daedalus-sandbox-identity.json'
    );
    const manifestStat = fs.lstatSync(manifestPath);
    if (
      !manifestStat.isFile() ||
      manifestStat.isSymbolicLink() ||
      manifestStat.uid !== 0 ||
      manifestStat.gid !== 0 ||
      (manifestStat.mode & 0o7777) !== 0o644
    )
      return 'unsupported-package';

    const manifest = JSON.parse(
      fs.readFileSync(manifestPath, 'utf8')
    ) as PackageManifest;
    const host = parseOsRelease();
    if (!host) return 'unsupported-host';
    const matrixRow = supportedHost(host, manifest.packageFamily);
    if (!matrixRow) return 'unsupported-host';
    if (
      manifest.schemaVersion !== 2 ||
      manifest.matrixRevision !== MATRIX_REVISION ||
      manifest.matrixRow !== matrixRow ||
      manifest.supportState !== 'supported' ||
      manifest.cluster !== cluster ||
      manifest.distribution?.id !== host.id ||
      manifest.distribution?.versionId !== host.versionId
    )
      return 'unsupported-package';

    const electronPath = path.join(
      expectedRoot,
      'libexec/bundle-electron/lib/electron/electron'
    );
    const helperPath = path.join(
      expectedRoot,
      'libexec/bundle-electron/lib/electron/chrome-sandbox'
    );
    const electronStat = fs.lstatSync(electronPath);
    const helperStat = fs.lstatSync(helperPath);
    const helperMode = Number.parseInt(String(manifest.helper?.mode), 8);
    if (
      fs.realpathSync(process.execPath) !== electronPath ||
      !electronStat.isFile() ||
      electronStat.isSymbolicLink() ||
      electronStat.uid !== 0 ||
      electronStat.gid !== 0 ||
      !helperStat.isFile() ||
      helperStat.isSymbolicLink() ||
      helperStat.uid !== 0 ||
      helperStat.gid !== 0 ||
      ![0o755, 0o4755].includes(helperMode) ||
      (helperStat.mode & 0o7777) !== helperMode ||
      !/^[a-f0-9]{64}$/.test(String(manifest.helper?.sha256)) ||
      sha256(helperPath) !== manifest.helper?.sha256
    )
      return 'unsupported-package';
  } catch {
    return 'unsupported-package';
  }
  return null;
};

const waitForLifecycleFailure = (
  webContents: WebContents,
  window: BrowserWindow
): { promise: Promise<never>; dispose: () => void } => {
  const listeners: Array<{
    emitter: EventEmitter;
    event: string;
    listener: () => void;
  }> = [];
  const promise = new Promise<never>((_resolve, reject) => {
    const rejectFailure = () => reject(new Error('canary lifecycle failure'));
    const add = (
      emitter: EventEmitter,
      event: string,
      listener: () => void = rejectFailure
    ) => {
      emitter.once(event, listener);
      listeners.push({ emitter, event, listener });
    };
    add(webContents, 'render-process-gone');
    add(webContents, 'unresponsive');
    add(webContents, 'did-fail-load');
    add(window, 'closed');
  });
  return {
    promise,
    dispose: () =>
      listeners.forEach(({ emitter, event, listener }) =>
        emitter.removeListener(event, listener)
      ),
  };
};

const clearCanarySession = async (canarySession: Session): Promise<void> => {
  await canarySession.clearStorageData();
  await canarySession.clearCache();
  await canarySession.clearAuthCache();
  await canarySession.clearHostResolverCache();
  await canarySession.closeAllConnections();
};

const runCanary = async (): Promise<DappSandboxUnavailableReason | null> => {
  const partition = `daedalus-dapp-sandbox-canary-${crypto
    .randomBytes(16)
    .toString('hex')}`;
  const canarySession = session.fromPartition(partition, { cache: false });
  let canaryWindow: BrowserWindow | undefined;
  let disposeLifecycle = () => undefined;
  let cleanupFailed = false;
  let canaryFailed = false;

  try {
    if (canarySession.isPersistent() || canarySession.getStoragePath() !== null)
      throw new Error('persistent canary session');

    canaryWindow = new BrowserWindow({
      show: false,
      frame: false,
      fullscreenable: false,
      autoHideMenuBar: true,
      webPreferences: {
        session: canarySession,
        nodeIntegration: false,
        nodeIntegrationInWorker: false,
        nodeIntegrationInSubFrames: false,
        contextIsolation: true,
        sandbox: true,
        webSecurity: true,
        allowRunningInsecureContent: false,
        webviewTag: false,
        devTools: false,
        plugins: false,
        spellcheck: false,
        enableWebSQL: false,
        navigateOnDragDrop: false,
        disableDialogs: true,
        autoplayPolicy: 'document-user-activation-required',
      },
    });
    canaryWindow.webContents.setWindowOpenHandler(() => ({ action: 'deny' }));
    canaryWindow.webContents.on('will-navigate', (event, url) => {
      if (url !== CANARY_URL) event.preventDefault();
    });
    const lifecycle = waitForLifecycleFailure(
      canaryWindow.webContents,
      canaryWindow
    );
    disposeLifecycle = lifecycle.dispose;
    let timeout: number | undefined;
    const timedOut = new Promise<never>((_resolve, reject) => {
      timeout = setTimeout(
        () => reject(new Error('canary timeout')),
        CANARY_TIMEOUT_MS
      );
    });
    try {
      await Promise.race([
        canaryWindow.loadURL(CANARY_URL),
        lifecycle.promise,
        timedOut,
      ]);
    } finally {
      clearTimeout(timeout);
    }

    const rendererPid = canaryWindow.webContents.getOSProcessId();
    if (!Number.isSafeInteger(rendererPid) || rendererPid <= 1)
      throw new Error('invalid renderer pid');
    const mainEvidence = readProcEvidence(process.pid);
    const rendererEvidence = readProcEvidence(rendererPid);
    if (
      mainEvidence.status.Pid !== String(process.pid) ||
      !validateRendererEvidence(mainEvidence, rendererEvidence)
    )
      throw new Error('invalid sandbox evidence');

    await new Promise<void>((resolve) => {
      setImmediate(resolve);
    });
    if (
      canaryWindow.isDestroyed() ||
      canaryWindow.webContents.isDestroyed() ||
      canaryWindow.webContents.getOSProcessId() !== rendererPid
    )
      throw new Error('renderer changed');
    const finalEvidence = readProcEvidence(rendererPid);
    if (
      finalEvidence.status.Pid !== rendererEvidence.status.Pid ||
      finalEvidence.startTime !== rendererEvidence.startTime
    )
      throw new Error('renderer instance changed');
  } catch {
    canaryFailed = true;
  } finally {
    disposeLifecycle();
    try {
      if (canaryWindow && !canaryWindow.isDestroyed()) canaryWindow.destroy();
      await clearCanarySession(canarySession);
    } catch {
      cleanupFailed = true;
    }
  }

  if (cleanupFailed) return 'cleanup-failed';
  return canaryFailed ? 'canary-failed' : null;
};

const checkAvailability = async (
  options: DappSandboxAvailabilityOptions
): Promise<DappSandboxAvailability> => {
  if (process.platform !== 'linux')
    return { status: 'unavailable', reason: 'unsupported-host' };
  let procArgv: string[] = [];
  try {
    procArgv = readProcEvidence(process.pid).argv;
  } catch {
    return { status: 'unavailable', reason: 'canary-failed' };
  }
  if (hasSandboxBypass([...process.argv, ...procArgv], process.env))
    return { status: 'unavailable', reason: 'sandbox-bypass' };

  if (!options.isDevelopment) {
    const packageFailure = validateProductionPackage(
      options.cluster,
      options.installRoot
    );
    if (packageFailure)
      return { status: 'unavailable', reason: packageFailure };
  }

  const canaryFailure = await runCanary();
  return canaryFailure
    ? { status: 'unavailable', reason: canaryFailure }
    : { status: 'available' };
};

export const startDappSandboxAvailabilityCheck = (
  options: DappSandboxAvailabilityOptions
): Promise<DappSandboxAvailability> => {
  if (availabilityPromise) return availabilityPromise;
  availability = Object.freeze({ status: 'checking' });
  availabilityPromise = checkAvailability(options)
    .catch(
      (): DappSandboxAvailability => ({
        status: 'unavailable',
        reason: 'canary-failed',
      })
    )
    .then((result) => {
      availability = Object.freeze(result);
      return availability;
    });
  return availabilityPromise;
};

export const getDappSandboxAvailability = (): DappSandboxAvailability =>
  availability;

export const requireDappSandboxAvailable = async (): Promise<void> => {
  const result = availabilityPromise
    ? await availabilityPromise
    : getDappSandboxAvailability();
  if (result.status !== 'available') {
    throw new DappSandboxUnavailableError(
      result.status === 'unavailable' ? result.reason : 'not-checked'
    );
  }
};
