import crypto from 'crypto';
import fs from 'fs';
import os from 'os';
import path from 'path';
import type EventEmitter from 'events';
import { app, BrowserWindow, session } from 'electron';
import type { ProcessMetric, Session, WebContents } from 'electron';

const MATRIX_REVISION = 'task-108-matrix-2026-08-18';
const ARCH_MATRIX_REVISION = 'task-111-matrix-2026-09-02';
const NIXOS_MATRIX_REVISION = 'task-112-matrix-2026-09-11';
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
const WINDOWS_PROGRAM_FILES = 'C:\\Program Files';
const DAPP_PRODUCT_NAMES: Readonly<Record<string, string>> = {
  mainnet: 'Daedalus Mainnet',
  'mainnet-flight': 'Daedalus Flight',
  preprod: 'Daedalus Pre-Prod',
  preview: 'Daedalus Preview',
};
const STATUS_FIELDS: Record<string, true> = {
  Pid: true,
  NoNewPrivs: true,
  Seccomp: true,
  Seccomp_filters: true,
  CapEff: true,
  CapPrm: true,
  CapInh: true,
};
const STATUS_FIELD_KEYS = Object.keys(STATUS_FIELDS);
const NAMESPACE_KEYS = ['pid', 'user', 'mnt', 'net'] as const;
const SHARED_NAMESPACE_KEYS = ['pid', 'user', 'net'] as const;
const FILESYSTEM_FIELD_KEYS = [
  'device',
  'inode',
  'filesystemType',
  'isDirectory',
  'empty',
  'deadProcRoot',
  'chromiumFdinfoRoot',
  'cwdMatchesRoot',
] as const;

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

type FilesystemEvidence = Readonly<{
  device: string;
  inode: string;
  filesystemType: string;
  isDirectory: boolean;
  empty: boolean;
  deadProcRoot: boolean;
  chromiumFdinfoRoot: boolean;
  cwdMatchesRoot: boolean;
}>;

type ProcEvidence = {
  pid: number;
  argv: string[];
  status: Record<string, string>;
  startTime: string;
  namespaces: Record<'pid' | 'user' | 'mnt' | 'net', string>;
  uidMap: string;
  gidMap: string;
  filesystem?: FilesystemEvidence;
};

type HostIdentity = {
  id: string;
  versionId?: string;
  buildId?: string;
  kernelRelease?: string;
  imageId?: string;
  imageVersion?: string;
};

type PackageManifest = {
  schemaVersion?: unknown;
  packageFamily?: unknown;
  matrixRevision?: unknown;
  matrixRow?: unknown;
  distribution?: {
    id?: unknown;
    versionId?: unknown;
    buildId?: unknown;
    kernelRelease?: unknown;
    imageId?: unknown;
    imageVersion?: unknown;
  };
  supportState?: unknown;
  cluster?: unknown;
  policy?: { kind?: unknown };
  helper?: { mode?: unknown; sha256?: unknown };
  launch?: { electron?: unknown };
};

type PackageValidation = {
  failure: DappSandboxUnavailableReason | null;
  usernsOnly: boolean;
};

type DesktopPackageIdentity = {
  appName: string;
  appPath: string;
  architecture: string;
  executablePath: string;
  installRoot: string;
  isPackaged: boolean;
  configPath: string;
  resourcesPath: string;
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

const normalizeWindowsPath = (value: string): string =>
  path.win32.normalize(value).toLowerCase();

export const validateWindowsPackageIdentity = (
  identity: DesktopPackageIdentity,
  cluster: string
): boolean => {
  const productName = DAPP_PRODUCT_NAMES[cluster];
  if (!productName) return false;
  const expectedRoot = path.win32.join(WINDOWS_PROGRAM_FILES, productName);
  return (
    identity.isPackaged &&
    identity.architecture === 'x64' &&
    identity.appName === productName &&
    normalizeWindowsPath(identity.installRoot) ===
      normalizeWindowsPath(expectedRoot) &&
    normalizeWindowsPath(identity.executablePath) ===
      normalizeWindowsPath(
        path.win32.join(expectedRoot, `${productName}.exe`)
      ) &&
    normalizeWindowsPath(identity.resourcesPath) ===
      normalizeWindowsPath(path.win32.join(expectedRoot, 'resources')) &&
    normalizeWindowsPath(identity.appPath) ===
      normalizeWindowsPath(path.win32.join(expectedRoot, 'resources', 'app')) &&
    normalizeWindowsPath(identity.configPath) ===
      normalizeWindowsPath(
        path.win32.join(expectedRoot, 'daedalus-config.json')
      )
  );
};

export const validateDarwinPackageIdentity = (
  identity: DesktopPackageIdentity,
  cluster: string
): boolean => {
  const productName = DAPP_PRODUCT_NAMES[cluster];
  if (!productName) return false;
  const root = identity.installRoot;
  if (
    !path.posix.isAbsolute(root) ||
    path.posix.normalize(root) !== root ||
    path.posix.basename(root) !== `${productName}.app`
  )
    return false;
  const resourcesPath = path.posix.join(root, 'Contents/Resources');
  return (
    identity.isPackaged &&
    (identity.architecture === 'x64' || identity.architecture === 'arm64') &&
    identity.appName === productName &&
    identity.executablePath ===
      path.posix.join(root, 'Contents/MacOS/Frontend') &&
    identity.resourcesPath === resourcesPath &&
    identity.appPath === path.posix.join(resourcesPath, 'app') &&
    identity.configPath ===
      path.posix.join(resourcesPath, 'daedalus-config.json')
  );
};

const findSandboxedRendererMetric = (
  metrics: readonly ProcessMetric[],
  rendererPid: number,
  platform: 'darwin' | 'win32'
): ProcessMetric | undefined => {
  if (!Number.isSafeInteger(rendererPid) || rendererPid <= 1) return undefined;
  return metrics.find(
    (metric) =>
      metric.pid === rendererPid &&
      metric.type === 'Tab' &&
      metric.sandboxed === true &&
      typeof metric.creationTime === 'number' &&
      Number.isFinite(metric.creationTime) &&
      metric.creationTime > 0 &&
      (platform === 'darwin' ||
        metric.integrityLevel === 'low' ||
        metric.integrityLevel === 'untrusted')
  );
};

export const validateMetricRendererEvidence = (
  metrics: readonly ProcessMetric[],
  rendererPid: number,
  platform: 'darwin' | 'win32'
): boolean =>
  Boolean(findSandboxedRendererMetric(metrics, rendererPid, platform));

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

const readFilesystemEvidence = (
  pid: number
): FilesystemEvidence | undefined => {
  // O_PATH is Linux UAPI: metadata access must survive Chromium's dead helper.
  const directoryFlags = 0x200000 | fs.constants.O_DIRECTORY;
  const statfs = (
    fs as typeof fs & {
      statfsSync?: (
        filePath: string,
        options: { bigint: true }
      ) => { type: bigint };
    }
  ).statfsSync;
  if (!statfs) return undefined;
  let rootFd: number | undefined;
  let cwdFd: number | undefined;
  let evidence: FilesystemEvidence | undefined;
  try {
    const rootPath = `/proc/${pid}/root`;
    const rootLink = fs.readlinkSync(rootPath);
    rootFd = fs.openSync(rootPath, directoryFlags);
    const root = fs.fstatSync(rootFd, { bigint: true });
    const heldRoot = `/proc/self/fd/${rootFd}`;
    const filesystemType = statfs(heldRoot, { bigint: true }).type.toString();
    const chromiumFdinfoRoot =
      /^\/proc\/[1-9][0-9]*(\/task\/[1-9][0-9]*)?\/fdinfo( \(deleted\))?$/.test(
        rootLink
      );
    let empty = false;
    let deadProcRoot = false;
    try {
      empty = fs.readdirSync(heldRoot).length === 0;
    } catch (error) {
      if (
        (error as NodeJS.ErrnoException).code !== 'ESRCH' ||
        filesystemType !== '40864' ||
        !root.isDirectory() ||
        !chromiumFdinfoRoot
      )
        throw error;
      deadProcRoot = true;
    }
    cwdFd = fs.openSync(`/proc/${pid}/cwd`, directoryFlags);
    const cwd = fs.fstatSync(cwdFd, { bigint: true });
    if (fs.readlinkSync(rootPath) !== rootLink)
      throw new Error('process root changed');
    evidence = {
      device: root.dev.toString(),
      inode: root.ino.toString(),
      filesystemType,
      isDirectory: root.isDirectory(),
      empty,
      deadProcRoot,
      chromiumFdinfoRoot,
      cwdMatchesRoot:
        cwd.isDirectory() && cwd.dev === root.dev && cwd.ino === root.ino,
    };
  } catch {
    evidence = undefined;
  } finally {
    for (const descriptor of [cwdFd, rootFd]) {
      if (descriptor !== undefined) {
        try {
          fs.closeSync(descriptor);
        } catch {
          evidence = undefined;
        }
      }
    }
  }
  return evidence;
};

const sameStatus = (
  first: Record<string, string>,
  second: Record<string, string>
): boolean =>
  STATUS_FIELD_KEYS.every((field) => first[field] === second[field]);

const readProcEvidence = (pid: number): ProcEvidence => {
  const procRoot = `/proc/${pid}`;
  const status = parseStatus(fs.readFileSync(`${procRoot}/status`, 'utf8'));
  const startTime = parseProcessStartTime(
    fs.readFileSync(`${procRoot}/stat`, 'utf8')
  );
  const evidence: ProcEvidence = {
    pid,
    argv: fs
      .readFileSync(`${procRoot}/cmdline`)
      .toString('utf8')
      .split('\0')
      .filter(Boolean),
    status,
    startTime,
    namespaces: {
      pid: fs.readlinkSync(`${procRoot}/ns/pid`),
      user: fs.readlinkSync(`${procRoot}/ns/user`),
      mnt: fs.readlinkSync(`${procRoot}/ns/mnt`),
      net: fs.readlinkSync(`${procRoot}/ns/net`),
    },
    uidMap: fs.readFileSync(`${procRoot}/uid_map`, 'utf8'),
    gidMap: fs.readFileSync(`${procRoot}/gid_map`, 'utf8'),
    filesystem: readFilesystemEvidence(pid),
  };
  const finalStatus = parseStatus(
    fs.readFileSync(`${procRoot}/status`, 'utf8')
  );
  const finalStartTime = parseProcessStartTime(
    fs.readFileSync(`${procRoot}/stat`, 'utf8')
  );
  if (
    status.Pid !== String(pid) ||
    !sameStatus(status, finalStatus) ||
    startTime !== finalStartTime
  )
    throw new Error('process sample changed');
  return evidence;
};

const parseIdMap = (text: string): number[][] | undefined => {
  if (typeof text !== 'string' || !text.trim()) return undefined;
  const ranges: number[][] = [];
  for (const line of text.trim().split('\n')) {
    const fields = line.trim().split(/\s+/);
    if (fields.length !== 3 || fields.some((field) => !/^\d+$/.test(field)))
      return undefined;
    const range = fields.map(Number);
    const [inside, outside, count] = range;
    if (
      range.some(
        (value) => !Number.isSafeInteger(value) || value > 0xffffffff
      ) ||
      count === 0 ||
      inside + count > 0x100000000 ||
      outside + count > 0x100000000
    )
      return undefined;
    ranges.push(range);
  }
  return ranges;
};

const sameIdMap = (
  first: number[][] | undefined,
  second: number[][] | undefined
): boolean => {
  if (!first || !second) return first === second;
  return (
    first.length === second.length &&
    first.every(
      (range, index) =>
        range[0] === second[index][0] &&
        range[1] === second[index][1] &&
        range[2] === second[index][2]
    )
  );
};

const sameIdMapText = (first: string, second: string): boolean => {
  const firstMap = parseIdMap(first);
  const secondMap = parseIdMap(second);
  if (!firstMap || !secondMap) return first === second;
  return sameIdMap(firstMap, secondMap);
};

const sameFilesystemEvidence = (
  first: FilesystemEvidence | undefined,
  second: FilesystemEvidence | undefined
): boolean => {
  if (!first || !second) return first === second;
  return FILESYSTEM_FIELD_KEYS.every((field) => first[field] === second[field]);
};

const validRootIdentity = (evidence?: FilesystemEvidence): boolean =>
  Boolean(
    evidence &&
    evidence.isDirectory === true &&
    /^\d+$/.test(evidence.device) &&
    /^[1-9]\d*$/.test(evidence.inode)
  );

const hasRestrictedSharedRoot = (
  mainEvidence: ProcEvidence,
  rendererEvidence: ProcEvidence
): boolean => {
  const mainRoot = mainEvidence.filesystem;
  const root = rendererEvidence.filesystem;
  const mainUid = parseIdMap(mainEvidence.uidMap);
  const mainGid = parseIdMap(mainEvidence.gidMap);
  const uid = parseIdMap(rendererEvidence.uidMap);
  const gid = parseIdMap(rendererEvidence.gidMap);
  return Boolean(
    SHARED_NAMESPACE_KEYS.every(
      (namespace) =>
        Boolean(mainEvidence.namespaces[namespace]) &&
        Boolean(rendererEvidence.namespaces[namespace])
    ) &&
    rendererEvidence.namespaces.user !== mainEvidence.namespaces.user &&
    rendererEvidence.namespaces.net !== mainEvidence.namespaces.net &&
    /^0+$/.test(rendererEvidence.status.CapPrm || '') &&
    /^0+$/.test(rendererEvidence.status.CapInh || '') &&
    mainUid &&
    mainGid &&
    uid &&
    gid &&
    uid.length === 1 &&
    gid.length === 1 &&
    uid[0][2] === 1 &&
    gid[0][2] === 1 &&
    !sameIdMap(uid, mainUid) &&
    !sameIdMap(gid, mainGid) &&
    validRootIdentity(mainRoot) &&
    validRootIdentity(root) &&
    mainRoot &&
    root &&
    (root.device !== mainRoot.device || root.inode !== mainRoot.inode) &&
    root.filesystemType === '40864' &&
    root.isDirectory &&
    root.chromiumFdinfoRoot &&
    root.cwdMatchesRoot &&
    (root.empty || root.deadProcRoot)
  );
};

const sameProcEvidence = (
  first: ProcEvidence,
  final: ProcEvidence,
  requireFilesystem: boolean
): boolean =>
  first.pid === final.pid &&
  first.startTime === final.startTime &&
  sameStatus(first.status, final.status) &&
  first.argv.length === final.argv.length &&
  first.argv.every((argument, index) => argument === final.argv[index]) &&
  NAMESPACE_KEYS.every(
    (namespace) => first.namespaces[namespace] === final.namespaces[namespace]
  ) &&
  sameIdMapText(first.uidMap, final.uidMap) &&
  sameIdMapText(first.gidMap, final.gidMap) &&
  (!requireFilesystem ||
    sameFilesystemEvidence(first.filesystem, final.filesystem));

export const validateRendererEvidence = (
  mainEvidence: ProcEvidence,
  rendererEvidence: ProcEvidence,
  requireUserNamespace = false
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
    rendererEvidence.namespaces.pid !== mainEvidence.namespaces.pid &&
    (rendererEvidence.namespaces.mnt !== mainEvidence.namespaces.mnt ||
      hasRestrictedSharedRoot(mainEvidence, rendererEvidence)) &&
    (!requireUserNamespace ||
      rendererEvidence.namespaces.user !== mainEvidence.namespaces.user)
  );
};
export const validateDappRendererSandbox = (
  webContents: WebContents
): boolean => {
  try {
    const rendererPid = webContents.getOSProcessId();
    if (process.platform === 'darwin' || process.platform === 'win32')
      return validateMetricRendererEvidence(
        app.getAppMetrics(),
        rendererPid,
        process.platform
      );
    if (process.platform !== 'linux') return false;
    if (
      !Number.isSafeInteger(rendererPid) ||
      rendererPid <= 1 ||
      webContents.isDestroyed()
    )
      return false;
    const mainEvidence = readProcEvidence(process.pid);
    const rendererEvidence = readProcEvidence(rendererPid);
    if (!validateRendererEvidence(mainEvidence, rendererEvidence)) return false;
    const sharedMount =
      mainEvidence.namespaces.mnt === rendererEvidence.namespaces.mnt;
    const finalMain = readProcEvidence(process.pid);
    const finalRenderer = readProcEvidence(rendererPid);
    return (
      validateRendererEvidence(finalMain, finalRenderer) &&
      sameProcEvidence(mainEvidence, finalMain, sharedMount) &&
      sameProcEvidence(rendererEvidence, finalRenderer, sharedMount) &&
      !webContents.isDestroyed() &&
      webContents.getOSProcessId() === rendererPid
    );
  } catch {
    return false;
  }
};

const parseOsRelease = (): HostIdentity | null => {
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
    const versionId = values.VERSION_ID || values.IMAGE_VERSION;
    return values.ID
      ? {
          id: values.ID,
          versionId,
          buildId: values.BUILD_ID,
          kernelRelease: os.release(),
          imageId: values.IMAGE_ID,
          imageVersion: values.IMAGE_VERSION,
        }
      : null;
  } catch {
    return null;
  }
};

type HostContract = {
  row: string;
  packageFamily: string;
  matrixRevision: string;
  fingerprint: HostIdentity;
  manifestFingerprint?: HostIdentity;
  helperMode: number;
  usernsOnly?: true;
};

const supportedHost = (
  host: HostIdentity,
  packageFamily: unknown
): HostContract | null => {
  const rows: readonly HostContract[] = [
    {
      row: 'ubuntu-24.04',
      packageFamily: 'deb',
      matrixRevision: MATRIX_REVISION,
      fingerprint: { id: 'ubuntu', versionId: '24.04' },
      helperMode: 0o4755,
    },
    {
      row: 'ubuntu-26.04',
      packageFamily: 'deb',
      matrixRevision: MATRIX_REVISION,
      fingerprint: { id: 'ubuntu', versionId: '26.04' },
      helperMode: 0o4755,
    },
    {
      row: 'debian-12',
      packageFamily: 'deb',
      matrixRevision: MATRIX_REVISION,
      fingerprint: { id: 'debian', versionId: '12' },
      helperMode: 0o4755,
    },
    {
      row: 'debian-13',
      packageFamily: 'deb',
      matrixRevision: MATRIX_REVISION,
      fingerprint: { id: 'debian', versionId: '13' },
      helperMode: 0o4755,
    },
    {
      row: 'fedora-43',
      packageFamily: 'rpm',
      matrixRevision: MATRIX_REVISION,
      fingerprint: { id: 'fedora', versionId: '43' },
      helperMode: 0o4755,
    },
    {
      row: 'arch-2026.09.01',
      packageFamily: 'arch',
      matrixRevision: ARCH_MATRIX_REVISION,
      fingerprint: {
        id: 'arch',
        buildId: 'rolling',
        kernelRelease: '7.2.2-arch1-1',
      },
      manifestFingerprint: {
        id: 'arch',
        versionId: '2026.09.01',
        buildId: 'rolling',
        kernelRelease: '7.2.2-arch1-1',
      },
      helperMode: 0o755,
      usernsOnly: true,
    },
    {
      row: 'omarchy-4.0.2',
      packageFamily: 'arch',
      matrixRevision: ARCH_MATRIX_REVISION,
      fingerprint: {
        id: 'omarchy',
        versionId: '4.0.2',
        buildId: '4.0.2',
        kernelRelease: '7.1.9-arch1-2',
      },
      helperMode: 0o755,
      usernsOnly: true,
    },
    {
      row: 'nixos-26.05',
      packageFamily: 'nix',
      matrixRevision: NIXOS_MATRIX_REVISION,
      fingerprint: { id: 'nixos', versionId: '26.05' },
      helperMode: 0o755,
      usernsOnly: true,
    },
  ];
  return (
    rows.find(
      (row) =>
        row.packageFamily === packageFamily &&
        Object.entries(row.fingerprint).every(
          ([key, value]) => host[key as keyof HostIdentity] === value
        )
    ) || null
  );
};

export const validatePackageIdentity = (
  manifest: PackageManifest,
  host: HostIdentity,
  cluster: string
): HostContract | null => {
  const contract = supportedHost(host, manifest.packageFamily);
  if (
    !contract ||
    manifest.schemaVersion !== 2 ||
    manifest.matrixRevision !== contract.matrixRevision ||
    manifest.matrixRow !== contract.row ||
    manifest.supportState !== 'supported' ||
    manifest.cluster !== cluster ||
    manifest.helper?.mode !==
      contract.helperMode.toString(8).padStart(4, '0') ||
    (contract.usernsOnly && manifest.policy?.kind !== 'none') ||
    Object.entries(contract.manifestFingerprint || contract.fingerprint).some(
      ([key, value]) =>
        manifest.distribution?.[key as keyof HostIdentity] !== value
    )
  )
    return null;
  return contract;
};

const sha256 = (filePath: string): string =>
  crypto.createHash('sha256').update(fs.readFileSync(filePath)).digest('hex');

const validateProductionPackage = (
  cluster: string,
  configuredInstallRoot?: string
): PackageValidation => {
  const unsupportedPackage = {
    failure: 'unsupported-package' as const,
    usernsOnly: false,
  };
  const expectedRoot = `/opt/daedalus/${cluster}`;
  const installRoot = configuredInstallRoot || process.env.ENTRYPOINT_DIR;
  if (!installRoot || path.resolve(installRoot) !== expectedRoot)
    return unsupportedPackage;

  try {
    if (fs.realpathSync(installRoot) !== expectedRoot)
      return unsupportedPackage;
    const configPath = path.join(expectedRoot, 'config/daedalus-config.json');
    const configStat = fs.lstatSync(configPath);
    if (
      process.env.DAEDALUS_CONFIG_FILE !== configPath ||
      fs.realpathSync(configPath) !== configPath ||
      !configStat.isFile() ||
      configStat.isSymbolicLink() ||
      configStat.uid !== 0 ||
      configStat.gid !== 0 ||
      (configStat.mode & 0o22) !== 0
    )
      return unsupportedPackage;

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
      return unsupportedPackage;

    const manifest = JSON.parse(
      fs.readFileSync(manifestPath, 'utf8')
    ) as PackageManifest;
    const host = parseOsRelease();
    if (!host) return { failure: 'unsupported-host', usernsOnly: false };
    const hostContract = supportedHost(host, manifest.packageFamily);
    if (!hostContract)
      return { failure: 'unsupported-host', usernsOnly: false };
    const contract = validatePackageIdentity(manifest, host, cluster);
    if (!contract) return unsupportedPackage;

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
      helperMode !== contract.helperMode ||
      (helperStat.mode & 0o7777) !== helperMode ||
      !/^[a-f0-9]{64}$/.test(String(manifest.helper?.sha256)) ||
      sha256(helperPath) !== manifest.helper?.sha256
    )
      return unsupportedPackage;
    return { failure: null, usernsOnly: contract.usernsOnly === true };
  } catch {
    return unsupportedPackage;
  }
};

export const validateNixOSProductionPackage = (
  cluster: string
): PackageValidation => {
  const unsupportedPackage = {
    failure: 'unsupported-package' as const,
    usernsOnly: false,
  };

  try {
    const entrypointDir = process.env.ENTRYPOINT_DIR;
    if (!entrypointDir) return unsupportedPackage;
    const canonicalRoot = fs.realpathSync(entrypointDir);
    if (
      path.resolve(entrypointDir) !== canonicalRoot ||
      !canonicalRoot.startsWith('/nix/store/')
    )
      return unsupportedPackage;
    const configPath = path.join(canonicalRoot, 'config/daedalus-config.json');
    const configStat = fs.lstatSync(configPath);
    if (
      process.env.DAEDALUS_CONFIG_FILE !== configPath ||
      fs.realpathSync(configPath) !== configPath ||
      !configStat.isFile() ||
      configStat.isSymbolicLink() ||
      configStat.uid !== 0 ||
      configStat.gid !== 0 ||
      (configStat.mode & 0o22) !== 0
    )
      return unsupportedPackage;

    const manifestPath = `/var/lib/daedalus/${cluster}/sandbox-identity.json`;
    const manifestStat = fs.lstatSync(manifestPath);
    if (
      !manifestStat.isFile() ||
      manifestStat.isSymbolicLink() ||
      manifestStat.uid !== 0 ||
      manifestStat.gid !== 0 ||
      (manifestStat.mode & 0o7777) !== 0o644
    )
      return unsupportedPackage;

    const manifest = JSON.parse(
      fs.readFileSync(manifestPath, 'utf8')
    ) as PackageManifest;
    const host = parseOsRelease();
    if (!host) return { failure: 'unsupported-host', usernsOnly: false };
    const hostContract = supportedHost(host, manifest.packageFamily);
    if (!hostContract)
      return { failure: 'unsupported-host', usernsOnly: false };
    const contract = validatePackageIdentity(manifest, host, cluster);
    if (!contract) return unsupportedPackage;

    const electronPath =
      typeof manifest.launch?.electron === 'string'
        ? manifest.launch.electron
        : null;
    if (
      !electronPath ||
      !electronPath.startsWith(`${canonicalRoot}${path.sep}`)
    )
      return unsupportedPackage;

    const helperPath = `/var/lib/daedalus/${cluster}/chrome-sandbox`;
    const electronStat = fs.lstatSync(electronPath);
    const helperStat = fs.lstatSync(helperPath);
    const helperMode = Number.parseInt(String(manifest.helper?.mode), 8);
    if (
      fs.realpathSync(process.execPath) !== electronPath ||
      !electronStat.isFile() ||
      electronStat.isSymbolicLink() ||
      electronStat.uid !== 0 ||
      electronStat.gid !== 0 ||
      (electronStat.mode & 0o22) !== 0 ||
      !helperStat.isFile() ||
      helperStat.isSymbolicLink() ||
      helperStat.uid !== 0 ||
      helperStat.gid !== 0 ||
      helperMode !== contract.helperMode ||
      (helperStat.mode & 0o7777) !== helperMode ||
      !/^[a-f0-9]{64}$/.test(String(manifest.helper?.sha256)) ||
      sha256(helperPath) !== manifest.helper?.sha256
    )
      return unsupportedPackage;
    return { failure: null, usernsOnly: contract.usernsOnly === true };
  } catch {
    return unsupportedPackage;
  }
};

const validateWindowsProductionPackage = (
  cluster: string,
  configuredInstallRoot?: string
): PackageValidation => {
  const unsupportedPackage = {
    failure: 'unsupported-package' as const,
    usernsOnly: false,
  };
  const installRoot =
    configuredInstallRoot || path.win32.dirname(process.execPath);
  const configPath = process.env.DAEDALUS_CONFIG_FILE || '';
  if (
    !validateWindowsPackageIdentity(
      {
        appName: app.getName(),
        appPath: app.getAppPath(),
        architecture: process.arch,
        executablePath: process.execPath,
        installRoot,
        isPackaged: app.isPackaged,
        configPath,
        resourcesPath: process.resourcesPath,
      },
      cluster
    )
  )
    return unsupportedPackage;

  try {
    const entries: ReadonlyArray<readonly [string, 'file' | 'directory']> = [
      [installRoot, 'directory'],
      [process.execPath, 'file'],
      [process.resourcesPath, 'directory'],
      [app.getAppPath(), 'directory'],
      [configPath, 'file'],
    ];
    if (
      entries.some(([entryPath, kind]) => {
        const stat = fs.lstatSync(entryPath);
        return (
          stat.isSymbolicLink() ||
          (kind === 'file' ? !stat.isFile() : !stat.isDirectory())
        );
      })
    )
      return unsupportedPackage;
    return { failure: null, usernsOnly: false };
  } catch {
    return unsupportedPackage;
  }
};

const validateDarwinProductionPackage = (
  cluster: string,
  configuredInstallRoot?: string
): PackageValidation => {
  const unsupportedPackage = {
    failure: 'unsupported-package' as const,
    usernsOnly: false,
  };
  const installRoot =
    configuredInstallRoot ||
    path.resolve(path.dirname(process.execPath), '..', '..');
  const configPath = process.env.DAEDALUS_CONFIG_FILE || '';
  if (
    !validateDarwinPackageIdentity(
      {
        appName: app.getName(),
        appPath: app.getAppPath(),
        architecture: process.arch,
        executablePath: process.execPath,
        installRoot,
        isPackaged: app.isPackaged,
        configPath,
        resourcesPath: process.resourcesPath,
      },
      cluster
    )
  )
    return unsupportedPackage;

  try {
    const productName = DAPP_PRODUCT_NAMES[cluster];
    const entries: ReadonlyArray<
      readonly [string, string, 'file' | 'directory']
    > = [
      [installRoot, '', 'directory'],
      [path.posix.join(installRoot, 'Contents'), 'Contents', 'directory'],
      [
        path.posix.join(installRoot, 'Contents/MacOS'),
        'Contents/MacOS',
        'directory',
      ],
      [
        path.posix.join(installRoot, 'Contents/Resources'),
        'Contents/Resources',
        'directory',
      ],
      [
        path.posix.join(installRoot, 'Contents/Resources/app'),
        'Contents/Resources/app',
        'directory',
      ],
      [
        path.posix.join(installRoot, 'Contents/MacOS/Frontend'),
        'Contents/MacOS/Frontend',
        'file',
      ],
      [
        path.posix.join(installRoot, 'Contents/MacOS', productName),
        `Contents/MacOS/${productName}`,
        'file',
      ],
      [
        path.posix.join(installRoot, 'Contents/Resources/helper'),
        'Contents/Resources/helper',
        'file',
      ],
      [configPath, 'Contents/Resources/daedalus-config.json', 'file'],
    ];
    const canonicalRoot = fs.realpathSync(installRoot);
    if (
      entries.some(([entryPath, relativePath, kind]) => {
        const stat = fs.lstatSync(entryPath);
        return (
          stat.isSymbolicLink() ||
          (kind === 'file' ? !stat.isFile() : !stat.isDirectory()) ||
          fs.realpathSync(entryPath) !==
            path.posix.join(canonicalRoot, relativePath)
        );
      })
    )
      return unsupportedPackage;
    return { failure: null, usernsOnly: false };
  } catch {
    return unsupportedPackage;
  }
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

const runCanary = async (
  requireUserNamespace = false
): Promise<DappSandboxUnavailableReason | null> => {
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
    let rendererStartIdentity: number | string;
    let initialMain: ProcEvidence | undefined;
    let initialRenderer: ProcEvidence | undefined;
    if (process.platform === 'darwin' || process.platform === 'win32') {
      const metric = findSandboxedRendererMetric(
        app.getAppMetrics(),
        rendererPid,
        process.platform
      );
      if (!metric) throw new Error('invalid sandbox evidence');
      rendererStartIdentity = metric.creationTime;
    } else {
      const mainEvidence = readProcEvidence(process.pid);
      const rendererEvidence = readProcEvidence(rendererPid);
      if (
        mainEvidence.status.Pid !== String(process.pid) ||
        !validateRendererEvidence(
          mainEvidence,
          rendererEvidence,
          requireUserNamespace
        )
      )
        throw new Error('invalid Linux sandbox evidence');
      rendererStartIdentity = rendererEvidence.startTime;
      initialMain = mainEvidence;
      initialRenderer = rendererEvidence;
    }

    await new Promise<void>((resolve) => {
      setImmediate(resolve);
    });
    if (
      canaryWindow.isDestroyed() ||
      canaryWindow.webContents.isDestroyed() ||
      canaryWindow.webContents.getOSProcessId() !== rendererPid
    )
      throw new Error('renderer changed');
    if (process.platform === 'darwin' || process.platform === 'win32') {
      const finalMetric = findSandboxedRendererMetric(
        app.getAppMetrics(),
        rendererPid,
        process.platform
      );
      if (!finalMetric || finalMetric.creationTime !== rendererStartIdentity)
        throw new Error('renderer instance changed');
    } else {
      const finalMain = readProcEvidence(process.pid);
      const finalRenderer = readProcEvidence(rendererPid);
      if (
        !initialMain ||
        !initialRenderer ||
        !validateRendererEvidence(
          finalMain,
          finalRenderer,
          requireUserNamespace
        ) ||
        !sameProcEvidence(
          initialMain,
          finalMain,
          initialMain.namespaces.mnt === initialRenderer.namespaces.mnt
        ) ||
        !sameProcEvidence(
          initialRenderer,
          finalRenderer,
          initialMain.namespaces.mnt === initialRenderer.namespaces.mnt
        ) ||
        canaryWindow.isDestroyed() ||
        canaryWindow.webContents.isDestroyed() ||
        canaryWindow.webContents.getOSProcessId() !== rendererPid
      )
        throw new Error('renderer instance changed');
    }
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
  if (
    process.platform !== 'darwin' &&
    process.platform !== 'linux' &&
    process.platform !== 'win32'
  )
    return { status: 'unavailable', reason: 'unsupported-host' };
  let argv = process.argv;
  if (process.platform === 'linux') {
    try {
      argv = [...argv, ...readProcEvidence(process.pid).argv];
    } catch {
      return { status: 'unavailable', reason: 'canary-failed' };
    }
  }
  if (
    hasSandboxBypass(argv, process.env) ||
    FORBIDDEN_SWITCHES.some((argument) =>
      app.commandLine.hasSwitch(argument.slice(2))
    )
  )
    return { status: 'unavailable', reason: 'sandbox-bypass' };

  let requireUserNamespace = false;
  if (!options.isDevelopment) {
    let packageValidation: PackageValidation;
    if (process.platform === 'darwin') {
      packageValidation = validateDarwinProductionPackage(
        options.cluster,
        options.installRoot
      );
    } else if (process.platform === 'win32') {
      packageValidation = validateWindowsProductionPackage(
        options.cluster,
        options.installRoot
      );
    } else {
      const host = parseOsRelease();
      if (host?.id === 'nixos') {
        packageValidation = validateNixOSProductionPackage(options.cluster);
      } else {
        packageValidation = validateProductionPackage(
          options.cluster,
          options.installRoot
        );
      }
    }
    if (packageValidation.failure)
      return { status: 'unavailable', reason: packageValidation.failure };
    requireUserNamespace = packageValidation.usernsOnly;
  }

  const canaryFailure = await runCanary(requireUserNamespace);
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
