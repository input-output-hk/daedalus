'use strict';

const assert = require('assert');
const childProcess = require('child_process');
const crypto = require('crypto');
const fs = require('fs');
const os = require('os');
const path = require('path');

const SCHEMA_VERSION = 2;
const IDENTITY_SCHEMA_VERSION = 2;
const MATRIX_REVISION = 'task-108-matrix-2026-08-18';
const ARCH_MATRIX_REVISION = 'task-111-matrix-2026-09-02';
const NIXOS_MATRIX_REVISION = 'task-112-matrix-2026-09-11';
const APPARMOR_LOADED_PROFILE_SUFFIX = ' (unconfined)';
const STDERR_LIMIT_BYTES = 8192;
const PROBE_TIMEOUT_MS = 15000;
const CLEANUP_TIMEOUT_MS = 2000;
const ROOT_TOKENS = [
  ['installRoot', '<INSTALL_ROOT>'],
  ['probeRoot', '<PROBE_ROOT>'],
  ['profileRoot', '<PROFILE_ROOT>'],
  ['home', '<HOME>'],
];
const NAMESPACE_NAMES = ['user', 'pid', 'mnt', 'net'];
const SANDBOX_CLASSES = new Set([
  'userns-only',
  'suid-only',
  'combined-unattributed',
]);
const SUPPORT_MATRIX = {
  'ubuntu-22.04': {
    packageFamily: 'deb',
    policy: 'none',
    supportState: 'wallet-only',
    reason: 'apparmor-policy-proof-pending',
    helperMode: '0755',
    distributionId: 'ubuntu',
    versionPattern: /^22\.04(?:\.\d+)?$/,
  },
  'ubuntu-24.04': {
    packageFamily: 'deb',
    policy: 'apparmor',
    supportState: 'supported',
    reason: 'supported',
    helperMode: '4755',
    apparmorSemanticIdentity: 'default-allow-userns-ubuntu-24.04',
    distributionId: 'ubuntu',
    versionPattern: /^24\.04(?:\.\d+)?$/,
  },
  'ubuntu-26.04': {
    packageFamily: 'deb',
    policy: 'apparmor',
    supportState: 'supported',
    reason: 'supported',
    helperMode: '4755',
    apparmorSemanticIdentity: 'default-allow-userns-ubuntu-26.04',
    distributionId: 'ubuntu',
    versionPattern: /^26\.04(?:\.\d+)?$/,
  },
  'debian-12': {
    packageFamily: 'deb',
    policy: 'none',
    supportState: 'supported',
    reason: 'supported',
    helperMode: '4755',
    distributionId: 'debian',
    versionPattern: /^12(?:\.\d+)?$/,
  },
  'debian-13': {
    packageFamily: 'deb',
    policy: 'none',
    supportState: 'supported',
    reason: 'supported',
    helperMode: '4755',
    distributionId: 'debian',
    versionPattern: /^13(?:\.\d+)?$/,
  },
  'fedora-43': {
    packageFamily: 'rpm',
    policy: 'selinux',
    supportState: 'supported',
    reason: 'supported',
    helperMode: '4755',
    distributionId: 'fedora',
    versionPattern: /^43$/,
  },
  'arch-2026.09.01': {
    packageFamily: 'arch',
    policy: 'none',
    supportState: 'supported',
    reason: 'supported',
    helperMode: '0755',
    matrixRevision: ARCH_MATRIX_REVISION,
    distributionId: 'arch',
    versionPattern: /^2026\.09\.01$/,
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
  },
  'omarchy-4.0.2': {
    packageFamily: 'arch',
    policy: 'none',
    supportState: 'supported',
    reason: 'supported',
    helperMode: '0755',
    matrixRevision: ARCH_MATRIX_REVISION,
    distributionId: 'omarchy',
    versionPattern: /^4\.0\.2$/,
    fingerprint: {
      id: 'omarchy',
      versionId: '4.0.2',
      buildId: '4.0.2',
      kernelRelease: '7.1.9-arch1-2',
    },
  },
  'nixos-26.05': {
    packageFamily: 'nix',
    policy: 'none',
    supportState: 'supported',
    reason: 'supported',
    helperMode: '0755',
    matrixRevision: NIXOS_MATRIX_REVISION,
    distributionId: 'nixos',
    versionPattern: /^26\.05$/,
  },
};
const FORBIDDEN_SWITCHES = [
  '--disable-gpu-sandbox',
  '--disable-sandbox',
  '--disable-seccomp-filter-sandbox',
  '--disable-setuid-sandbox',
  '--in-process-gpu',
  '--no-sandbox',
  '--single-process',
];
const STDERR_CATEGORIES = [
  ['namespace-denied', /namespace|userns|operation not permitted/i],
  ['apparmor-denied', /apparmor.*(?:denied|reject)|audit.*denied/i],
  ['selinux-denied', /selinux|avc:.*denied/i],
  ['evidence-invalid', /evidence-invalid|assertion|invalid/i],
  ['sandbox-init-failed', /sandbox.*(?:fail|fatal)|zygote.*(?:fail|fatal)/i],
  ['timeout', /timed? out|sandbox-probe-failed:timeout/i],
  ['renderer-exited', /render.*(?:crash|exit|gone)/i],
];

function debug(stage) {
  if (
    process.env.DAEDALUS_PROBE_DEBUG === '1' &&
    !process.argv.includes('--exit-only')
  ) {
    process.stderr.write(`sandbox-probe-stage:${stage}\n`);
  }
}

function createEvidenceEnvelope(values = {}) {
  return {
    schemaVersion: SCHEMA_VERSION,
    result: null,
    matrix: null,
    host: null,
    versions: null,
    paths: null,
    files: null,
    directories: null,
    policy: null,
    main: null,
    renderer: null,
    assertions: null,
    failure: null,
    diagnostics: null,
    ...values,
  };
}

function createFailureEvidence(error, context = null, partial = {}) {
  const safeCode = /^[a-z0-9:-]+$/.test(String(error && error.message))
    ? error.message
    : 'unclassified';
  const category =
    error && error.name === 'AssertionError'
      ? 'evidence-invalid'
      : categorizeStderr(safeCode, 1);
  return createEvidenceEnvelope({
    ...partial,
    result: 'fail',
    matrix: context,
    failure: { category, code: safeCode },
    diagnostics: {
      sanitized: true,
      rawHostDataExported: false,
      stderrSummaryRequired: true,
    },
  });
}

function writeFailure(error, context) {
  const evidence = createFailureEvidence(error, context);
  process.stdout.write(
    `${JSON.stringify(evidence, null, 2)}\n`
  );
  process.stderr.write(
    `sandbox-probe-failed:${evidence.failure.category}:${evidence.failure.code}\n`
  );
}

function escapeRegExp(value) {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function replaceLiteralIgnoreCase(value, literal, replacement) {
  if (!literal) return value;
  return value.replace(new RegExp(escapeRegExp(literal), 'gi'), replacement);
}

function sensitiveEnvironmentEntries(environment, roots) {
  const sensitiveName =
    /(?:auth|cookie|credential|daedalus|electron|home|host|key|logname|password|path|secret|token|url|user|xdg)/i;
  const separatelySanitizedName =
    /^(?:HOSTNAME|LOGNAME|USER|XDG_(?:CURRENT_DESKTOP|SESSION_CLASS|SESSION_DESKTOP|SESSION_TYPE)|DAEDALUS_PROBE_(?:CLUSTER|DEBUG|MATRIX_REVISION|MATRIX_ROW|SANDBOX_CLASS|SELINUX_SOURCE_CONTEXT))$/;
  const rootValues = new Set(Object.values(roots));
  return Object.entries(environment)
    .filter(
      ([name, value]) =>
        !separatelySanitizedName.test(name) &&
        sensitiveName.test(name) &&
        value &&
        value.length >= 4 &&
        !rootValues.has(value)
    )
    .sort(([, left], [, right]) => right.length - left.length);
}

function sensitiveEnvironmentValues(environment, roots) {
  return sensitiveEnvironmentEntries(environment, roots).map(([, value]) => value);
}

function privacyContext(roots) {
  const environmentEntries = sensitiveEnvironmentEntries(process.env, roots);
  return {
    roots,
    username: os.userInfo().username,
    hostname: os.hostname(),
    environmentNames: environmentEntries.map(([name]) => name),
    environmentValues: environmentEntries.map(([, value]) => value),
  };
}

function withTimeout(promise, timeoutMs) {
  let timeout;
  const deadline = new Promise((_, reject) => {
    timeout = setTimeout(() => reject(new Error('timeout')), timeoutMs);
  });
  return Promise.race([promise, deadline]).finally(() => clearTimeout(timeout));
}

async function performCleanup(probeSession, profileRoot, dependencies = {}) {
  const remove = dependencies.remove || fs.promises.rm;
  const failures = [];
  if (probeSession) {
    try {
      const results = await withTimeout(
        Promise.allSettled([
          probeSession.clearStorageData(),
          probeSession.clearCache(),
        ]),
        CLEANUP_TIMEOUT_MS
      );
      if (results.some((result) => result.status === 'rejected')) {
        failures.push('session-cleanup-failed');
      }
    } catch (_error) {
      failures.push('session-cleanup-failed');
    }
  }
  try {
    await withTimeout(remove(profileRoot, { recursive: true, force: true }), CLEANUP_TIMEOUT_MS);
  } catch (_error) {
    failures.push('profile-cleanup-failed');
  }
  return failures.length > 0 ? new Error(failures.join(':')) : null;
}

function sha256(data) {
  return crypto.createHash('sha256').update(data).digest('hex');
}

function hashFile(filePath) {
  return sha256(fs.readFileSync(filePath));
}

function canonicalPath(filePath) {
  return fs.realpathSync(filePath);
}

function inferInstallRoot(executablePath) {
  const marker = `${path.sep}libexec${path.sep}`;
  const markerIndex = executablePath.indexOf(marker);
  if (markerIndex <= 0) {
    throw new Error('installed-electron-path-required');
  }
  return executablePath.slice(0, markerIndex);
}

function createProfileRoot() {
  const requestedRoot = process.env.DAEDALUS_PROBE_PROFILE_ROOT;
  if (!requestedRoot) {
    return fs.mkdtempSync(path.join(os.tmpdir(), 'daedalus-sandbox-probe-'));
  }

  const profileRoot = path.resolve(requestedRoot);
  if (fs.existsSync(profileRoot)) throw new Error('profile-root-already-exists');
  fs.mkdirSync(profileRoot, { mode: 0o700 });
  return profileRoot;
}

function replaceKnownRoots(value, roots) {
  let normalized = value;
  const replacements = ROOT_TOKENS.map(([name, token]) => [
    roots[name],
    token,
  ])
    .filter(([root]) => Boolean(root))
    .sort(([left], [right]) => right.length - left.length);

  for (const [root, token] of replacements) {
    const pattern = new RegExp(
      `(^|[^A-Za-z0-9_])${escapeRegExp(root)}(?=$|[/\\\\]|[^A-Za-z0-9_])`,
      'g'
    );
    normalized = normalized.replace(pattern, (_match, prefix) => `${prefix}${token}`);
  }
  return normalized;
}

function protectRootTokens(value) {
  let protectedValue = value;
  const restore = [];
  for (const [, token] of ROOT_TOKENS) {
    const sentinel = `__DAEDALUS_ROOT_${restore.length}__`;
    protectedValue = protectedValue.split(token).join(sentinel);
    restore.push([sentinel, token]);
  }
  return { protectedValue, restore };
}

function restoreRootTokens(value, restore) {
  let restored = value;
  for (const [sentinel, token] of restore) {
    restored = restored.split(sentinel).join(token);
  }
  return restored;
}

function replaceAbsolutePaths(value) {
  let pathIndex = 0;
  return value.replace(
    /(^|[^A-Za-z0-9_>\/])\/(?!\/)[^\s"'<>[\]{}(),;]*/g,
    (match, prefix) => {
      pathIndex += 1;
      return `${prefix}<PATH_${pathIndex}>`;
    }
  );
}

function replaceEnvironmentValues(value) {
  return value.replace(
    /(^|[\s,;])([A-Za-z_][A-Za-z0-9_]{0,63})=(?!<ENV_VALUE>)("[^"]*"|'[^']*'|[^\s]+)/gm,
    '$1$2=<ENV_VALUE>'
  );
}

function sanitizeText(rawValue, privacy) {
  const { roots, username, hostname, environmentValues } = privacy;
  let sanitized = rawValue.replace(/\r\n?/g, '\n');
  sanitized = sanitized.replace(
    /\b[a-z][a-z0-9+.-]*:\/\/[^\s"'<>]*/gi,
    '<URL>'
  );
  sanitized = replaceKnownRoots(sanitized, roots);
  sanitized = replaceLiteralIgnoreCase(sanitized, username, '<USER>');
  sanitized = replaceLiteralIgnoreCase(sanitized, hostname, '<HOST>');
  for (const value of environmentValues) {
    sanitized = sanitized.split(value).join('<ENV_VALUE>');
  }
  sanitized = replaceEnvironmentValues(sanitized);

  const { protectedValue, restore } = protectRootTokens(sanitized);
  sanitized = replaceAbsolutePaths(protectedValue);
  sanitized = restoreRootTokens(sanitized, restore);
  return sanitized;
}

function truncateUtf8(value, maxBytes) {
  const bytes = Buffer.from(value, 'utf8');
  if (bytes.length <= maxBytes) return { value, truncated: false };

  let end = maxBytes;
  while (end > 0 && (bytes[end] & 0xc0) === 0x80) end -= 1;
  return {
    value: `${bytes.subarray(0, end).toString('utf8')}\n<TRUNCATED>`,
    truncated: true,
  };
}
function assertNoSensitiveContent(value, privacy) {
  const { roots, username, hostname, environmentNames = [], environmentValues } = privacy;
  for (const [name] of ROOT_TOKENS) {
    if (roots[name] && value.includes(roots[name])) {
      throw new Error(`residual-sensitive-root:${name}`);
    }
  }
  if (
    username &&
    new RegExp(
      `(^|[^A-Za-z0-9_])${escapeRegExp(username)}($|[^A-Za-z0-9_])`,
      'i'
    ).test(value)
  ) {
    throw new Error('residual-username');
  }
  if (
    hostname &&
    new RegExp(
      `(^|[^A-Za-z0-9_])${escapeRegExp(hostname)}($|[^A-Za-z0-9_])`,
      'i'
    ).test(value)
  ) {
    throw new Error('residual-hostname');
  }
  if (/\b[a-z][a-z0-9+.-]*:\/\//i.test(value)) {
    throw new Error('residual-url');
  }
  for (const [index, environmentValue] of environmentValues.entries()) {
    if (value.includes(environmentValue)) {
      const name = environmentNames[index]
        ? environmentNames[index].toLowerCase().replace(/_/g, '-')
        : 'unknown';
      throw new Error(`residual-environment-value:${name}`);
    }
  }

  const { protectedValue } = protectRootTokens(value);
  if (/(^|[^A-Za-z0-9_>\/])\/(?!\/)[^\s"'<>[\]{}(),;]*/m.test(protectedValue)) {
    throw new Error('residual-absolute-path');
  }
  if (
    /(^|[\s,;])[A-Za-z_][A-Za-z0-9_]{0,63}=(?!<ENV_VALUE>)[^\s]+/m.test(value)
  ) {
    throw new Error('residual-environment-value');
  }
}

function normalizeArgv(argv, privacy) {
  return argv.map((argument) => {
    const normalized = sanitizeText(argument, privacy).replace(
      /(--[a-z0-9-]*pid[a-z0-9-]*=)\d+/gi,
      '$1<PID>'
    );
    assertNoSensitiveContent(normalized, privacy);
    return normalized;
  });
}

function categorizeStderr(stderr, exitCode) {
  if (exitCode === 0 && stderr.trim() === '') return 'success';
  const match = STDERR_CATEGORIES.find(([, pattern]) => pattern.test(stderr));
  return match ? match[0] : 'other';
}

function relevantStderr(rawText) {
  const relevantLine =
    /sandbox-probe-|\bFATAL\b|apparmor|selinux|avc:|userns|namespace|sandbox.*(?:fail|fatal)|zygote.*(?:fail|fatal)/i;
  const lines = rawText.split('\n').filter((line) => relevantLine.test(line));
  if (lines.length > 0) return `${lines.join('\n')}\n`;
  return rawText.trim() === '' ? '' : '<NON_PROBE_STDERR_REDACTED>\n';
}

function summarizeStderr(rawBytes, exitCode, privacy) {
  const rawText = rawBytes.toString('utf8');
  const sanitized = sanitizeText(relevantStderr(rawText), privacy);
  assertNoSensitiveContent(sanitized, privacy);
  const excerpt = truncateUtf8(sanitized, STDERR_LIMIT_BYTES);
  return {
    exitCode,
    byteCount: rawBytes.length,
    sha256: sha256(rawBytes),
    category: categorizeStderr(rawText, exitCode),
    sanitizedExcerpt: excerpt.value,
    truncated: excerpt.truncated,
  };
}

function parseStatus(statusText) {
  const allowed = new Set([
    'Name',
    'Pid',
    'PPid',
    'NoNewPrivs',
    'Seccomp',
    'Seccomp_filters',
    'CapEff',
    'CapPrm',
    'CapInh',
  ]);
  const result = {};
  for (const line of statusText.split('\n')) {
    const separator = line.indexOf(':');
    if (separator === -1) continue;
    const key = line.slice(0, separator);
    if (allowed.has(key)) result[key] = line.slice(separator + 1).trim();
  }
  return result;
}

function parseProcessStartTime(statText) {
  const commandEnd = statText.lastIndexOf(')');
  assert(commandEnd !== -1, 'proc-stat-command');
  const fields = statText.slice(commandEnd + 2).trim().split(/\s+/);
  const startTime = fields[19];
  assert(/^\d+$/.test(startTime || ''), 'proc-stat-start-time');
  return startTime;
}

function readFilesystemEvidence(pid, io = fs) {
  let rootFd;
  let cwdFd;
  let result;
  try {
    if (typeof io.statfsSync !== 'function') return undefined;
    const rootPath = `/proc/${pid}/root`;
    const before = io.readlinkSync(rootPath);
    const flags = 0x200000 | fs.constants.O_DIRECTORY; // Linux O_PATH.
    rootFd = io.openSync(rootPath, flags);
    cwdFd = io.openSync(`/proc/${pid}/cwd`, flags);
    const root = io.fstatSync(rootFd, { bigint: true });
    const cwd = io.fstatSync(cwdFd, { bigint: true });
    const heldRoot = `/proc/self/fd/${rootFd}`;
    const filesystemType = io.statfsSync(heldRoot, { bigint: true }).type.toString();
    const chromiumFdinfoRoot =
      /^\/proc\/[1-9][0-9]*(\/task\/[1-9][0-9]*)?\/fdinfo( \(deleted\))?$/.test(before);
    let empty = false;
    let deadProcRoot = false;
    try {
      empty = io.readdirSync(heldRoot).length === 0;
    } catch (error) {
      if (
        error && error.code === 'ESRCH' && chromiumFdinfoRoot &&
        filesystemType === '40864' && root.isDirectory()
      ) deadProcRoot = true;
      else throw error;
    }
    if (io.readlinkSync(rootPath) !== before) return undefined;
    result = {
      device: root.dev.toString(),
      inode: root.ino.toString(),
      filesystemType,
      isDirectory: root.isDirectory(),
      empty,
      deadProcRoot,
      chromiumFdinfoRoot,
      cwdMatchesRoot: cwd.isDirectory() && cwd.dev === root.dev && cwd.ino === root.ino,
    };
  } catch (_) {
    result = undefined;
  } finally {
    for (const fd of [cwdFd, rootFd]) {
      if (fd !== undefined) {
        try { io.closeSync(fd); } catch (_) { result = undefined; }
      }
    }
  }
  return result;
}

function parseNamespaceMap(value) {
  if (typeof value !== 'string' || !value.trim()) return undefined;
  const ranges = [];
  for (const line of value.trim().split('\n')) {
    const fields = line.trim().split(/\s+/);
    if (fields.length !== 3 || fields.some((field) => !/^\d+$/.test(field))) return undefined;
    const [inside, outside, count] = fields.map(Number);
    if (
      [inside, outside, count].some((field) => !Number.isSafeInteger(field) || field > 0xffffffff) ||
      count === 0 || inside + count > 0x100000000 || outside + count > 0x100000000
    ) return undefined;
    ranges.push([inside, outside, count]);
  }
  return ranges;
}

function namespaceMapsEqual(left, right) {
  const leftRanges = parseNamespaceMap(left);
  const rightRanges = parseNamespaceMap(right);
  if (!leftRanges || !rightRanges) return left === right;
  return leftRanges.length === rightRanges.length && leftRanges.every((range, index) =>
    range.every((field, column) => field === rightRanges[index][column])
  );
}

function validRootIdentity(filesystem) {
  return Boolean(filesystem && filesystem.isDirectory === true &&
    /^\d+$/.test(filesystem.device) && /^\d+$/.test(filesystem.inode));
}

function sameRoot(left, right) {
  return left.device === right.device && left.inode === right.inode;
}

function readProcEvidence(pid, privacy, io = fs) {
  const procRoot = `/proc/${pid}`;
  const initialStatus = parseStatus(io.readFileSync(`${procRoot}/status`, 'utf8'));
  const initialStart = parseProcessStartTime(io.readFileSync(`${procRoot}/stat`, 'utf8'));
  assert.strictEqual(Number(initialStatus.Pid), pid, 'proc-pid-mismatch');
  const rawArgv = io.readFileSync(`${procRoot}/cmdline`).toString('utf8').split('\0').filter(Boolean);
  const namespaces = Object.fromEntries(
    NAMESPACE_NAMES.map((name) => [name, io.readlinkSync(`${procRoot}/ns/${name}`)])
  );
  const uidMap = io.readFileSync(`${procRoot}/uid_map`, 'utf8').trim();
  const gidMap = io.readFileSync(`${procRoot}/gid_map`, 'utf8').trim();
  assert(/^[\d\s]+$/.test(uidMap) && /^[\d\s]+$/.test(gidMap), 'unexpected-namespace-map');
  let securityLabel;
  try {
    securityLabel = io.readFileSync(`${procRoot}/attr/current`, 'utf8').replace(/\0+$/, '').trim();
  } catch (error) {
    if (error && (error.code === 'ENOENT' || error.code === 'EINVAL')) securityLabel = null;
    else throw error;
  }
  const filesystem = readFilesystemEvidence(pid, io);
  const status = parseStatus(io.readFileSync(`${procRoot}/status`, 'utf8'));
  const startTime = parseProcessStartTime(io.readFileSync(`${procRoot}/stat`, 'utf8'));
  assert.deepStrictEqual(status, initialStatus, 'proc-status-changed');
  assert.strictEqual(startTime, initialStart, 'proc-instance-replaced');
  return { pid, argv: normalizeArgv(rawArgv, privacy), status, namespaces, uidMap, gidMap,
    securityLabel, startTime, filesystem };
}

function assertRendererInstance(initialEvidence, finalEvidence) {
  assert.strictEqual(finalEvidence.status.Pid, initialEvidence.status.Pid, 'renderer-pid-reused');
  assert.strictEqual(
    finalEvidence.startTime,
    initialEvidence.startTime,
    'renderer-instance-replaced'
  );
}

function assertStableEvidence(initial, final, requireFilesystem) {
  assertRendererInstance(initial, final);
  assert.deepStrictEqual(final.status, initial.status, 'process-status-changed');
  assert.deepStrictEqual(final.argv, initial.argv, 'process-argv-changed');
  assert.deepStrictEqual(final.namespaces, initial.namespaces, 'process-namespaces-changed');
  assert(namespaceMapsEqual(final.uidMap, initial.uidMap), 'process-uid-map-changed');
  assert(namespaceMapsEqual(final.gidMap, initial.gidMap), 'process-gid-map-changed');
  if (requireFilesystem) {
    assert.deepStrictEqual(final.filesystem, initial.filesystem, 'process-filesystem-changed');
  }
  assert.strictEqual(final.securityLabel, initial.securityLabel, 'process-label-changed');
}

async function assertRendererAfterLifecycleYield(
  initialMain, initialRenderer, rendererPid, webContents, isRendererGone,
  validate, privacy, dependencies = {}
) {
  const yieldToEvents =
    dependencies.yieldToEvents || (() => new Promise((resolve) => setImmediate(resolve)));
  const readEvidence = dependencies.readEvidence || ((pid) => readProcEvidence(pid, privacy));
  await yieldToEvents();
  const assertLive = () => {
    assert(!isRendererGone(), 'renderer-exited-during-collection');
    assert(!webContents.isDestroyed(), 'renderer-destroyed-after-collection');
    assert.strictEqual(webContents.getOSProcessId(), rendererPid, 'renderer-pid-changed-after-collection');
  };
  assertLive();
  const main = readEvidence(initialMain.pid);
  const renderer = readEvidence(rendererPid);
  assert.strictEqual(Number(main.status.Pid), initialMain.pid, 'main-proc-pid-changed');
  assert.strictEqual(Number(renderer.status.Pid), rendererPid, 'renderer-proc-pid-changed');
  validate(main, renderer);
  const requireFilesystem = initialRenderer.namespaces.mnt === initialMain.namespaces.mnt;
  assertStableEvidence(initialMain, main, requireFilesystem);
  assertStableEvidence(initialRenderer, renderer, requireFilesystem);
  assertLive();
  return { main, renderer };
}

function hasForbiddenSwitch(argv) {
  return argv.some((argument) =>
    FORBIDDEN_SWITCHES.some(
      (forbidden) => argument === forbidden || argument.startsWith(`${forbidden}=`)
    )
  );
}

function mapClassification(value) {
  const identity = value
    .split('\n')
    .filter(Boolean)
    .every((line) => {
      const [inside, outside] = line.trim().split(/\s+/).map(Number);
      return Number.isSafeInteger(inside) && inside === outside;
    });
  return identity ? 'identity' : 'remapped';
}

function normalizeProcessEvidence(
  evidence,
  mainEvidence,
  pidToken,
  privacy,
  includeSecurityLabel = false
) {
  const securityLabel = includeSecurityLabel && evidence.securityLabel
    ? sanitizeText(evidence.securityLabel, privacy)
    : null;
  if (securityLabel) assertNoSensitiveContent(securityLabel, privacy);
  return {
    pid: pidToken,
    pidRelationships: {
      pidMatchesObserved: true,
      parentIsMain:
        Boolean(evidence.status.PPid) && evidence.status.PPid === mainEvidence.status.Pid,
    },
    argv: evidence.argv,
    status: {
      NoNewPrivs: evidence.status.NoNewPrivs,
      Seccomp: evidence.status.Seccomp,
      Seccomp_filters: evidence.status.Seccomp_filters,
      CapEff: evidence.status.CapEff,
      CapPrm: evidence.status.CapPrm,
      CapInh: evidence.status.CapInh,
    },
    namespaces: Object.fromEntries(
      NAMESPACE_NAMES.map((name) => [
        name,
        { sameAsMain: evidence.namespaces[name] === mainEvidence.namespaces[name] },
      ])
    ),
    maps: {
      uid: {
        sameAsMain: namespaceMapsEqual(evidence.uidMap, mainEvidence.uidMap),
        classification: mapClassification(evidence.uidMap),
      },
      gid: {
        sameAsMain: namespaceMapsEqual(evidence.gidMap, mainEvidence.gidMap),
        classification: mapClassification(evidence.gidMap),
      },
    },
    filesystem: validRootIdentity(evidence.filesystem) &&
      validRootIdentity(mainEvidence.filesystem) && evidence.filesystem.chromiumFdinfoRoot &&
      evidence.filesystem.filesystemType === '40864' && evidence.filesystem.isDirectory &&
      evidence.filesystem.cwdMatchesRoot && (evidence.filesystem.empty || evidence.filesystem.deadProcRoot)
      ? {
        route: 'chromium-proc-fdinfo',
        sameRootAsMain: sameRoot(evidence.filesystem, mainEvidence.filesystem),
        procfs: evidence.filesystem.filesystemType === '40864',
        directory: evidence.filesystem.isDirectory,
        empty: evidence.filesystem.empty,
        deadProcRoot: evidence.filesystem.deadProcRoot,
        cwdMatchesRoot: evidence.filesystem.cwdMatchesRoot,
      } : null,
    securityLabel,
  };
}

function normalizedPath(filePath, privacy) {
  if (filePath.startsWith('/etc/apparmor.d/')) return '<APPARMOR_PROFILE>';
  if (filePath.startsWith('/usr/share/selinux/packages/')) return '<SELINUX_POLICY>';
  const value = sanitizeText(filePath, privacy);
  assertNoSensitiveContent(value, privacy);
  return value;
}

function normalizedHostEnvironment(host) {
  const kernel = os.release();
  assert(/^[A-Za-z0-9._+-]+$/.test(kernel), 'kernel-release');
  const sessionType = (process.env.XDG_SESSION_TYPE || 'unknown').toLowerCase();
  assert(new Set(['x11', 'wayland', 'tty', 'unknown']).has(sessionType), 'session-type');
  return { ...host, kernel, sessionType };
}

function assertRendererEvidence(mainEvidence, rendererEvidence, sandboxClass) {
  const chromiumProcessType = rendererEvidence.argv.find((argument) =>
    argument.startsWith('--type=')
  );
  assert(
    chromiumProcessType === '--type=renderer' || chromiumProcessType === '--type=zygote',
    'unexpected-renderer-process-type'
  );
  assert(!hasForbiddenSwitch(mainEvidence.argv), 'forbidden-main-switch');
  assert(!hasForbiddenSwitch(rendererEvidence.argv), 'forbidden-renderer-switch');
  assert.strictEqual(rendererEvidence.status.NoNewPrivs, '1', 'no-new-privs');
  assert.strictEqual(rendererEvidence.status.Seccomp, '2', 'seccomp-mode');
  if (rendererEvidence.status.Seccomp_filters !== undefined) {
    assert(
      Number(rendererEvidence.status.Seccomp_filters) > 0,
      'seccomp-filter-count'
    );
  }
  assert(/^0+$/.test(rendererEvidence.status.CapEff || ''), 'effective-capabilities');
  for (const name of NAMESPACE_NAMES) {
    assert(rendererEvidence.namespaces[name], `missing-${name}-namespace`);
  }
  assert.notStrictEqual(
    rendererEvidence.namespaces.pid,
    mainEvidence.namespaces.pid,
    'shared-pid-namespace'
  );
  if (rendererEvidence.namespaces.mnt === mainEvidence.namespaces.mnt) {
    for (const name of ['pid', 'user', 'net']) {
      assert(mainEvidence.namespaces[name] && rendererEvidence.namespaces[name], `missing-${name}-namespace`);
    }
    for (const name of ['user', 'net']) {
      assert.notStrictEqual(rendererEvidence.namespaces[name], mainEvidence.namespaces[name], `shared-${name}-namespace`);
    }
    for (const field of ['CapPrm', 'CapInh']) {
      assert(/^0+$/.test(rendererEvidence.status[field] || ''), `${field}-capabilities`);
    }
    for (const field of ['uidMap', 'gidMap']) {
      const ranges = parseNamespaceMap(rendererEvidence[field]);
      assert(ranges && ranges.length === 1 && ranges[0][2] === 1, 'renderer-single-id-map');
      assert(parseNamespaceMap(mainEvidence[field]), 'main-namespace-map');
      assert(!namespaceMapsEqual(rendererEvidence[field], mainEvidence[field]), `shared-${field}`);
    }
    const root = rendererEvidence.filesystem;
    assert(validRootIdentity(mainEvidence.filesystem) && validRootIdentity(root), 'missing-root-identity');
    assert(
      root.filesystemType === '40864' && root.isDirectory && root.chromiumFdinfoRoot &&
      root.cwdMatchesRoot && (root.empty || root.deadProcRoot) &&
      !sameRoot(root, mainEvidence.filesystem),
      'unverified-shared-mount-root'
    );
  }
  if (sandboxClass !== 'suid-only') {
    assert.notStrictEqual(
      rendererEvidence.namespaces.user,
      mainEvidence.namespaces.user,
      'shared-user-namespace'
    );
    assert(!namespaceMapsEqual(rendererEvidence.uidMap, mainEvidence.uidMap), 'shared-uid-map');
    assert(!namespaceMapsEqual(rendererEvidence.gidMap, mainEvidence.gidMap), 'shared-gid-map');
  }
}

function fileMetadata(filePath) {
  const lstat = fs.lstatSync(filePath);
  assert(lstat.isFile(), 'expected-regular-file');
  assert(!lstat.isSymbolicLink(), 'unexpected-symlink');
  return {
    sha256: hashFile(filePath),
    mode: (lstat.mode & 0o7777).toString(8).padStart(4, '0'),
    uid: lstat.uid,
    gid: lstat.gid,
    regularFile: true,
    symlink: false,
  };
}

function assertRootFile(filePath, expectedMode) {
  const metadata = fileMetadata(filePath);
  assertFileContract(metadata, expectedMode);
  return metadata;
}

function assertFileContract(metadata, expectedMode) {
  assert.strictEqual(metadata.uid, 0, 'file-owner');
  assert.strictEqual(metadata.gid, 0, 'file-group');
  assert.strictEqual(metadata.mode, expectedMode, 'file-mode');
}

function assertRootDirectory(directoryPath) {
  const stats = fs.lstatSync(directoryPath);
  assert(stats.isDirectory() && !stats.isSymbolicLink(), 'expected-directory');
  assert.strictEqual(stats.uid, 0, 'directory-owner');
  assert.strictEqual(stats.gid, 0, 'directory-group');
  assert.strictEqual(
    (stats.mode & 0o7777).toString(8).padStart(4, '0'),
    '0755',
    'directory-mode'
  );
  return { mode: '0755', owner: 'root', group: 'root' };
}

function publicFileMetadata(metadata) {
  return {
    sha256: metadata.sha256,
    mode: metadata.mode,
    owner: metadata.uid === 0 ? 'root' : 'non-root',
    group: metadata.gid === 0 ? 'root' : 'non-root',
    regularFile: metadata.regularFile,
    symlink: metadata.symlink,
  };
}

function assertIdentityHash(expected, metadata, name) {
  assert(expected && /^[a-f0-9]{64}$/.test(expected.sha256), `identity-${name}-hash`);
  assert.strictEqual(metadata.sha256, expected.sha256, `identity-${name}-mismatch`);
}

function runHostCommand(command, args) {
  const result = childProcess.spawnSync(command, args, {
    encoding: 'utf8',
    timeout: 5000,
    env: { PATH: '/usr/sbin:/usr/bin:/sbin:/bin' },
  });
  return {
    status: result.status,
    stdout: result.stdout || '',
    stderr: result.stderr || '',
    error: result.error,
  };
}

function requiredEnvironment(name) {
  const value = process.env[name];
  if (!value) throw new Error(`missing-${name.toLowerCase().replaceAll('_', '-')}`);
  return value;
}

function readContractSelection(installRoot) {
  const matrixRow = requiredEnvironment('DAEDALUS_PROBE_MATRIX_ROW');
  const matrixRevision = requiredEnvironment('DAEDALUS_PROBE_MATRIX_REVISION');
  const sandboxClass = requiredEnvironment('DAEDALUS_PROBE_SANDBOX_CLASS');
  const cluster = requiredEnvironment('DAEDALUS_PROBE_CLUSTER');
  const row = SUPPORT_MATRIX[matrixRow];
  if (!row) throw new Error('unsupported-matrix-row');
  if (matrixRevision !== (row.matrixRevision || MATRIX_REVISION))
    throw new Error('unsupported-matrix-revision');
  if (row.supportState !== 'supported') throw new Error(`wallet-only-matrix-row:${row.reason}`);
  if (row.packageFamily === 'arch')
    assert.strictEqual(sandboxClass, 'userns-only', 'arch-requires-userns-only');
  if (row.packageFamily === 'nix')
    assert.strictEqual(sandboxClass, 'userns-only', 'nix-requires-userns-only');
  if (!SANDBOX_CLASSES.has(sandboxClass)) throw new Error('unsupported-sandbox-class');
  if (!/^[a-z0-9][a-z0-9-]*$/.test(cluster)) throw new Error('invalid-cluster');
  // NixOS installs to the Nix store (path varies by derivation hash); skip the
  // fixed-root assertion that applies to deb/rpm/arch packages.
  if (row.packageFamily !== 'nix')
    assert.strictEqual(
      installRoot,
      `/opt/daedalus/${cluster}`,
      'unexpected-install-root'
    );
  return {
    ...row,
    matrixRow,
    matrixRevision,
    sandboxClass,
    cluster,
  };
}

function parseOsRelease(value) {
  const fields = {};
  for (const line of value.split('\n')) {
    const match = line.match(/^([A-Z_]+)=(.*)$/);
    if (!match) continue;
    let fieldValue = match[2];
    if (
      (fieldValue.startsWith('"') && fieldValue.endsWith('"')) ||
      (fieldValue.startsWith("'") && fieldValue.endsWith("'"))
    ) {
      fieldValue = fieldValue.slice(1, -1);
    }
    fields[match[1]] = fieldValue;
  }
  return {
    id: fields.ID,
    versionId: fields.VERSION_ID || fields.IMAGE_VERSION,
    buildId: fields.BUILD_ID,
    imageId: fields.IMAGE_ID,
    imageVersion: fields.IMAGE_VERSION,
  };
}

function assertHostContract(contract, host) {
  assert.strictEqual(process.arch, 'x64', 'x86-64-required');
  assert.strictEqual(host.id, contract.distributionId, 'matrix-distribution-mismatch');
  if (host.versionId !== undefined || !contract.fingerprint) {
    assert(contract.versionPattern.test(host.versionId || ''), 'matrix-version-mismatch');
  }
  if (contract.fingerprint) {
    for (const [name, value] of Object.entries(contract.fingerprint)) {
      assert.strictEqual(host[name], value, `matrix-${name}-mismatch`);
    }
  }
}

function readHostContract(contract) {
  const host = {
    ...parseOsRelease(fs.readFileSync('/etc/os-release', 'utf8')),
    kernelRelease: os.release(),
  };
  assertHostContract(contract, host);
  return { ...host, distributionId: host.id, architecture: 'x86_64' };
}

function exactPackagePaths(installRoot, cluster, policy) {
  const paths = {
    launcher: path.join(installRoot, 'bin', 'daedalus'),
    frontend: path.join(installRoot, 'libexec', 'daedalus-frontend'),
    wrapper: path.join(installRoot, 'libexec', 'electron'),
    electron: path.join(
      installRoot,
      'libexec',
      'bundle-electron',
      'lib',
      'electron',
      'electron'
    ),
    chromeSandbox: path.join(
      installRoot,
      'libexec',
      'bundle-electron',
      'lib',
      'electron',
      'chrome-sandbox'
    ),
    identityManifest: path.join(
      installRoot,
      'share',
      'daedalus-sandbox-identity.json'
    ),
  };
  if (policy === 'apparmor') {
    paths.policyAsset = `/etc/apparmor.d/opt.daedalus.${cluster}.electron`;
  } else if (policy === 'selinux') {
    paths.policyAsset = `/usr/share/selinux/packages/daedalus-${cluster}.cil`;
  }
  return paths;
}

function loadIdentityManifest(manifestPath, contract) {
  assertRootFile(manifestPath, '0644');
  const manifest = JSON.parse(fs.readFileSync(manifestPath, 'utf8'));
  assertIdentityManifest(manifest, contract);
  return manifest;
}

function assertIdentityManifest(manifest, contract) {
  assert.strictEqual(manifest.schemaVersion, IDENTITY_SCHEMA_VERSION, 'identity-schema-version');
  assert.strictEqual(manifest.packageFamily, contract.packageFamily, 'identity-package-family');
  assert.strictEqual(
    manifest.matrixRevision,
    contract.matrixRevision || MATRIX_REVISION,
    'identity-matrix-revision'
  );
  assert.strictEqual(manifest.matrixRow, contract.matrixRow, 'identity-matrix-row');
  assert.strictEqual(manifest.supportState, contract.supportState, 'identity-support-state');
  assert.strictEqual(manifest.reason, contract.reason, 'identity-support-reason');
  assert.strictEqual(
    manifest.distribution && manifest.distribution.id,
    contract.distributionId,
    'identity-distribution'
  );
  assert(
    contract.versionPattern.test(manifest.distribution.versionId || ''),
    'identity-distribution-version'
  );
  for (const [name, value] of Object.entries(
    contract.manifestFingerprint || contract.fingerprint || {}
  )) {
    assert.strictEqual(
      manifest.distribution[name],
      value,
      `identity-distribution-${name}`
    );
  }
  assert.strictEqual(manifest.cluster, contract.cluster, 'identity-cluster');
  assert.strictEqual(manifest.policy && manifest.policy.kind, contract.policy, 'identity-policy');
  assert.strictEqual(manifest.helper && manifest.helper.mode, contract.helperMode, 'identity-helper-mode');
  if (contract.policy === 'apparmor') {
    assert.strictEqual(
      manifest.policy.semanticIdentity,
      contract.apparmorSemanticIdentity,
      'identity-apparmor-semantics'
    );
    assert.strictEqual(
      manifest.policy.loadedProfileSuffix,
      APPARMOR_LOADED_PROFILE_SUFFIX,
      'identity-apparmor-loaded-profile-suffix'
    );
  }
  assert(manifest.files && typeof manifest.files === 'object', 'identity-files');
}

function verifyPackageFiles(
  paths,
  manifest,
  sandboxClass,
  metadataReader = assertRootFile,
  directoryReader = assertRootDirectory
) {
  const modes = {
    launcher: '0755',
    frontend: '0755',
    wrapper: '0755',
    electron: '0755',
    chromeSandbox: manifest.helper.mode,
    policyAsset: '0644',
  };
  const result = {};
  for (const [name, filePath] of Object.entries(paths)) {
    if (name === 'identityManifest' || (name === 'policyAsset' && !filePath)) continue;
    const metadata = metadataReader(filePath, modes[name]);
    const expected = manifest.files[name];
    assertIdentityHash(expected, metadata, name);
    result[name] = publicFileMetadata(metadata);
  }
  result.identityManifest = publicFileMetadata(
    metadataReader(paths.identityManifest, '0644')
  );
  const directories = {};
  for (const directoryPath of new Set([
    path.dirname(path.dirname(paths.launcher)),
    path.dirname(paths.launcher),
    path.dirname(paths.frontend),
    path.dirname(path.dirname(paths.electron)),
    path.dirname(path.dirname(path.dirname(paths.electron))),
    path.dirname(path.dirname(path.dirname(path.dirname(paths.electron)))),
    path.dirname(paths.electron),
    path.dirname(paths.identityManifest),
  ])) {
    directories[directoryPath] = directoryReader(directoryPath);
  }
  return { files: result, directories };
}

function expectedPolicy(contract, executablePath) {
  if (contract.policy === 'apparmor') {
    return {
      kind: 'apparmor',
      assetPath: `/etc/apparmor.d/opt.daedalus.${contract.cluster}.electron`,
      processLabel: executablePath,
      semanticIdentity: contract.apparmorSemanticIdentity,
    };
  }
  if (contract.policy === 'selinux') {
    return {
      kind: 'selinux',
      assetPath: `/usr/share/selinux/packages/daedalus-${contract.cluster}.cil`,
    };
  }
  return { kind: 'none' };
}

function assertHelperForClass(helperMetadata, sandboxClass) {
  assert.strictEqual(helperMetadata.uid, 0, 'helper-owner');
  assert.strictEqual(helperMetadata.gid, 0, 'helper-group');
  const expectedMode = sandboxClass === 'userns-only' ? '0755' : '4755';
  assert.strictEqual(helperMetadata.mode, expectedMode, 'helper-mode');
}

function assertPolicyLabel(expectedLabel, rendererLabel, kind) {
  assert(rendererLabel, 'missing-renderer-security-label');
  assert.strictEqual(rendererLabel, expectedLabel, `${kind}-label-mismatch`);
}

function parseSelinuxContext(value) {
  assertSafePolicyValue(value);
  const match = value.match(/^([^:]+):([^:]+):([^:]+):(.+)$/);
  assert(match, 'selinux-context-format');
  return { user: match[1], role: match[2], type: match[3], range: match[4] };
}

function assertSelinuxProcessContext(identity, sourceLabel, observedLabel, processKind) {
  const source = parseSelinuxContext(sourceLabel);
  const observed = parseSelinuxContext(observedLabel);
  const roleName = `${processKind}ProcessRole`;
  const typeName = `${processKind}ProcessType`;
  assert.strictEqual(source.role, identity.transitionSourceRole, 'selinux-source-role');
  assert.strictEqual(source.type, identity.transitionSourceType, 'selinux-source-type');
  assert.deepStrictEqual(
    observed,
    {
      user: source.user,
      role: identity[roleName],
      type: identity[typeName],
      range: source.range,
    },
    `selinux-${processKind}-context`
  );
  return { source, observed };
}

function assertSafePolicyValue(value) {
  assert(typeof value === 'string' && /^[A-Za-z0-9_.:/()-]+$/.test(value), 'policy-value');
}

function assertPolicyHostEvidence(expected, host) {
  if (expected.kind === 'apparmor') {
    assert.strictEqual(host.semanticCompatible, true, 'apparmor-semantic-incompatible');
    assert(host.parserVersion, 'missing-apparmor-parser-version');
  } else if (expected.kind === 'selinux') {
    assert.strictEqual(host.state, 'enforcing', 'selinux-not-enforcing');
    assert.strictEqual(
      host.electronFileContext,
      expected.electronFileContext,
      'selinux-electron-context-mismatch'
    );
    assert.strictEqual(
      host.helperFileContext,
      expected.helperFileContext,
      'selinux-helper-context-mismatch'
    );
    assert.strictEqual(host.effectiveFileContexts, true, 'selinux-policy-not-effective');
  }
}

function verifySelinuxEffectivePolicy(identity, paths, run = runHostCommand) {
  const enforce = run('getenforce', []);
  assert.strictEqual(enforce.status, 0, 'selinux-state-read');
  assert.strictEqual(enforce.stdout.trim(), 'Enforcing', 'selinux-not-enforcing');
  for (const [name, filePath, expectedContext] of [
    ['electron', paths.electron, identity.electronFileContext],
    ['helper', paths.chromeSandbox, identity.helperFileContext],
  ]) {
    const match = run('matchpathcon', ['-n', filePath]);
    assert.strictEqual(match.status, 0, `selinux-${name}-expected-context-read`);
    assert.strictEqual(
      match.stdout.trim(),
      expectedContext,
      `selinux-${name}-effective-context`
    );
  }
  return { state: 'enforcing', effectiveFileContexts: true };
}

function observeUsernsAvailability(run = runHostCommand) {
  const result = run('unshare', ['-Ur', 'true']);
  if (result.error && result.error.code === 'ETIMEDOUT') throw new Error('userns-check-timeout');
  if (result.error && result.error.code === 'ENOENT') throw new Error('userns-check-unavailable');
  return { available: result.status === 0, check: 'unshare-Ur' };
}

function assertSandboxClassPrerequisites(sandboxClass, userns) {
  if (sandboxClass === 'suid-only') {
    assert.strictEqual(userns.available, false, 'userns-must-be-unavailable');
  } else {
    assert.strictEqual(userns.available, true, 'userns-required');
  }
}

function hasApprovedSandboxRoute({ suid, userns }) {
  return suid === true || userns === true;
}

function readSelinuxContext(filePath) {
  const result = runHostCommand('stat', ['--format=%C', filePath]);
  assert.strictEqual(result.status, 0, 'selinux-context-read');
  const value = result.stdout.trim();
  assertSafePolicyValue(value);
  return value;
}

function collectPolicyEvidence(
  contract,
  paths,
  manifest,
  rendererEvidence,
  dependencies = {}
) {
  const readFileSync = dependencies.readFileSync || fs.readFileSync;
  const run = dependencies.run || runHostCommand;
  const readContext = dependencies.readContext || readSelinuxContext;
  const expected = expectedPolicy(contract, paths.electron);
  if (expected.kind === 'none') return { kind: 'none', required: false };
  const identity = manifest.policy;
  assert(identity && identity.kind === expected.kind, 'identity-policy-kind');
  if (expected.kind === 'apparmor') {
    assertSafePolicyValue(identity.processLabel);
    const expectedRendererLabel = `${identity.processLabel}${identity.loadedProfileSuffix}`;
    assertPolicyLabel(expectedRendererLabel, rendererEvidence.securityLabel, expected.kind);
    assert.strictEqual(identity.processLabel, paths.electron, 'apparmor-attachment-path');
    assert.strictEqual(identity.requiredAbi, '4.0', 'apparmor-required-abi');
    assert.deepStrictEqual(identity.requiredFlags, ['default_allow'], 'apparmor-required-flags');
    assert.deepStrictEqual(identity.requiredRules, ['userns'], 'apparmor-required-rules');
    assert.strictEqual(identity.semanticIdentity, contract.apparmorSemanticIdentity, 'apparmor-semantic-identity');
    const enabled = readFileSync('/sys/module/apparmor/parameters/enabled', 'utf8').trim();
    assert(/^Y$/i.test(enabled), 'apparmor-disabled');
    const profiles = readFileSync('/sys/kernel/security/apparmor/profiles', 'utf8');
    assert(
      profiles.split('\n').some(
        (line) => line === `${identity.processLabel}${identity.loadedProfileSuffix}`
      ),
      'apparmor-profile-not-loaded'
    );
    const parser = run('apparmor_parser', ['--version']);
    assert.strictEqual(parser.status, 0, 'apparmor-parser-unavailable');
    const parserMatch = `${parser.stdout}\n${parser.stderr}`.match(/version\s+([0-9.]+)/i);
    assert(parserMatch, 'apparmor-parser-version');
    const profileSource = readFileSync(paths.policyAsset, 'utf8');
    assert(/abi\s+<abi\/4\.0>/.test(profileSource), 'apparmor-profile-abi');
    assert(/flags=\(default_allow\)/.test(profileSource), 'apparmor-profile-default-allow');
    assert(/\buserns\s*,/.test(profileSource), 'apparmor-profile-userns');
    const parserAcceptance = run('apparmor_parser', [
      '--skip-kernel-load',
      paths.policyAsset,
    ]);
    assert.strictEqual(parserAcceptance.status, 0, 'apparmor-profile-parse');
    return {
      kind: 'apparmor',
      required: true,
      semanticCompatible: true,
      semanticIdentity: identity.semanticIdentity,
      parserVersion: parserMatch[1],
      profileIdentity: identity.processLabel,
      rendererLabelMatches: true,
      profileLoaded: true,
      profileHashBound: true,
      profileParserAccepted: true,
    };
  }

  for (const name of [
    'electronFileContext',
    'helperFileContext',
    'module',
    'mainProcessRole',
    'mainProcessType',
    'rendererProcessRole',
    'rendererProcessType',
  ]) {
    assertSafePolicyValue(identity[name]);
  }
  assert(dependencies.mainEvidence, 'missing-selinux-main-evidence');
  const mainContext = assertSelinuxProcessContext(
    identity,
    dependencies.mainEvidence.securityLabel,
    dependencies.mainEvidence.securityLabel,
    'main'
  );
  const rendererContext = assertSelinuxProcessContext(
    identity,
    dependencies.mainEvidence.securityLabel,
    rendererEvidence.securityLabel,
    'renderer'
  );
  const effectivePolicy = verifySelinuxEffectivePolicy(identity, paths, run);
  const host = {
    state: effectivePolicy.state,
    electronFileContext: readContext(paths.electron),
    helperFileContext: readContext(paths.chromeSandbox),
    effectiveFileContexts: effectivePolicy.effectiveFileContexts,
  };
  assertPolicyHostEvidence({ kind: 'selinux', ...identity }, host);
  return {
    kind: 'selinux',
    required: true,
    state: host.state,
    mainContext: mainContext.observed,
    rendererContext: rendererContext.observed,
    electronFileContext: identity.electronFileContext,
    helperFileContext: identity.helperFileContext,
    module: identity.module,
    rendererLabelMatches: true,
    electronFileContextMatches: true,
    helperFileContextMatches: true,
    effectiveFileContexts: true,
  };
}

function normalizePolicyEvidence(policy, privacy) {
  if (policy.kind === 'apparmor') {
    return {
      ...policy,
      profileIdentity: normalizedPath(policy.profileIdentity, privacy),
    };
  }
  assertNoSensitiveContent(JSON.stringify(policy), privacy);
  return policy;
}

async function waitForRenderer(window, timeoutMs) {
  return new Promise((resolve, reject) => {
    const timeout = setTimeout(() => reject(new Error('timeout')), timeoutMs);
    const finish = (callback) => (...values) => {
      clearTimeout(timeout);
      callback(...values);
    };
    window.webContents.once('did-finish-load', finish(resolve));
    window.webContents.once(
      'render-process-gone',
      finish((_event, details) => reject(new Error(`renderer-exited:${details.reason}`)))
    );
  });
}

async function runElectronProbe({ transitionOnly = false, exitOnly = false } = {}) {
  assert(!exitOnly || transitionOnly, 'exit-only-requires-transition-only');
  if (process.platform !== 'linux') throw new Error('linux-required');
  if (process.env.ELECTRON_DISABLE_SANDBOX !== undefined) {
    throw new Error('sandbox-disabling-environment');
  }

  const { app, BrowserWindow, session } = require('electron');
  debug('electron-loaded');
  const executablePath = canonicalPath(process.execPath);
  const installRoot = canonicalPath(inferInstallRoot(executablePath));
  const contract = readContractSelection(installRoot);
  const host = normalizedHostEnvironment(readHostContract(contract));
  const packagePaths = exactPackagePaths(installRoot, contract.cluster, contract.policy);
  assert.strictEqual(executablePath, packagePaths.electron, 'unexpected-electron-path');
  const identityManifest = loadIdentityManifest(packagePaths.identityManifest, contract);
  const probeRoot = canonicalPath(__dirname);
  const home = canonicalPath(os.homedir());
  const profileRoot = createProfileRoot();
  const roots = {
    installRoot,
    probeRoot,
    profileRoot: canonicalPath(profileRoot),
    home,
  };
  const privacy = privacyContext(roots);
  app.setPath('userData', profileRoot);
  app.disableHardwareAcceleration();

  let window;
  let probeSession;
  let failure;
  let completedEvidence;
  let rendererGone = false;
  const rendererGoneHandler = () => {
    rendererGone = true;
  };
  const keepAliveDuringCleanup = () => undefined;
  app.on('window-all-closed', keepAliveDuringCleanup);
  const deadline = setTimeout(() => {
    if (!exitOnly) {
      process.stderr.write('sandbox-probe-failed:timeout:process-deadline\n');
    }
    try {
      if (window && !window.isDestroyed()) window.destroy();
    } catch (_error) {
      // The process exits fail-closed below even if Electron cleanup is unavailable.
    }
    // Do not start unbounded synchronous cleanup after the hard deadline.
    // The disposable profile remains host-local for operator cleanup.
    app.exit(1);
  }, PROBE_TIMEOUT_MS);
  try {
    await app.whenReady();
    debug('app-ready');
    const partition = `daedalus-sandbox-probe-${crypto.randomBytes(16).toString('hex')}`;
    probeSession = session.fromPartition(partition, { cache: false });
    debug('session-created');
    window = new BrowserWindow({
      show: false,
      webPreferences: {
        allowRunningInsecureContent: false,
        contextIsolation: true,
        nodeIntegration: false,
        nodeIntegrationInSubFrames: false,
        nodeIntegrationInWorker: false,
        partition,
        plugins: false,
        sandbox: true,
        webSecurity: true,
        webviewTag: false,
      },
    });
    window.webContents.on('render-process-gone', rendererGoneHandler);
    debug('window-created');

    const rendererReady = waitForRenderer(window, PROBE_TIMEOUT_MS);
    await Promise.all([
      window.loadURL(
        'data:text/html;charset=utf-8,%3C!doctype%20html%3E%3Ctitle%3Esandbox-probe%3C/title%3E'
      ),
      rendererReady,
    ]);
    debug('renderer-loaded');

    const rendererPid = window.webContents.getOSProcessId();
    assert(Number.isSafeInteger(rendererPid) && rendererPid > 1, 'invalid-renderer-pid');
    let mainEvidence = readProcEvidence(process.pid, privacy);
    let rendererEvidence = readProcEvidence(rendererPid, privacy);
    assert.strictEqual(Number(mainEvidence.status.Pid), process.pid, 'main-proc-pid');
    assert.strictEqual(Number(rendererEvidence.status.Pid), rendererPid, 'renderer-proc-pid');
    if (process.env.DAEDALUS_PROBE_DEBUG === '1' && !exitOnly) {
      const typeArguments = rendererEvidence.argv.filter((argument) =>
        argument.startsWith('--type')
      );
      process.stderr.write(
        `sandbox-probe-renderer-type:${JSON.stringify(typeArguments)}\n`
      );
    }
    if (transitionOnly) {
      assert.strictEqual(contract.policy, 'selinux', 'transition-only-requires-selinux');
      const moduleEvidence = verifySelinuxEffectivePolicy(
        identityManifest.policy,
        packagePaths
      );
      assert(!hasForbiddenSwitch(mainEvidence.argv), 'forbidden-main-switch');
      assert(!hasForbiddenSwitch(rendererEvidence.argv), 'forbidden-renderer-switch');
      const sourceLabel = requiredEnvironment('DAEDALUS_PROBE_SELINUX_SOURCE_CONTEXT');
      if (process.env.DAEDALUS_PROBE_DEBUG === '1' && !exitOnly) {
        process.stderr.write(
          `sandbox-probe-selinux-labels:${JSON.stringify({
            main: mainEvidence.securityLabel,
            renderer: rendererEvidence.securityLabel,
          })}\n`
        );
      }
      const mainTransition = assertSelinuxProcessContext(
        identityManifest.policy,
        sourceLabel,
        mainEvidence.securityLabel,
        'main'
      );
      const rendererTransition = assertSelinuxProcessContext(
        identityManifest.policy,
        sourceLabel,
        rendererEvidence.securityLabel,
        'renderer'
      );
      assert(!rendererGone, 'renderer-exited-during-collection');
      assert(!window.webContents.isDestroyed(), 'renderer-destroyed-during-collection');
      assert.strictEqual(
        window.webContents.getOSProcessId(),
        rendererPid,
        'renderer-pid-changed'
      );
      ({ main: mainEvidence, renderer: rendererEvidence } = await assertRendererAfterLifecycleYield(
        mainEvidence, rendererEvidence, rendererPid, window.webContents, () => rendererGone,
        (main, renderer) => {
          assert(!hasForbiddenSwitch(main.argv), 'forbidden-main-switch');
          assert(!hasForbiddenSwitch(renderer.argv), 'forbidden-renderer-switch');
          assertSelinuxProcessContext(identityManifest.policy, sourceLabel, main.securityLabel, 'main');
          assertSelinuxProcessContext(identityManifest.policy, sourceLabel, renderer.securityLabel, 'renderer');
        }, privacy
      ));
      const packageIdentity = verifyPackageFiles(
        packagePaths,
        identityManifest,
        contract.sandboxClass
      );
      const electronFileContext = readSelinuxContext(packagePaths.electron);
      const helperFileContext = readSelinuxContext(packagePaths.chromeSandbox);
      assert.strictEqual(
        electronFileContext,
        identityManifest.policy.electronFileContext,
        'selinux-electron-context-mismatch'
      );
      assert.strictEqual(
        helperFileContext,
        identityManifest.policy.helperFileContext,
        'selinux-helper-context-mismatch'
      );
      completedEvidence = createEvidenceEnvelope({
        result: 'pass',
        matrix: {
          revision: contract.matrixRevision,
          row: contract.matrixRow,
          packageFamily: contract.packageFamily,
          policy: contract.policy,
          mode: 'transition-only',
          snapshotFingerprint: contract.fingerprint || null,
        },
        host,
        paths: Object.fromEntries(
          Object.entries(packagePaths).map(([name, filePath]) => [
            name,
            normalizedPath(filePath, privacy),
          ])
        ),
        files: packageIdentity.files,
        policy: {
          kind: 'selinux',
          ...moduleEvidence,
          module: identityManifest.policy.module,
          priority: identityManifest.policy.priority,
          configuredSemanticVersion: identityManifest.policy.semanticVersion,
          configuredSourceCilSha256: identityManifest.policy.sourceCilSha256,
          sourceContext: mainTransition.source,
          mainContext: mainTransition.observed,
          rendererContext: rendererTransition.observed,
          electronFileContext,
          helperFileContext,
        },
        main: normalizeProcessEvidence(mainEvidence, mainEvidence, '<MAIN_PID>', privacy, true),
        renderer: normalizeProcessEvidence(
          rendererEvidence,
          mainEvidence,
          '<RENDERER_PID>',
          privacy,
          true
        ),
        assertions: {
          noSandboxBypass: true,
          exactRendererPid: true,
          selinuxProcessContexts: true,
          exactFileContexts: true,
          containmentChecked: false,
        },
        diagnostics: {
          sanitized: true,
          rawHostDataExported: false,
          stderrSummaryRequired: true,
        },
      });
      debug('transition-verified');
    } else {
      assertRendererEvidence(mainEvidence, rendererEvidence, contract.sandboxClass);
      debug('renderer-verified');

      const userns = observeUsernsAvailability();
    assertSandboxClassPrerequisites(contract.sandboxClass, userns);
    const packageIdentity = verifyPackageFiles(
      packagePaths,
      identityManifest,
      contract.sandboxClass
    );
    assert(
      hasApprovedSandboxRoute({
        suid: packageIdentity.files.chromeSandbox.mode === '4755',
        userns: userns.available,
      }),
      'no-approved-sandbox-route'
    );
    assertHelperForClass(
      {
        uid: packageIdentity.files.chromeSandbox.owner === 'root' ? 0 : -1,
        gid: packageIdentity.files.chromeSandbox.group === 'root' ? 0 : -1,
        mode: packageIdentity.files.chromeSandbox.mode,
      },
      contract.sandboxClass
    );
    let policy = collectPolicyEvidence(
      contract,
      packagePaths,
      identityManifest,
      rendererEvidence,
      { mainEvidence }
    );
    assert(!rendererGone, 'renderer-exited-during-collection');
    assert(!window.webContents.isDestroyed(), 'renderer-destroyed-during-collection');
    assert.strictEqual(
      window.webContents.getOSProcessId(),
      rendererPid,
      'renderer-pid-changed'
    );
    ({ main: mainEvidence, renderer: rendererEvidence } = await assertRendererAfterLifecycleYield(
      mainEvidence, rendererEvidence, rendererPid, window.webContents, () => rendererGone,
      (main, renderer) => {
        assertRendererEvidence(main, renderer, contract.sandboxClass);
        policy = collectPolicyEvidence(
          contract, packagePaths, identityManifest, renderer, { mainEvidence: main }
        );
      }, privacy
    ));
    const normalizedMain = normalizeProcessEvidence(
      mainEvidence,
      mainEvidence,
      '<MAIN_PID>',
      privacy
    );
    const normalizedRenderer = normalizeProcessEvidence(
      rendererEvidence,
      mainEvidence,
      '<RENDERER_PID>',
      privacy,
      contract.policy !== 'none'
    );
    const paths = Object.fromEntries(
      Object.entries(packagePaths).map(([name, filePath]) => [
        name,
        normalizedPath(filePath, privacy),
      ])
    );
    completedEvidence = createEvidenceEnvelope({
      result: 'pass',
      matrix: {
        revision: contract.matrixRevision,
        row: contract.matrixRow,
        packageFamily: contract.packageFamily,
        sandboxClass: contract.sandboxClass,
        policy: contract.policy,
        userns,
        snapshotFingerprint: contract.fingerprint || null,
      },
      host,
      versions: {
        electron: process.versions.electron,
        chrome: process.versions.chrome,
      },
      paths,
      files: {
        ...packageIdentity.files,
        probe: publicFileMetadata(fileMetadata(__filename)),
      },
      directories: Object.entries(packageIdentity.directories).map(
        ([directoryPath, metadata]) => ({
          path: normalizedPath(directoryPath, privacy),
          ...metadata,
        })
      ),
      policy: normalizePolicyEvidence(policy, privacy),
      main: normalizedMain,
      renderer: normalizedRenderer,
      assertions: {
        noSandboxBypass: true,
        exactRendererPid: true,
        rendererInstanceStable: true,
        chromiumProcessTypeRecorded: true,
        noNewPrivs: true,
        seccomp: true,
        zeroEffectiveCapabilities: true,
        separatePidNamespace: true,
        mountNamespaceRecorded: true,
        separateUserNamespace: contract.sandboxClass !== 'suid-only',
        separateUserAndGroupMaps: contract.sandboxClass !== 'suid-only',
        helperContract: true,
        policyContract: true,
      },
      failure: null,
      diagnostics: {
        sanitized: true,
        rawHostDataExported: false,
        stderrSummaryRequired: true,
      },
      });
    }
  } catch (error) {
    failure = error;
    process.exitCode = 1;
  } finally {
    if (window && !window.isDestroyed()) {
      window.webContents.removeListener('render-process-gone', rendererGoneHandler);
    }
    try {
      if (window && !window.isDestroyed()) window.destroy();
    } catch (_error) {
      // Continue bounded cleanup and quit.
    }
    const cleanupFailure = await performCleanup(probeSession, profileRoot);
    if (!failure && cleanupFailure) failure = cleanupFailure;
  }
  app.removeListener('window-all-closed', keepAliveDuringCleanup);
  clearTimeout(deadline);
  const matrixContext = {
    revision: contract.matrixRevision,
    row: contract.matrixRow,
    packageFamily: contract.packageFamily,
    sandboxClass: contract.sandboxClass,
    policy: contract.policy,
  };
  const finalEvidence = failure
    ? createFailureEvidence(failure, matrixContext, completedEvidence || {})
    : completedEvidence;
  assert(finalEvidence, 'missing-final-evidence');
  assertNoSensitiveContent(JSON.stringify(finalEvidence), privacy);
  if (!exitOnly) process.stdout.write(`${JSON.stringify(finalEvidence, null, 2)}\n`);
  if (failure && !exitOnly) {
    process.stderr.write(
      `sandbox-probe-failed:${finalEvidence.failure.category}:${finalEvidence.failure.code}\n`
    );
  }
  app.exit(failure ? 1 : 0);
}

function rootsFromEnvironment() {
  const roots = {
    installRoot: process.env.DAEDALUS_PROBE_INSTALL_ROOT,
    probeRoot: process.env.DAEDALUS_PROBE_ROOT,
    profileRoot: process.env.DAEDALUS_PROBE_PROFILE_ROOT,
    home: process.env.DAEDALUS_PROBE_HOME || os.homedir(),
  };
  for (const [name] of ROOT_TOKENS) {
    if (!roots[name]) throw new Error(`missing-sanitizer-root:${name}`);
    roots[name] = path.resolve(roots[name]);
  }
  return roots;
}

function runStderrSanitizer(args) {
  const inputIndex = args.indexOf('--input');
  const exitCodeIndex = args.indexOf('--exit-code');
  const probeIndex = args.indexOf('--probe-json');
  if (inputIndex === -1 || !args[inputIndex + 1]) throw new Error('missing-input');
  if (exitCodeIndex === -1 || !args[exitCodeIndex + 1]) {
    throw new Error('missing-exit-code');
  }
  const roots = rootsFromEnvironment();
  const privacy = privacyContext(roots);
  const rawBytes = fs.readFileSync(args[inputIndex + 1]);
  const summary = summarizeStderr(rawBytes, Number(args[exitCodeIndex + 1]), privacy);
  let probeEvidence = createFailureEvidence(new Error('missing-probe-evidence'));
  if (probeIndex !== -1 && args[probeIndex + 1] && fs.existsSync(args[probeIndex + 1])) {
    probeEvidence = parseProbeEvidence(fs.readFileSync(args[probeIndex + 1], 'utf8'));
  }
  const evidence = mergeDiagnostics(probeEvidence, summary);
  assertNoSensitiveContent(JSON.stringify(evidence), privacy);
  process.stdout.write(`${JSON.stringify(evidence, null, 2)}\n`);
}

function parseProbeEvidence(value) {
  try {
    if (!value.trim()) throw new Error('empty');
    const evidence = JSON.parse(value);
    assert.strictEqual(evidence.schemaVersion, SCHEMA_VERSION, 'probe-schema-version');
    return evidence;
  } catch (_error) {
    return createFailureEvidence(new Error('missing-probe-evidence'));
  }
}

function mergeDiagnostics(probeEvidence, summary) {
  const failed = summary.exitCode !== 0 || probeEvidence.result === 'fail';
  return createEvidenceEnvelope({
    ...probeEvidence,
    result: failed ? 'fail' : 'pass',
    failure:
      probeEvidence.failure ||
      (failed
        ? { category: summary.category, code: `process-exit:${summary.exitCode}` }
        : null),
    diagnostics: {
      ...summary,
      sanitized: true,
      rawHostDataExported: false,
      stderrSummaryRequired: false,
    },
  });
}

async function runSelfTest() {
  const roots = {
    installRoot: '/home/alice/.daedalus/mainnet',
    probeRoot: '/work/daedalus/scripts/linux-chromium-sandbox-probe',
    profileRoot: '/tmp/daedalus-sandbox-probe-secret',
    home: '/home/alice',
  };
  const privacy = {
    roots,
    username: 'alice',
    hostname: 'build-host',
    environmentValues: ['outside-assignment-secret'],
  };
  assert.deepStrictEqual(
    sensitiveEnvironmentValues(
      {
        USER: 'daedalus',
        DAEDALUS_PROBE_MATRIX_ROW: 'fedora-43',
        DAEDALUS_PROBE_SELINUX_SOURCE_CONTEXT:
          'unconfined_u:unconfined_r:unconfined_t:s0-s0:c0.c1023',
        API_TOKEN: 'secret-token',
      },
      roots
    ),
    ['secret-token']
  );
  const argv = normalizeArgv(
    [
      `${roots.installRoot}/libexec/bundle-electron/lib/electron/electron`,
      `--user-data-dir=${roots.profileRoot}`,
      roots.probeRoot,
      '--type=renderer',
      '--crashpad-handler-pid=4242',
    ],
    privacy
  );
  assert.deepStrictEqual(argv, [
    '<INSTALL_ROOT>/libexec/bundle-electron/lib/electron/electron',
    '--user-data-dir=<PROFILE_ROOT>',
    '<PROBE_ROOT>',
    '--type=renderer',
    '--crashpad-handler-pid=<PID>',
  ]);

  const stderr = Buffer.from(
    `FATAL home=${roots.home} failed at {${roots.installRoot}/bin/daedalus} for ALICE on BUILD-HOST; see file://${roots.home}/secret and [/var/log/private]; token outside-assignment-secret\n`
  );
  const summary = summarizeStderr(stderr, 1, privacy);
  assert.strictEqual(summary.category, 'other');
  assert(summary.sanitizedExcerpt.includes('home=<ENV_VALUE>'));
  assert(summary.sanitizedExcerpt.includes('<INSTALL_ROOT>/bin/daedalus'));
  assert(summary.sanitizedExcerpt.includes('<USER>'));
  assert(summary.sanitizedExcerpt.includes('<HOST>'));
  assert(summary.sanitizedExcerpt.includes('<URL>'));
  assert(summary.sanitizedExcerpt.includes('<PATH_1>'));
  assert(summary.sanitizedExcerpt.includes('token <ENV_VALUE>'));
  assertNoSensitiveContent(summary.sanitizedExcerpt, privacy);

  assert.throws(
    () => assertNoSensitiveContent('[/unredacted/path]', privacy),
    /residual-absolute-path/
  );
  assert.throws(
    () => assertNoSensitiveContent('file:///unredacted/path', privacy),
    /residual-url/
  );
  assert.strictEqual(categorizeStderr('Failed to move to new namespace', 1), 'namespace-denied');
  assert(hasForbiddenSwitch(['--no-sandbox']));

  const mainEvidence = {
    argv: ['<INSTALL_ROOT>/libexec/electron'],
    pid: 41,
    startTime: '12344',
    status: { Pid: '41' },
    namespaces: { user: 'user:[1]', pid: 'pid:[1]', mnt: 'mnt:[1]', net: 'net:[1]' },
    uidMap: '0 0 4294967295',
    gidMap: '0 0 4294967295',
    securityLabel: 'unconfined',
  };
  const rendererEvidence = {
    argv: ['--type=renderer'],
    pid: 42,
    startTime: '12345',
    status: {
      Pid: '42',
      NoNewPrivs: '1',
      Seccomp: '2',
      Seccomp_filters: '1',
      CapEff: '0000000000000000',
      CapPrm: '0000000000000000',
      CapInh: '0000000000000000',
    },
    namespaces: { user: 'user:[2]', pid: 'pid:[2]', mnt: 'mnt:[2]', net: 'net:[2]' },
    uidMap: '0 1000 1',
    gidMap: '0 1000 1',
    securityLabel:
      '/opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/electron (unconfined)',
  };
  const mainRoot = {
    device: '1', inode: '2', filesystemType: '61267', isDirectory: true,
    empty: false, deadProcRoot: false, chromiumFdinfoRoot: false, cwdMatchesRoot: false,
  };
  const confinedRoot = {
    device: '3', inode: '4', filesystemType: '40864', isDirectory: true,
    empty: true, deadProcRoot: false, chromiumFdinfoRoot: true, cwdMatchesRoot: true,
  };
  const sharedMain = { ...mainEvidence, filesystem: mainRoot };
  const sharedRenderer = {
    ...rendererEvidence,
    namespaces: { ...rendererEvidence.namespaces, mnt: mainEvidence.namespaces.mnt },
    filesystem: confinedRoot,
  };
  for (const sandboxClass of SANDBOX_CLASSES) {
    assertRendererEvidence(sharedMain, sharedRenderer, sandboxClass);
    assertRendererEvidence(sharedMain, {
      ...sharedRenderer, filesystem: { ...confinedRoot, empty: false, deadProcRoot: true },
    }, sandboxClass);
  }
  assertRendererEvidence(sharedMain, {
    ...sharedRenderer, uidMap: '  00\t01000  01 \n', gidMap: '\t0 1000 1\n',
  }, 'combined-unattributed');
  for (const filesystem of [
    undefined,
    { ...confinedRoot, device: 'not-numeric' },
    { ...confinedRoot, inode: '' },
    { ...confinedRoot, empty: false },
    { ...confinedRoot, filesystemType: '61267' },
    { ...confinedRoot, isDirectory: false },
    { ...confinedRoot, chromiumFdinfoRoot: false },
    { ...confinedRoot, cwdMatchesRoot: false },
    { ...confinedRoot, device: mainRoot.device, inode: mainRoot.inode },
  ]) {
    assert.throws(() => assertRendererEvidence(sharedMain, {
      ...sharedRenderer, filesystem,
    }, 'combined-unattributed'));
  }
  for (const main of [
    mainEvidence,
    { ...sharedMain, filesystem: { ...mainRoot, inode: 'bad' } },
    { ...sharedMain, filesystem: { ...mainRoot, isDirectory: false } },
  ]) {
    assert.throws(() => assertRendererEvidence(main, sharedRenderer, 'combined-unattributed'));
  }
  assert.throws(() => assertRendererEvidence(mainEvidence, {
    ...sharedRenderer, filesystem: undefined,
  }, 'combined-unattributed'));
  for (const field of ['device', 'inode']) {
    assertRendererEvidence(sharedMain, {
      ...sharedRenderer,
      filesystem: { ...confinedRoot, device: mainRoot.device, inode: mainRoot.inode, [field]: '99' },
    }, 'combined-unattributed');
  }
  for (const name of ['pid', 'user', 'net']) {
    assert.throws(() => assertRendererEvidence(sharedMain, {
      ...sharedRenderer, namespaces: { ...sharedRenderer.namespaces, [name]: sharedMain.namespaces[name] },
    }, 'combined-unattributed'));
  }
  for (const name of ['pid', 'user', 'net']) {
    assert.throws(() => assertRendererEvidence({
      ...sharedMain, namespaces: { ...sharedMain.namespaces, [name]: undefined },
    }, sharedRenderer, 'combined-unattributed'));
  }
  for (const field of ['CapEff', 'CapPrm', 'CapInh']) {
    for (const value of [undefined, '1', 'invalid']) {
      assert.throws(() => assertRendererEvidence(sharedMain, {
        ...sharedRenderer, status: { ...sharedRenderer.status, [field]: value },
      }, 'combined-unattributed'));
    }
  }
  for (const value of [
    '', '0 1000', '0 1000 0', '-1 1000 1', '0 4294967296 1',
    '4294967295 1000 2', '0 4294967295 2', '0 1000 1 extra',
    '0 1000 2', '0 1000 1\n1 1001 1', 'NaN 1000 1',
  ]) {
    for (const field of ['uidMap', 'gidMap']) {
      assert.throws(() => assertRendererEvidence(sharedMain, {
        ...sharedRenderer, [field]: value,
      }, 'combined-unattributed'));
    }
  }
  assert.deepStrictEqual(parseNamespaceMap('4294967295 4294967295 1'), [[4294967295, 4294967295, 1]]);
  assert.deepStrictEqual(parseNamespaceMap('0 0 1\n1 1 4294967295'), [[0, 0, 1], [1, 1, 4294967295]]);
  assert.throws(() => assertRendererEvidence(
    { ...sharedMain, uidMap: '00\t1000 01' }, sharedRenderer, 'combined-unattributed'
  ));
  assert.throws(() => assertRendererEvidence(
    { ...sharedMain, gidMap: '' }, sharedRenderer, 'combined-unattributed'
  ));
  // Optional filesystem failures do not disable the established separate-mount route.
  assertRendererEvidence({ ...mainEvidence, filesystem: undefined }, rendererEvidence, 'combined-unattributed');

  const errno = (code) => Object.assign(new Error(code), { code });
  const filesystemFixture = (options = {}) => {
    const closed = [];
    const opened = [];
    const stat = (dev = 3n, ino = 4n) => ({
      dev, ino, isDirectory: () => options.directory !== false,
    });
    let links = 0;
    const io = {
      readlinkSync: () => {
        if (options.readlinkError) throw errno(options.readlinkError);
        links += 1;
        return options.changedLink && links > 1 ? '/proc/99/fdinfo' :
          (options.rootLink || '/proc/12/task/13/fdinfo (deleted)');
      },
      openSync: (path, flags) => {
        assert.strictEqual(flags, 0x200000 | fs.constants.O_DIRECTORY);
        if (options.openError && path.endsWith(options.openError)) throw errno('EACCES');
        const fd = opened.length + 20;
        opened.push(fd);
        return fd;
      },
      fstatSync: (fd, flags) => {
        assert.deepStrictEqual(flags, { bigint: true });
        if (options.statError || (options.cwdStatError && fd === 21)) throw errno('EIO');
        return fd === 21 && options.cwdDifferent ? stat(3n, 5n) : stat();
      },
      statfsSync: (path, flags) => {
        assert.strictEqual(path, '/proc/self/fd/20');
        assert.deepStrictEqual(flags, { bigint: true });
        if (options.statfsError) throw errno('EIO');
        return { type: options.nonProcfs ? 61267n : 40864n };
      },
      readdirSync: (path) => {
        assert.strictEqual(path, '/proc/self/fd/20');
        if (options.readdirError) throw errno(options.readdirError);
        return options.nonempty ? ['not-exported'] : [];
      },
      closeSync: (fd) => {
        closed.push(fd);
        if (options.closeError === fd) throw errno('EIO');
      },
    };
    if (options.noStatfs) delete io.statfsSync;
    return { io, closed, opened };
  };
  const liveRootFixture = filesystemFixture();
  assert.deepStrictEqual(readFilesystemEvidence(42, liveRootFixture.io), confinedRoot);
  assert.deepStrictEqual(liveRootFixture.closed, [21, 20]);
  const deadRootFixture = filesystemFixture({ readdirError: 'ESRCH' });
  assert.deepStrictEqual(readFilesystemEvidence(42, deadRootFixture.io), {
    ...confinedRoot, empty: false, deadProcRoot: true,
  });
  assert.deepStrictEqual(deadRootFixture.closed, [21, 20]);
  for (const options of [
    { noStatfs: true }, { readlinkError: 'EACCES' }, { openError: '/root' },
    { openError: '/cwd' }, { statError: true }, { cwdStatError: true }, { statfsError: true },
    { readdirError: 'EACCES' }, { readdirError: 'ENOENT' }, { readdirError: 'EIO' },
    { readdirError: 'ESRCH', nonProcfs: true },
    { readdirError: 'ESRCH', directory: false },
    { readdirError: 'ESRCH', rootLink: '/tmp/empty' },
    { readdirError: 'ESRCH', rootLink: '/proc/0/fdinfo' },
    { readdirError: 'ESRCH', rootLink: '/proc/12/task/0/fdinfo' },
    { changedLink: true }, { closeError: 20 }, { closeError: 21 },
  ]) {
    const fixture = filesystemFixture(options);
    assert.strictEqual(readFilesystemEvidence(42, fixture.io), undefined);
    assert.deepStrictEqual(fixture.closed, [...fixture.opened].reverse());
  }
  for (const options of [{ nonempty: true }, { cwdDifferent: true }, { nonProcfs: true }, { directory: false }]) {
    const fixture = filesystemFixture(options);
    const filesystem = readFilesystemEvidence(42, fixture.io);
    assert(filesystem);
    assert.throws(() => assertRendererEvidence(sharedMain, {
      ...sharedRenderer, filesystem,
    }, 'combined-unattributed'));
    assert.deepStrictEqual(fixture.closed, [21, 20]);
  }
  const procFixture = (options = {}) => {
    const filesystem = filesystemFixture();
    const status = [
      'Pid:\t42', 'NoNewPrivs:\t1', 'Seccomp:\t2', 'Seccomp_filters:\t1',
      'CapEff:\t0000', 'CapPrm:\t0000', 'CapInh:\t0000',
    ].join('\n');
    const processStat = (start) =>
      `42 (electron renderer) S 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 ${start} 20`;
    let statusReads = 0;
    let statReads = 0;
    const io = {
      ...filesystem.io,
      readFileSync: (path) => {
        if (options.readError && path.endsWith(options.readError)) throw errno('EACCES');
        if (path.endsWith('/status')) {
          statusReads += 1;
          return options.statusChanged && statusReads > 1 ? status.replace('Seccomp:\t2', 'Seccomp:\t0') : status;
        }
        if (path.endsWith('/stat')) {
          statReads += 1;
          return processStat(options.startChanged && statReads > 1 ? '12346' : '12345');
        }
        if (path.endsWith('/cmdline')) return Buffer.from('--type=renderer\0');
        if (path.endsWith('/uid_map') || path.endsWith('/gid_map')) return '0 1000 1';
        if (path.endsWith('/attr/current')) return 'unconfined';
        throw new Error('unexpected-fixture-read');
      },
      readlinkSync: (path) => {
        if (options.namespaceError && path.includes('/ns/')) throw errno('EACCES');
        if (path.includes('/ns/')) return `${path.split('/').pop()}:[2]`;
        return filesystem.io.readlinkSync(path);
      },
    };
    return { ...filesystem, io };
  };
  const completeProc = readProcEvidence(42, privacy, procFixture().io);
  assert.strictEqual(completeProc.status.CapPrm, '0000');
  assert.strictEqual(completeProc.namespaces.net, 'net:[2]');
  assert.deepStrictEqual(completeProc.filesystem, confinedRoot);
  for (const options of [
    { readError: '/status' }, { readError: '/stat' }, { readError: '/cmdline' },
    { readError: '/uid_map' }, { readError: '/gid_map' }, { readError: '/attr/current' },
    { namespaceError: true }, { statusChanged: true }, { startChanged: true },
  ]) {
    const fixture = procFixture(options);
    assert.throws(() => readProcEvidence(42, privacy, fixture.io));
    assert.deepStrictEqual(fixture.closed, [...fixture.opened].reverse());
  }
  assertRendererEvidence(mainEvidence, rendererEvidence, 'userns-only');
  assertRendererEvidence(mainEvidence, rendererEvidence, 'combined-unattributed');
  assert.strictEqual(
    parseProcessStartTime(
      '42 (electron renderer) S 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 12345 20'
    ),
    '12345'
  );
  assertRendererInstance(
    { status: { Pid: '42' }, startTime: '12345' },
    { status: { Pid: '42' }, startTime: '12345' }
  );
  assert.throws(
    () =>
      assertRendererInstance(
        { status: { Pid: '42' }, startTime: '12345' },
        { status: { Pid: '42' }, startTime: '12346' }
      ),
    /renderer-instance-replaced/
  );
  const fixtureWebContents = {
    isDestroyed: () => false,
    getOSProcessId: () => 42,
  };
  const validateFixture = (main, renderer) => assertRendererEvidence(main, renderer, 'combined-unattributed');
  const finalRenderer = { ...rendererEvidence };
  const readPids = [];
  const finalSamples = await assertRendererAfterLifecycleYield(
    mainEvidence, rendererEvidence, 42, fixtureWebContents, () => false,
    validateFixture, privacy,
    {
      yieldToEvents: async () => undefined,
      readEvidence: (pid) => {
        readPids.push(pid);
        return pid === 41 ? mainEvidence : finalRenderer;
      },
    }
  );
  assert.deepStrictEqual(readPids, [41, 42]);
  assert.strictEqual(finalSamples.renderer, finalRenderer);
  assert.strictEqual(finalSamples.main, mainEvidence);
  const separatedWithRoot = { ...rendererEvidence, filesystem: confinedRoot };
  const separatedFinal = await assertRendererAfterLifecycleYield(
    sharedMain, separatedWithRoot, 42, fixtureWebContents, () => false, validateFixture, privacy,
    {
      yieldToEvents: async () => undefined,
      readEvidence: (pid) => pid === 41 ? mainEvidence : rendererEvidence,
    }
  );
  assert.strictEqual(separatedFinal.renderer.filesystem, undefined);
  assert.strictEqual(separatedFinal.main.filesystem, undefined);
  const separatedMalformedMap = { ...rendererEvidence, uidMap: '0 4294967296 1' };
  await assertRendererAfterLifecycleYield(
    mainEvidence, separatedMalformedMap, 42, fixtureWebContents, () => false, validateFixture, privacy,
    {
      yieldToEvents: async () => undefined,
      readEvidence: (pid) => pid === 41 ? mainEvidence : separatedMalformedMap,
    }
  );
  await assert.rejects(
    assertRendererAfterLifecycleYield(
      mainEvidence, rendererEvidence, 42, fixtureWebContents, () => false,
      validateFixture, privacy,
      {
        yieldToEvents: async () => undefined,
        readEvidence: (pid) => pid === 41 ? mainEvidence : { ...rendererEvidence, startTime: '12346' },
      }
    ), /renderer-instance-replaced/
  );
  for (const replacement of [
    { ...sharedRenderer, status: { ...sharedRenderer.status, NoNewPrivs: '0' } },
    { ...sharedRenderer, status: { ...sharedRenderer.status, Seccomp: '0' } },
    { ...sharedRenderer, status: { ...sharedRenderer.status, Seccomp_filters: '0' } },
    { ...sharedRenderer, status: { ...sharedRenderer.status, CapPrm: '1' } },
    { ...sharedRenderer, status: { ...sharedRenderer.status, CapInh: '1' } },
    { ...sharedRenderer, status: { ...sharedRenderer.status, Pid: '43' } },
    { ...sharedRenderer, uidMap: '0 1001 1' },
    { ...sharedRenderer, gidMap: '0 1000 2' },
    { ...sharedRenderer, filesystem: undefined },
    { ...sharedRenderer, filesystem: { ...confinedRoot, empty: false } },
    { ...sharedRenderer, filesystem: { ...confinedRoot, inode: '99' } },
    { ...sharedRenderer, filesystem: { ...confinedRoot, cwdMatchesRoot: false } },
    { ...sharedRenderer, namespaces: { ...sharedRenderer.namespaces, net: 'net:[99]' } },
    { ...sharedRenderer, namespaces: { ...sharedRenderer.namespaces, pid: 'pid:[99]' } },
    { ...sharedRenderer, argv: ['--type=renderer', '--no-sandbox'] },
    { ...sharedRenderer, securityLabel: 'changed' },
  ]) {
    await assert.rejects(assertRendererAfterLifecycleYield(
      sharedMain, sharedRenderer, 42, fixtureWebContents, () => false, validateFixture, privacy,
      {
        yieldToEvents: async () => undefined,
        readEvidence: (pid) => pid === 41 ? sharedMain : replacement,
      }
    ));
  }
  for (const replacement of [
    { ...sharedMain, startTime: 'changed' },
    { ...sharedMain, status: { Pid: '40' } },
    { ...sharedMain, status: { ...sharedMain.status, Seccomp: '0' } },
    { ...sharedMain, filesystem: undefined },
    { ...sharedMain, filesystem: { ...mainRoot, device: '99' } },
    { ...sharedMain, uidMap: '0 1 4294967295' },
    { ...sharedMain, gidMap: '0 1 4294967295' },
    { ...sharedMain, namespaces: { ...sharedMain.namespaces, mnt: 'mnt:[99]' } },
    { ...sharedMain, argv: ['--no-sandbox'] },
  ]) {
    await assert.rejects(assertRendererAfterLifecycleYield(
      sharedMain, sharedRenderer, 42, fixtureWebContents, () => false, validateFixture, privacy,
      {
        yieldToEvents: async () => undefined,
        readEvidence: (pid) => pid === 41 ? replacement : sharedRenderer,
      }
    ));
  }
  let finalReadCount = 0;
  await assert.rejects(assertRendererAfterLifecycleYield(
    sharedMain, sharedRenderer, 42,
    { isDestroyed: () => finalReadCount === 2, getOSProcessId: () => 42 },
    () => false, validateFixture, privacy,
    {
      yieldToEvents: async () => undefined,
      readEvidence: (pid) => {
        finalReadCount += 1;
        return pid === 41 ? sharedMain : sharedRenderer;
      },
    }
  ), /renderer-destroyed-after-collection/);
  finalReadCount = 0;
  await assert.rejects(assertRendererAfterLifecycleYield(
    sharedMain, sharedRenderer, 42,
    { isDestroyed: () => false, getOSProcessId: () => finalReadCount === 2 ? 43 : 42 },
    () => false, validateFixture, privacy,
    {
      yieldToEvents: async () => undefined,
      readEvidence: (pid) => {
        finalReadCount += 1;
        return pid === 41 ? sharedMain : sharedRenderer;
      },
    }
  ), /renderer-pid-changed-after-collection/);
  const finalSharedRenderer = { ...sharedRenderer, uidMap: '00\t1000 01' };
  const finalSharedSamples = await assertRendererAfterLifecycleYield(
    sharedMain, sharedRenderer, 42, fixtureWebContents, () => false, validateFixture, privacy,
    {
      yieldToEvents: async () => undefined,
      readEvidence: (pid) => pid === 41 ? sharedMain : finalSharedRenderer,
    }
  );
  assert.strictEqual(finalSharedSamples.renderer, finalSharedRenderer);
  const normalizedFinalShared = normalizeProcessEvidence(
    finalSharedSamples.renderer, finalSharedSamples.main, '<RENDERER_PID>', privacy
  );
  assert.deepStrictEqual(normalizedFinalShared.filesystem, {
    route: 'chromium-proc-fdinfo', sameRootAsMain: false, procfs: true, directory: true,
    empty: true, deadProcRoot: false, cwdMatchesRoot: true,
  });
  assert.strictEqual(normalizedFinalShared.namespaces.net.sameAsMain, false);
  assert.strictEqual(normalizedFinalShared.status.CapPrm, rendererEvidence.status.CapPrm);
  assert.strictEqual(normalizedFinalShared.status.CapInh, rendererEvidence.status.CapInh);
  assert(!Object.hasOwn(normalizedFinalShared.filesystem, 'device'));
  assert(!Object.hasOwn(normalizedFinalShared.filesystem, 'inode'));
  assertNoSensitiveContent(JSON.stringify(normalizedFinalShared), privacy);
  const normalizedDeadRoot = normalizeProcessEvidence(
    { ...sharedRenderer, filesystem: { ...confinedRoot, empty: false, deadProcRoot: true } },
    sharedMain, '<RENDERER_PID>', privacy
  );
  assert.strictEqual(normalizedDeadRoot.filesystem.deadProcRoot, true);
  assert.strictEqual(normalizedDeadRoot.filesystem.empty, false);
  assert.strictEqual(normalizeProcessEvidence(
    sharedMain, sharedMain, '<MAIN_PID>', privacy
  ).filesystem, null);
  assert.strictEqual(normalizeProcessEvidence(
    sharedRenderer, mainEvidence, '<RENDERER_PID>', privacy
  ).filesystem, null);
  assert.strictEqual(normalizeProcessEvidence(
    { ...sharedRenderer, filesystem: { ...confinedRoot, chromiumFdinfoRoot: false } },
    sharedMain, '<RENDERER_PID>', privacy
  ).filesystem, null);
  assertRendererEvidence(
    mainEvidence,
    {
      ...rendererEvidence,
      namespaces: { ...rendererEvidence.namespaces, user: mainEvidence.namespaces.user },
      uidMap: mainEvidence.uidMap,
      gidMap: mainEvidence.gidMap,
    },
    'suid-only'
  );
  assert.throws(
    () =>
      assertRendererEvidence(
        mainEvidence,
        {
          ...rendererEvidence,
          namespaces: { ...rendererEvidence.namespaces, user: mainEvidence.namespaces.user },
        },
        'userns-only'
      ),
    /shared-user-namespace/
  );

  assertHelperForClass({ uid: 0, gid: 0, mode: '4755' }, 'suid-only');
  assertHelperForClass({ uid: 0, gid: 0, mode: '4755' }, 'combined-unattributed');
  assertHelperForClass({ uid: 0, gid: 0, mode: '0755' }, 'userns-only');
  assert.throws(
    () => assertHelperForClass({ uid: 1000, gid: 1000, mode: '4755' }, 'suid-only'),
    /helper-owner/
  );
  assert.throws(
    () => assertHelperForClass({ uid: 0, gid: 0, mode: '0755' }, 'suid-only'),
    /helper-mode/
  );
  assertFileContract({ uid: 0, gid: 0, mode: '0755' }, '0755');
  assert.throws(
    () => assertFileContract({ uid: 1, gid: 0, mode: '0755' }, '0755'),
    /file-owner/
  );
  assert.throws(
    () => assertFileContract({ uid: 0, gid: 1, mode: '0755' }, '0755'),
    /file-group/
  );
  assert.throws(
    () => assertFileContract({ uid: 0, gid: 0, mode: '0775' }, '0755'),
    /file-mode/
  );
  assertIdentityHash(
    { sha256: 'a'.repeat(64) },
    { sha256: 'a'.repeat(64) },
    'electron'
  );
  assert.throws(
    () =>
      assertIdentityHash(
        { sha256: 'a'.repeat(64) },
        { sha256: 'b'.repeat(64) },
        'electron'
      ),
    /identity-electron-mismatch/
  );

  const normalizedRenderer = normalizeProcessEvidence(
    rendererEvidence,
    mainEvidence,
    '<RENDERER_PID>',
    privacy
  );
  const normalizedJson = JSON.stringify(normalizedRenderer);
  assert(!normalizedJson.includes('user:[2]'));
  assert(!normalizedJson.includes('0 1000 1'));
  assert.strictEqual(normalizedRenderer.pid, '<RENDERER_PID>');
  assert.strictEqual(normalizedRenderer.namespaces.user.sameAsMain, false);
  assert.strictEqual(normalizedRenderer.maps.uid.classification, 'remapped');
  const publicMetadata = publicFileMetadata({
    sha256: 'a'.repeat(64),
    mode: '0755',
    uid: 0,
    gid: 0,
    regularFile: true,
    symlink: false,
  });
  assert.deepStrictEqual(publicMetadata.owner, 'root');
  assert(!Object.hasOwn(publicMetadata, 'uid'));
  const finalEvidenceFixture = {
    schemaVersion: SCHEMA_VERSION,
    result: 'pass',
    paths: {
      electron: '<INSTALL_ROOT>/libexec/bundle-electron/lib/electron/electron',
      policyAsset: '<APPARMOR_PROFILE>',
    },
    renderer: normalizedRenderer,
    files: { electron: publicMetadata },
    diagnostics: summarizeStderr(
      Buffer.from(`AVC denied path=${roots.home}/private user=alice host=build-host`),
      1,
      privacy
    ),
  };
  assertNoSensitiveContent(JSON.stringify(finalEvidenceFixture), privacy);
  const privateFailure = createFailureEvidence(new Error('/home/alice/private'));
  assert.strictEqual(privateFailure.schemaVersion, SCHEMA_VERSION);
  assert.strictEqual(privateFailure.result, 'fail');
  assert.deepStrictEqual(privateFailure.failure, {
    category: 'other',
    code: 'unclassified',
  });
  assert.strictEqual(privateFailure.diagnostics.stderrSummaryRequired, true);

  const appArmor = expectedPolicy(
    {
      policy: 'apparmor',
      cluster: 'mainnet',
      apparmorSemanticIdentity: 'default-allow-userns-ubuntu-24.04',
    },
    '/opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/electron'
  );
  assertPolicyLabel(
    `${appArmor.processLabel}${APPARMOR_LOADED_PROFILE_SUFFIX}`,
    rendererEvidence.securityLabel,
    'apparmor'
  );
  assert.throws(
    () => assertPolicyLabel(appArmor.processLabel, 'unconfined', 'apparmor'),
    /apparmor-label-mismatch/
  );
  assertPolicyHostEvidence(
    appArmor,
    { semanticCompatible: true, parserVersion: '4.0.2' }
  );
  assert.throws(
    () =>
      assertPolicyHostEvidence(
        appArmor,
        { semanticCompatible: false, parserVersion: '4.0.2' }
      ),
    /apparmor-semantic-incompatible/
  );
  const selinux = expectedPolicy(
    { policy: 'selinux', cluster: 'mainnet' },
    '/opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/electron'
  );
  const selinuxIdentity = {
    ...selinux,
    processLabel: 'system_u:system_r:reviewed_daedalus_t:s0',
    electronFileContext: 'system_u:object_r:reviewed_electron_exec_t:s0',
    helperFileContext: 'system_u:object_r:reviewed_sandbox_exec_t:s0',
    module: 'reviewed_daedalus',
    priority: 200,
  };
  assert.deepStrictEqual(
    assertSelinuxProcessContext(
      {
        transitionSourceRole: 'unconfined_r',
        transitionSourceType: 'unconfined_t',
        mainProcessRole: 'unconfined_r',
        mainProcessType: 'unconfined_t',
      },
      'unconfined_u:unconfined_r:unconfined_t:s0-s0:c0.c1023',
      'unconfined_u:unconfined_r:unconfined_t:s0-s0:c0.c1023',
      'main'
    ).observed,
    {
      user: 'unconfined_u',
      role: 'unconfined_r',
      type: 'unconfined_t',
      range: 's0-s0:c0.c1023',
    }
  );
  assert.throws(
    () =>
      assertSelinuxProcessContext(
        {
          transitionSourceRole: 'unconfined_r',
          transitionSourceType: 'unconfined_t',
          rendererProcessRole: 'unconfined_r',
          rendererProcessType: 'chrome_sandbox_t',
        },
        'unconfined_u:unconfined_r:unconfined_t:s0',
        'unconfined_u:unconfined_r:unconfined_t:s0',
        'renderer'
      ),
    /selinux-renderer-context/
  );
  assertPolicyLabel(
    selinuxIdentity.processLabel,
    'system_u:system_r:reviewed_daedalus_t:s0',
    'selinux'
  );
  assertPolicyHostEvidence(selinuxIdentity, {
    state: 'enforcing',
    electronFileContext: selinuxIdentity.electronFileContext,
    helperFileContext: selinuxIdentity.helperFileContext,
    effectiveFileContexts: true,
  });
  const selinuxPathsFixture = {
    electron: '/opt/daedalus/mainnet/libexec/electron-bin',
    chromeSandbox: '/opt/daedalus/mainnet/libexec/chrome-sandbox',
  };
  assert.deepStrictEqual(
    verifySelinuxEffectivePolicy(selinuxIdentity, selinuxPathsFixture, (command, args) => ({
      status: 0,
      stdout:
        command === 'getenforce'
          ? 'Enforcing\n'
          : args[1] === selinuxPathsFixture.electron
            ? `${selinuxIdentity.electronFileContext}\n`
            : `${selinuxIdentity.helperFileContext}\n`,
    })),
    { state: 'enforcing', effectiveFileContexts: true }
  );
  assert.throws(
    () =>
      verifySelinuxEffectivePolicy(selinuxIdentity, selinuxPathsFixture, (command) => ({
        status: 0,
        stdout:
          command === 'getenforce'
            ? 'Enforcing\n'
            : 'system_u:object_r:bin_t:s0\n',
      })),
    /selinux-electron-effective-context/
  );
  assert.throws(
    () =>
      assertPolicyLabel(
        selinuxIdentity.processLabel,
        'system_u:system_r:unconfined_t:s0',
        'selinux'
      ),
    /selinux-label-mismatch/
  );
  assert.throws(
    () =>
      assertPolicyHostEvidence(selinuxIdentity, {
        state: 'enforcing',
        electronFileContext: 'system_u:object_r:bin_t:s0',
        helperFileContext: 'system_u:object_r:bin_t:s0',
        moduleLoaded: true,
      }),
    /selinux-electron-context-mismatch/
  );

  assert.deepStrictEqual(Object.keys(SUPPORT_MATRIX), [
    'ubuntu-22.04',
    'ubuntu-24.04',
    'ubuntu-26.04',
    'debian-12',
    'debian-13',
    'fedora-43',
    'arch-2026.09.01',
    'omarchy-4.0.2',
    'nixos-26.05',
  ]);
  assert.strictEqual(SUPPORT_MATRIX['ubuntu-24.04'].policy, 'apparmor');
  assert.strictEqual(SUPPORT_MATRIX['ubuntu-22.04'].supportState, 'wallet-only');
  assert.strictEqual(SUPPORT_MATRIX['ubuntu-22.04'].policy, 'none');
  assert.strictEqual(SUPPORT_MATRIX['ubuntu-22.04'].helperMode, '0755');
  assert.strictEqual(SUPPORT_MATRIX['ubuntu-24.04'].supportState, 'supported');
  assert.strictEqual(SUPPORT_MATRIX['debian-13'].policy, 'none');
  assert.strictEqual(SUPPORT_MATRIX['fedora-43'].policy, 'selinux');
  assert.strictEqual(SUPPORT_MATRIX['arch-2026.09.01'].policy, 'none');
  assert.strictEqual(SUPPORT_MATRIX['arch-2026.09.01'].helperMode, '0755');
  assert.strictEqual(SUPPORT_MATRIX['omarchy-4.0.2'].matrixRevision, ARCH_MATRIX_REVISION);
  assert.strictEqual(SUPPORT_MATRIX['fedora-42'], undefined);
  assert.strictEqual(SUPPORT_MATRIX['opensuse-leap-15.6'], undefined);
  assert.strictEqual(MATRIX_REVISION, 'task-108-matrix-2026-08-18');
  assert.strictEqual(ARCH_MATRIX_REVISION, 'task-111-matrix-2026-09-02');
  assert.strictEqual(NIXOS_MATRIX_REVISION, 'task-112-matrix-2026-09-11');
  assert.strictEqual(SUPPORT_MATRIX['nixos-26.05'].packageFamily, 'nix');
  assert.strictEqual(SUPPORT_MATRIX['nixos-26.05'].matrixRevision, NIXOS_MATRIX_REVISION);
  const matrixVersionFixtures = {
    'ubuntu-22.04': ['22.04', '22.04.5', '24.04'],
    'ubuntu-24.04': ['24.04', '24.04.3', '25.10'],
    'ubuntu-26.04': ['26.04', '26.04.1', '26.10'],
    'debian-12': ['12', '12.11', '13'],
    'debian-13': ['13', '13.1', '12'],
    'fedora-43': ['43', '43', '42'],
    'arch-2026.09.01': ['2026.09.01', '2026.09.01', '2026.09.02'],
    'omarchy-4.0.2': ['4.0.2', '4.0.2', '4.0.3'],
  };
  for (const [rowName, [base, point, rejected]] of Object.entries(
    matrixVersionFixtures
  )) {
    const row = SUPPORT_MATRIX[rowName];
    assert(row.versionPattern.test(base));
    assert(row.versionPattern.test(point));
    assert(!row.versionPattern.test(rejected));
  }

  const contractEnvironment = {
    DAEDALUS_PROBE_MATRIX_ROW: 'ubuntu-24.04',
    DAEDALUS_PROBE_MATRIX_REVISION: MATRIX_REVISION,
    DAEDALUS_PROBE_SANDBOX_CLASS: 'userns-only',
    DAEDALUS_PROBE_CLUSTER: 'mainnet',
  };
  const originalEnvironment = Object.fromEntries(
    Object.keys(contractEnvironment).map((name) => [name, process.env[name]])
  );
  try {
    Object.assign(process.env, contractEnvironment);
    assert.deepStrictEqual(readContractSelection('/opt/daedalus/mainnet'), {
      matrixRow: 'ubuntu-24.04',
      matrixRevision: MATRIX_REVISION,
      sandboxClass: 'userns-only',
      cluster: 'mainnet',
      packageFamily: 'deb',
      policy: 'apparmor',
      supportState: 'supported',
      reason: 'supported',
      helperMode: '4755',
      apparmorSemanticIdentity: 'default-allow-userns-ubuntu-24.04',
      distributionId: 'ubuntu',
      versionPattern: /^24\.04(?:\.\d+)?$/,
    });
    const parsedHost = parseOsRelease('ID=ubuntu\nVERSION_ID="24.04"\nHOME_URL="https://example.invalid"\n');
    assert.deepStrictEqual(parsedHost, {
      id: 'ubuntu',
      versionId: '24.04',
      buildId: undefined,
      imageId: undefined,
      imageVersion: undefined,
    });
    assertHostContract(readContractSelection('/opt/daedalus/mainnet'), parsedHost);
    assert.throws(
      () =>
        assertHostContract(readContractSelection('/opt/daedalus/mainnet'), {
          id: 'ubuntu',
          versionId: '25.10',
        }),
      /matrix-version-mismatch/
    );
    process.env.DAEDALUS_PROBE_MATRIX_ROW = 'ubuntu-25.10';
    assert.throws(
      () => readContractSelection('/opt/daedalus/mainnet'),
      /unsupported-matrix-row/
    );
    process.env.DAEDALUS_PROBE_MATRIX_ROW = 'ubuntu-24.04';
    process.env.DAEDALUS_PROBE_MATRIX_REVISION = 'stale';
    assert.throws(
      () => readContractSelection('/opt/daedalus/mainnet'),
      /unsupported-matrix-revision/
    );
    process.env.DAEDALUS_PROBE_MATRIX_REVISION = MATRIX_REVISION;
    process.env.DAEDALUS_PROBE_SANDBOX_CLASS = 'suid-only';
    assert.strictEqual(
      readContractSelection('/opt/daedalus/mainnet').sandboxClass,
      'suid-only'
    );
    process.env.DAEDALUS_PROBE_MATRIX_ROW = 'ubuntu-22.04';
    assert.throws(
      () => readContractSelection('/opt/daedalus/mainnet'),
      /wallet-only-matrix-row:apparmor-policy-proof-pending/
    );
    Object.assign(process.env, {
      DAEDALUS_PROBE_MATRIX_ROW: 'arch-2026.09.01',
      DAEDALUS_PROBE_MATRIX_REVISION: ARCH_MATRIX_REVISION,
      DAEDALUS_PROBE_SANDBOX_CLASS: 'userns-only',
    });
    const archContract = readContractSelection('/opt/daedalus/mainnet');
    assert.strictEqual(archContract.packageFamily, 'arch');
    assert.strictEqual(archContract.policy, 'none');
    const archHost = {
      ...parseOsRelease('ID=arch\nBUILD_ID=rolling\n'),
      kernelRelease: '7.2.2-arch1-1',
    };
    assertHostContract(archContract, archHost);
    assert.throws(
      () =>
        assertHostContract(archContract, {
          ...archHost,
          kernelRelease: '7.2.3-arch1-1',
        }),
      /matrix-kernelRelease-mismatch/
    );
    process.env.DAEDALUS_PROBE_SANDBOX_CLASS = 'suid-only';
    assert.throws(
      () => readContractSelection('/opt/daedalus/mainnet'),
      /arch-requires-userns-only/
    );
  } finally {
    for (const [name, value] of Object.entries(originalEnvironment)) {
      if (value === undefined) delete process.env[name];
      else process.env[name] = value;
    }
  }

  const fixtureContract = {
    matrixRevision: MATRIX_REVISION,
    matrixRow: 'ubuntu-24.04',
    cluster: 'mainnet',
    packageFamily: 'deb',
    supportState: 'supported',
    reason: 'supported',
    helperMode: '4755',
    policy: 'apparmor',
    apparmorSemanticIdentity: 'default-allow-userns-ubuntu-24.04',
    distributionId: 'ubuntu',
    versionPattern: /^24\.04(?:\.\d+)?$/,
  };
  const fixturePaths = exactPackagePaths(
    '/opt/daedalus/mainnet',
    'mainnet',
    'apparmor'
  );
  const fixtureManifest = {
    schemaVersion: IDENTITY_SCHEMA_VERSION,
    packageFamily: 'deb',
    matrixRevision: MATRIX_REVISION,
    matrixRow: 'ubuntu-24.04',
    distribution: { id: 'ubuntu', versionId: '24.04' },
    supportState: 'supported',
    reason: 'supported',
    cluster: 'mainnet',
    helper: { mode: '4755', sha256: 'a'.repeat(64) },
    policy: {
      kind: 'apparmor',
      processLabel: fixturePaths.electron,
      semanticIdentity: 'default-allow-userns-ubuntu-24.04',
      loadedProfileSuffix: APPARMOR_LOADED_PROFILE_SUFFIX,
      requiredAbi: '4.0',
      requiredFlags: ['default_allow'],
      requiredRules: ['userns'],
    },
    files: Object.fromEntries(
      ['launcher', 'frontend', 'wrapper', 'electron', 'chromeSandbox', 'policyAsset'].map(
        (name) => [name, { sha256: 'a'.repeat(64) }]
      )
    ),
  };
  assertIdentityManifest(fixtureManifest, fixtureContract);
  assert.throws(
    () =>
      assertIdentityManifest(
        {
          ...fixtureManifest,
          policy: { ...fixtureManifest.policy, loadedProfileSuffix: ' (enforce)' },
        },
        fixtureContract
      ),
    /identity-apparmor-loaded-profile-suffix/
  );
  assert.throws(
    () => assertIdentityManifest({ ...fixtureManifest, cluster: 'preview' }, fixtureContract),
    /identity-cluster/
  );

  const archFixtureContract = {
    matrixRevision: ARCH_MATRIX_REVISION,
    matrixRow: 'arch-2026.09.01',
    cluster: 'mainnet',
    packageFamily: 'arch',
    supportState: 'supported',
    reason: 'supported',
    helperMode: '0755',
    policy: 'none',
    distributionId: 'arch',
    versionPattern: /^2026\.09\.01$/,
    fingerprint: SUPPORT_MATRIX['arch-2026.09.01'].fingerprint,
    manifestFingerprint:
      SUPPORT_MATRIX['arch-2026.09.01'].manifestFingerprint,
  };
  const archFixtureManifest = {
    ...fixtureManifest,
    packageFamily: 'arch',
    matrixRevision: ARCH_MATRIX_REVISION,
    matrixRow: 'arch-2026.09.01',
    distribution: {
      ...SUPPORT_MATRIX['arch-2026.09.01'].manifestFingerprint,
    },
    helper: { mode: '0755', sha256: 'a'.repeat(64) },
    policy: { kind: 'none' },
  };
  assertIdentityManifest(archFixtureManifest, archFixtureContract);
  assert.throws(
    () =>
      assertIdentityManifest(
        {
          ...archFixtureManifest,
          distribution: {
            ...archFixtureManifest.distribution,
            kernelRelease: '7.2.3-arch1-1',
          },
        },
        archFixtureContract
      ),
    /identity-distribution-kernelRelease/
  );
  assert.throws(
    () =>
      assertIdentityManifest(
        { ...archFixtureManifest, helper: { mode: '4755' } },
        archFixtureContract
      ),
    /identity-helper-mode/
  );
  assert.throws(
    () =>
      assertIdentityManifest(
        { ...archFixtureManifest, policy: { kind: 'apparmor' } },
        archFixtureContract
      ),
    /identity-policy/
  );
  const observedFixtureModes = {};
  const verifiedPackage = verifyPackageFiles(
    fixturePaths,
    fixtureManifest,
    'combined-unattributed',
    (filePath, expectedMode) => {
      observedFixtureModes[filePath] = expectedMode;
      return {
        sha256: 'a'.repeat(64),
        mode: expectedMode,
        uid: 0,
        gid: 0,
        regularFile: true,
        symlink: false,
      };
    },
    () => ({ mode: '0755', owner: 'root', group: 'root' })
  );
  assert.strictEqual(verifiedPackage.files.chromeSandbox.mode, '4755');
  assert.strictEqual(verifiedPackage.files.policyAsset.sha256, 'a'.repeat(64));
  assert(Object.keys(verifiedPackage.directories).length >= 6);
  assert.strictEqual(observedFixtureModes[fixturePaths.launcher], '0755');
  assert.strictEqual(observedFixtureModes[fixturePaths.frontend], '0755');
  assert.strictEqual(observedFixtureModes[fixturePaths.wrapper], '0755');
  assert.strictEqual(observedFixtureModes[fixturePaths.electron], '0755');
  assert.strictEqual(observedFixtureModes[fixturePaths.chromeSandbox], '4755');
  assert.strictEqual(observedFixtureModes[fixturePaths.policyAsset], '0644');
  assert.strictEqual(observedFixtureModes[fixturePaths.identityManifest], '0644');

  const appArmorEvidence = collectPolicyEvidence(
    fixtureContract,
    fixturePaths,
    fixtureManifest,
    { securityLabel: `${fixturePaths.electron}${APPARMOR_LOADED_PROFILE_SUFFIX}` },
    {
      readFileSync: (filePath) => {
        if (filePath.endsWith('/enabled')) return 'Y\n';
        if (filePath === fixturePaths.policyAsset)
          return `abi <abi/4.0>,\nprofile ${fixturePaths.electron} flags=(default_allow) { userns, }\n`;
        return `${fixturePaths.electron}${APPARMOR_LOADED_PROFILE_SUFFIX}\n`;
      },
      run: () => ({ status: 0, stdout: 'AppArmor parser version 4.0', stderr: '' }),
    }
  );
  assert.strictEqual(appArmorEvidence.profileHashBound, true);
  assert.throws(
    () =>
      collectPolicyEvidence(
        fixtureContract,
        fixturePaths,
        fixtureManifest,
        { securityLabel: `${fixturePaths.electron}${APPARMOR_LOADED_PROFILE_SUFFIX}` },
        {
          readFileSync: (filePath) => {
            if (filePath.endsWith('/enabled')) return 'Y\n';
            if (filePath === fixturePaths.policyAsset)
              return `abi <abi/4.0>,\nprofile ${fixturePaths.electron} { userns, }\n`;
            return `${fixturePaths.electron}${APPARMOR_LOADED_PROFILE_SUFFIX}\n`;
          },
          run: () => ({ status: 0, stdout: 'AppArmor parser version 3.0', stderr: '' }),
        }
      ),
    /apparmor-profile-default-allow/
  );

  const selinuxPaths = exactPackagePaths(
    '/opt/daedalus/mainnet',
    'mainnet',
    'selinux'
  );
  const selinuxManifest = {
    ...fixtureManifest,
    policy: {
      kind: 'selinux',
      transitionSourceRole: 'unconfined_r',
      transitionSourceType: 'unconfined_t',
      mainProcessRole: 'unconfined_r',
      mainProcessType: 'unconfined_t',
      rendererProcessRole: 'unconfined_r',
      rendererProcessType: 'chrome_sandbox_t',
      electronFileContext: 'system_u:object_r:bin_t:s0',
      helperFileContext: 'system_u:object_r:chrome_sandbox_exec_t:s0',
      module: 'reviewed_daedalus',
    },
  };
  const selinuxEvidence = collectPolicyEvidence(
    { policy: 'selinux', cluster: 'mainnet' },
    selinuxPaths,
    selinuxManifest,
    { securityLabel: 'unconfined_u:unconfined_r:chrome_sandbox_t:s0-s0:c0.c1023' },
    {
      mainEvidence: {
        securityLabel: 'unconfined_u:unconfined_r:unconfined_t:s0-s0:c0.c1023',
      },
      run: (command, args) => ({
        status: 0,
        stdout:
          command === 'getenforce'
            ? 'Enforcing\n'
            : args[1] === selinuxPaths.electron
              ? `${selinuxManifest.policy.electronFileContext}\n`
              : `${selinuxManifest.policy.helperFileContext}\n`,
        stderr: '',
      }),
      readContext: (filePath) =>
        filePath === selinuxPaths.electron
          ? selinuxManifest.policy.electronFileContext
          : selinuxManifest.policy.helperFileContext,
    }
  );
  assert.strictEqual(selinuxEvidence.module, 'reviewed_daedalus');

  assert.deepStrictEqual(
    observeUsernsAvailability(() => ({ status: 0, stdout: '', stderr: '' })),
    { available: true, check: 'unshare-Ur' }
  );
  assert.deepStrictEqual(
    observeUsernsAvailability(() => ({ status: 1, stdout: '', stderr: '' })),
    { available: false, check: 'unshare-Ur' }
  );
  assertSandboxClassPrerequisites('userns-only', { available: true });
  assertSandboxClassPrerequisites('suid-only', { available: false });
  assert.throws(
    () => assertSandboxClassPrerequisites('combined-unattributed', { available: false }),
    /userns-required/
  );
  assert.strictEqual(hasApprovedSandboxRoute({ suid: true, userns: false }), true);
  assert.strictEqual(hasApprovedSandboxRoute({ suid: false, userns: true }), true);
  assert.strictEqual(hasApprovedSandboxRoute({ suid: false, userns: false }), false);

  const longSummary = summarizeStderr(
    Buffer.from(`FATAL sandbox ${'x'.repeat(STDERR_LIMIT_BYTES + 100)}`),
    1,
    privacy
  );
  assert.strictEqual(longSummary.truncated, true);

  const cleanupFailure = await performCleanup(null, '/tmp/fixture', {
    remove: async () => {
      throw new Error('fixture-remove-failure');
    },
  });
  assert.strictEqual(cleanupFailure.message, 'profile-cleanup-failed');
  let profileRemovalAttempted = false;
  const sessionCleanupFailure = await performCleanup(
    {
      clearStorageData: async () => {
        throw new Error('fixture-storage-failure');
      },
      clearCache: async () => undefined,
    },
    '/tmp/fixture',
    {
      remove: async () => {
        profileRemovalAttempted = true;
      },
    }
  );
  assert.strictEqual(sessionCleanupFailure.message, 'session-cleanup-failed');
  assert.strictEqual(profileRemovalAttempted, true);
  const combinedCleanupFailure = await performCleanup(
    {
      clearStorageData: async () => {
        throw new Error('fixture-storage-failure');
      },
      clearCache: async () => undefined,
    },
    '/tmp/fixture',
    {
      remove: async () => {
        throw new Error('fixture-remove-failure');
      },
    }
  );
  assert.strictEqual(
    combinedCleanupFailure.message,
    'session-cleanup-failed:profile-cleanup-failed'
  );
  const unifiedFailure = createFailureEvidence(
    cleanupFailure,
    { revision: MATRIX_REVISION, row: 'ubuntu-24.04' },
    createEvidenceEnvelope({ result: 'pass', host: { distributionId: 'ubuntu' } })
  );
  assert.strictEqual(unifiedFailure.result, 'fail');
  assert.strictEqual(unifiedFailure.host.distributionId, 'ubuntu');
  assert.strictEqual(unifiedFailure.failure.code, 'profile-cleanup-failed');
  const mergedEvidence = mergeDiagnostics(
    createEvidenceEnvelope({ result: 'pass', matrix: { revision: MATRIX_REVISION } }),
    summarizeStderr(Buffer.alloc(0), 0, privacy)
  );
  assert.strictEqual(mergedEvidence.result, 'pass');
  assert.strictEqual(mergedEvidence.diagnostics.stderrSummaryRequired, false);
  assert.strictEqual(mergedEvidence.matrix.revision, MATRIX_REVISION);
  for (const malformedProbe of ['', '{"schemaVersion":2', '{"schemaVersion":1}']) {
    const malformedEvidence = mergeDiagnostics(
      parseProbeEvidence(malformedProbe),
      summarizeStderr(Buffer.from('FATAL sandbox failed'), 132, privacy)
    );
    assert.strictEqual(malformedEvidence.result, 'fail');
    assert.strictEqual(malformedEvidence.failure.code, 'missing-probe-evidence');
    assert.strictEqual(malformedEvidence.diagnostics.exitCode, 132);
    assert.strictEqual(malformedEvidence.diagnostics.byteCount > 0, true);
    assert.strictEqual(malformedEvidence.diagnostics.stderrSummaryRequired, false);
  }

  const fixtureRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'daedalus-probe-fixture-'));
  try {
    const regularPath = path.join(fixtureRoot, 'regular');
    const symlinkPath = path.join(fixtureRoot, 'symlink');
    fs.writeFileSync(regularPath, 'fixture', { mode: 0o755 });
    fs.symlinkSync(regularPath, symlinkPath);
    assert.strictEqual(fileMetadata(regularPath).regularFile, true);
    assert.throws(() => fileMetadata(symlinkPath), /expected-regular-file/);
  } finally {
    fs.rmSync(fixtureRoot, { recursive: true, force: true });
  }

  await assert.rejects(
    withTimeout(new Promise(() => undefined), 5),
    /timeout/
  );
  const exitOnlyFailure = childProcess.spawnSync(
    process.execPath,
    [__filename, '--transition-only', '--exit-only'],
    {
      encoding: 'utf8',
      env: { ...process.env, ELECTRON_DISABLE_SANDBOX: '1' },
    }
  );
  assert.strictEqual(exitOnlyFailure.status, 1);
  assert.strictEqual(exitOnlyFailure.stdout, '');
  assert.strictEqual(exitOnlyFailure.stderr, '');
  process.stdout.write('linux-chromium-sandbox-probe self-test passed\n');
}

async function main() {
  const args = process.argv.slice(2);
  if (args.includes('--self-test')) {
    await runSelfTest();
    return;
  }
  if (args[0] === 'sanitize-stderr') {
    runStderrSanitizer(args.slice(1));
    return;
  }
  await runElectronProbe({
    transitionOnly: args.includes('--transition-only'),
    exitOnly: args.includes('--exit-only'),
  });
}

main().catch((error) => {
  if (process.argv.includes('--self-test')) {
    process.stderr.write(`${error.stack}\n`);
    process.exitCode = 1;
    return;
  }
  if (!process.argv.includes('--exit-only')) writeFailure(error);
  process.exitCode = 1;
});
