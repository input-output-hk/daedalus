'use strict';

const assert = require('assert');
const crypto = require('crypto');
const fs = require('fs');
const os = require('os');
const path = require('path');

const SCHEMA_VERSION = 1;
const STDERR_LIMIT_BYTES = 8192;
const PROBE_TIMEOUT_MS = 15000;
const CLEANUP_TIMEOUT_MS = 2000;
const ROOT_TOKENS = [
  ['installRoot', '<INSTALL_ROOT>'],
  ['probeRoot', '<PROBE_ROOT>'],
  ['profileRoot', '<PROFILE_ROOT>'],
  ['home', '<HOME>'],
];
const NAMESPACE_NAMES = ['user', 'pid', 'mnt'];
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
  ['evidence-invalid', /evidence-invalid|assertion|invalid/i],
  ['sandbox-init-failed', /sandbox.*(?:fail|fatal)|zygote.*(?:fail|fatal)/i],
  ['timeout', /timed? out|sandbox-probe-failed:timeout/i],
  ['renderer-exited', /render.*(?:crash|exit|gone)/i],
];

function debug(stage) {
  if (process.env.DAEDALUS_PROBE_DEBUG === '1') {
    process.stderr.write(`sandbox-probe-stage:${stage}\n`);
  }
}

function writeFailure(error) {
  const safeCode = /^[a-z0-9:-]+$/.test(String(error && error.message))
    ? error.message
    : 'unclassified';
  const category =
    error && error.name === 'AssertionError'
      ? 'evidence-invalid'
      : categorizeStderr(safeCode, 1);
  process.stderr.write(`sandbox-probe-failed:${category}:${safeCode}\n`);
}

function escapeRegExp(value) {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function replaceLiteralIgnoreCase(value, literal, replacement) {
  if (!literal) return value;
  return value.replace(new RegExp(escapeRegExp(literal), 'gi'), replacement);
}

function sensitiveEnvironmentValues(environment, roots) {
  const sensitiveName =
    /(?:auth|cookie|credential|daedalus|electron|home|host|key|logname|password|path|secret|token|url|user|xdg)/i;
  const rootValues = new Set(Object.values(roots));
  return Object.entries(environment)
    .filter(([name, value]) => sensitiveName.test(name) && value && value.length >= 4)
    .map(([, value]) => value)
    .filter((value) => !rootValues.has(value))
    .sort((left, right) => right.length - left.length);
}

function privacyContext(roots) {
  return {
    roots,
    username: os.userInfo().username,
    hostname: os.hostname(),
    environmentValues: sensitiveEnvironmentValues(process.env, roots),
  };
}

function withTimeout(promise, timeoutMs) {
  let timeout;
  const deadline = new Promise((_, reject) => {
    timeout = setTimeout(() => reject(new Error('timeout')), timeoutMs);
  });
  return Promise.race([promise, deadline]).finally(() => clearTimeout(timeout));
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
  const { roots, username, hostname, environmentValues } = privacy;
  for (const [name] of ROOT_TOKENS) {
    if (roots[name] && value.includes(roots[name])) {
      throw new Error(`residual-sensitive-root:${name}`);
    }
  }
  if (username && new RegExp(escapeRegExp(username), 'i').test(value)) {
    throw new Error('residual-username');
  }
  if (hostname && new RegExp(escapeRegExp(hostname), 'i').test(value)) {
    throw new Error('residual-hostname');
  }
  if (/\b[a-z][a-z0-9+.-]*:\/\//i.test(value)) {
    throw new Error('residual-url');
  }
  for (const environmentValue of environmentValues) {
    if (value.includes(environmentValue)) {
      throw new Error('residual-environment-value');
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
    const normalized = sanitizeText(argument, privacy);
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
    /sandbox-probe-|\bFATAL\b|apparmor|userns|namespace|sandbox.*(?:fail|fatal)|zygote.*(?:fail|fatal)/i;
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

function readProcEvidence(pid, privacy) {
  const procRoot = `/proc/${pid}`;
  const rawArgv = fs
    .readFileSync(`${procRoot}/cmdline`)
    .toString('utf8')
    .split('\0')
    .filter(Boolean);
  const namespaces = Object.fromEntries(
    NAMESPACE_NAMES.map((name) => [
      name,
      fs.readlinkSync(`${procRoot}/ns/${name}`),
    ])
  );
  const uidMap = fs.readFileSync(`${procRoot}/uid_map`, 'utf8').trim();
  const gidMap = fs.readFileSync(`${procRoot}/gid_map`, 'utf8').trim();
  if (!/^[\d\s]+$/.test(uidMap) || !/^[\d\s]+$/.test(gidMap)) {
    throw new Error('unexpected-namespace-map');
  }
  return {
    pid,
    argv: normalizeArgv(rawArgv, privacy),
    status: parseStatus(fs.readFileSync(`${procRoot}/status`, 'utf8')),
    namespaces,
    uidMap,
    gidMap,
  };
}

function hasForbiddenSwitch(argv) {
  return argv.some((argument) =>
    FORBIDDEN_SWITCHES.some(
      (forbidden) => argument === forbidden || argument.startsWith(`${forbidden}=`)
    )
  );
}

function assertRendererEvidence(mainEvidence, rendererEvidence) {
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
  assert.notStrictEqual(
    rendererEvidence.namespaces.mnt,
    mainEvidence.namespaces.mnt,
    'shared-mount-namespace'
  );
  assert.notStrictEqual(
    rendererEvidence.namespaces.user,
    mainEvidence.namespaces.user,
    'shared-user-namespace'
  );
  assert.notStrictEqual(rendererEvidence.uidMap, mainEvidence.uidMap, 'shared-uid-map');
  assert.notStrictEqual(rendererEvidence.gidMap, mainEvidence.gidMap, 'shared-gid-map');
}

function fileMetadata(filePath) {
  const stats = fs.statSync(filePath);
  return {
    sha256: hashFile(filePath),
    mode: (stats.mode & 0o7777).toString(8).padStart(4, '0'),
    uid: stats.uid,
    gid: stats.gid,
  };
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

async function runElectronProbe() {
  if (process.platform !== 'linux') throw new Error('linux-required');
  if (process.env.ELECTRON_DISABLE_SANDBOX !== undefined) {
    throw new Error('sandbox-disabling-environment');
  }

  const { app, BrowserWindow, session } = require('electron');
  debug('electron-loaded');
  const executablePath = canonicalPath(process.execPath);
  const installRoot = canonicalPath(inferInstallRoot(executablePath));
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
  const keepAliveDuringCleanup = () => undefined;
  app.on('window-all-closed', keepAliveDuringCleanup);
  const deadline = setTimeout(() => {
    process.stderr.write('sandbox-probe-failed:timeout:process-deadline\n');
    try {
      if (window && !window.isDestroyed()) window.destroy();
    } catch (_error) {
      // The process exits fail-closed below even if Electron cleanup is unavailable.
    }
    try {
      fs.rmSync(profileRoot, { recursive: true, force: true });
    } catch (_error) {
      // The disposable operator profile remains host-local if forced cleanup fails.
    }
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
    const mainEvidence = readProcEvidence(process.pid, privacy);
    const rendererEvidence = readProcEvidence(rendererPid, privacy);
    if (process.env.DAEDALUS_PROBE_DEBUG === '1') {
      const typeArguments = rendererEvidence.argv.filter((argument) =>
        argument.startsWith('--type')
      );
      process.stderr.write(
        `sandbox-probe-renderer-type:${JSON.stringify(typeArguments)}\n`
      );
    }
    assertRendererEvidence(mainEvidence, rendererEvidence);
    debug('renderer-verified');

    const helperPath = path.join(path.dirname(executablePath), 'chrome-sandbox');
    const wrapperPath = path.join(roots.installRoot, 'libexec', 'electron');
    const helperMetadata = fileMetadata(helperPath);
    const usableSetuidHelper =
      helperMetadata.uid === 0 && (Number.parseInt(helperMetadata.mode, 8) & 0o4000) !== 0;
    assert(!usableSetuidHelper, 'unexpected-usable-setuid-helper');
    const evidence = {
      schemaVersion: SCHEMA_VERSION,
      result: 'pass',
      versions: {
        electron: process.versions.electron,
        chrome: process.versions.chrome,
        kernel: os.release(),
      },
      files: {
        electron: fileMetadata(executablePath),
        chromeSandbox: helperMetadata,
        wrapper: fileMetadata(wrapperPath),
        probe: fileMetadata(__filename),
      },
      main: mainEvidence,
      renderer: rendererEvidence,
      assertions: {
        noSandboxBypass: true,
        exactRendererPid: true,
        chromiumProcessTypeRecorded: true,
        noNewPrivs: true,
        seccomp: true,
        zeroEffectiveCapabilities: true,
        separateUserPidMountNamespaces: true,
        separateUserAndGroupMaps: true,
        noUsableSetuidHelper: true,
      },
    };
    process.stdout.write(`${JSON.stringify(evidence, null, 2)}\n`);
  } catch (error) {
    failure = error;
    writeFailure(error);
    process.exitCode = 1;
  } finally {
    try {
      if (window && !window.isDestroyed()) window.destroy();
    } catch (_error) {
      // Continue bounded cleanup and quit.
    }
    if (probeSession) {
      await withTimeout(
        Promise.allSettled([
          probeSession.clearStorageData(),
          probeSession.clearCache(),
        ]),
        CLEANUP_TIMEOUT_MS
      ).catch(() => undefined);
    }
    try {
      fs.rmSync(profileRoot, { recursive: true, force: true });
    } finally {
      clearTimeout(deadline);
    }
  }
  app.removeListener('window-all-closed', keepAliveDuringCleanup);
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
  if (inputIndex === -1 || !args[inputIndex + 1]) throw new Error('missing-input');
  if (exitCodeIndex === -1 || !args[exitCodeIndex + 1]) {
    throw new Error('missing-exit-code');
  }
  const roots = rootsFromEnvironment();
  const privacy = privacyContext(roots);
  const rawBytes = fs.readFileSync(args[inputIndex + 1]);
  const summary = summarizeStderr(rawBytes, Number(args[exitCodeIndex + 1]), privacy);
  process.stdout.write(`${JSON.stringify(summary, null, 2)}\n`);
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
  const argv = normalizeArgv(
    [
      `${roots.installRoot}/libexec/bundle-electron/lib/electron/electron`,
      `--user-data-dir=${roots.profileRoot}`,
      roots.probeRoot,
      '--type=renderer',
    ],
    privacy
  );
  assert.deepStrictEqual(argv, [
    '<INSTALL_ROOT>/libexec/bundle-electron/lib/electron/electron',
    '--user-data-dir=<PROFILE_ROOT>',
    '<PROBE_ROOT>',
    '--type=renderer',
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
  await assert.rejects(
    withTimeout(new Promise(() => undefined), 5),
    /timeout/
  );
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
  await runElectronProbe();
}

main().catch((error) => {
  if (process.argv.includes('--self-test')) {
    process.stderr.write(`${error.stack}\n`);
    process.exitCode = 1;
    return;
  }
  writeFailure(error);
  process.exitCode = 1;
});
