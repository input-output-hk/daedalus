#!/usr/bin/env node
const assert = require('assert');
const fs = require('fs');
const path = require('path');
const { spawnSync } = require('child_process');

const root = process.env.DAEDALUS_INSTALL_ROOT || '/opt/daedalus/mainnet';
assert.strictEqual(path.resolve(root), root);
assert(root.startsWith('/opt/daedalus/'));
const electron = path.join(root, 'libexec/bundle-electron/bin/electron');
const entry = path.join(
  root,
  'libexec/daedalus-js/main/dappGuestSecurityHarness.js'
);
const launcherConfig = path.join(root, 'config/launcher-config.yaml');
const identityManifest = path.join(
  root,
  'share/daedalus-sandbox-identity.json'
);
for (const file of [electron, entry, launcherConfig, identityManifest]) {
  assert(fs.statSync(file).isFile(), `missing installed package file: ${file}`);
  assert(fs.realpathSync(file).startsWith(`${root}/`));
}
const identity = JSON.parse(fs.readFileSync(identityManifest, 'utf8'));
assert.strictEqual(identity.schemaVersion, 2);
assert.strictEqual(identity.packageFamily, 'arch');
assert.strictEqual(identity.matrixRevision, 'task-111-matrix-2026-09-02');
assert(['arch-2026.09.01', 'omarchy-4.0.2'].includes(identity.matrixRow));
assert.strictEqual(identity.supportState, 'supported');
assert.strictEqual(identity.policy && identity.policy.kind, 'none');
const expectedDistribution =
  identity.matrixRow === 'arch-2026.09.01'
    ? {
        id: 'arch',
        versionId: '2026.09.01',
        buildId: 'rolling',
        kernelRelease: '7.2.2-arch1-1',
      }
    : {
        id: 'omarchy',
        versionId: '4.0.2',
        buildId: '4.0.2',
        kernelRelease: '7.1.8-arch1-Watanare-T2-3-t2',
      };
assert.deepStrictEqual(identity.distribution, expectedDistribution);
assert.strictEqual(identity.helper && identity.helper.mode, '0755');

const run = spawnSync(electron, ['--disable-gpu', entry], {
  encoding: 'utf8',
  env: {
    ...process.env,
    CHROME_DEVEL_SANDBOX: path.join(
      root,
      'libexec/bundle-electron/lib/electron/chrome-sandbox'
    ),
    ENTRYPOINT_DIR: root,
    LAUNCHER_CONFIG: launcherConfig,
  },
  timeout: 45_000,
});
if (run.error) throw run.error;
assert.strictEqual(run.status, 0, run.stderr);
const lines = run.stdout.trim().split('\n');
const result = JSON.parse(lines[lines.length - 1]);
assert.strictEqual(result.schemaVersion, 2);
for (const category of [
  'task802IpcMatrix',
  'task802TransportMatrix',
  'task802DestinationBindingMatrix',
  'task802LifecycleRaceMatrix',
  'task802SwitchVariantMatrix',
  'task802NonpersistentStorageMatrix',
]) {
  assert.strictEqual(result[category], true, category);
}
for (const [key, value] of Object.entries(result)) {
  if (key !== 'schemaVersion' && key !== 'manifestChannels')
    assert.strictEqual(value, true, key);
}
assert(result.manifestChannels > 0);
process.stdout.write(`${JSON.stringify(result)}\n`);
