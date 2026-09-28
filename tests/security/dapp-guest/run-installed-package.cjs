#!/usr/bin/env node
const assert = require('assert');
const fs = require('fs');
const path = require('path');
const { spawnSync } = require('child_process');

const root = process.env.DAEDALUS_INSTALL_ROOT || '/opt/daedalus/mainnet';
assert.strictEqual(path.resolve(root), root);
const packageCluster = path.basename(root);
const isNixOS = root.startsWith('/nix/store/');
assert(isNixOS || root.startsWith('/opt/daedalus/'));

const config = path.join(root, 'config/daedalus-config.json');
const entry = path.join(
  root,
  'libexec/daedalus-js/main/dappGuestSecurityHarness.js'
);
let electron = path.join(
  root,
  'libexec/bundle-electron/lib/electron/electron'
);
let helper = path.join(
  root,
  'libexec/bundle-electron/lib/electron/chrome-sandbox'
);

if (isNixOS) {
  const manifestPath = `/var/lib/daedalus/${packageCluster}/sandbox-identity.json`;
  const manifestStat = fs.lstatSync(manifestPath);
  assert(manifestStat.isFile() && manifestStat.uid === 0);
  assert.strictEqual(manifestStat.mode & 0o22, 0);
  const identity = JSON.parse(fs.readFileSync(manifestPath, 'utf8'));
  assert.strictEqual(typeof identity.launch?.electron, 'string');
  electron = identity.launch.electron;
  helper = `/var/lib/daedalus/${packageCluster}/chrome-sandbox`;
}

for (const file of [electron, entry, config]) {
  assert(fs.statSync(file).isFile(), `missing installed package file: ${file}`);
  assert(
    fs.realpathSync(file).startsWith(`${root}/`),
    `installed package file escaped root: ${file}`
  );
}

const run = spawnSync(electron, ['--disable-gpu', entry], {
  encoding: 'utf8',
  env: {
    ...process.env,
    CHROME_DEVEL_SANDBOX: helper,
    ENTRYPOINT_DIR: root,
    DAEDALUS_CONFIG_FILE: config,
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
