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
for (const file of [electron, entry, launcherConfig]) {
  assert(fs.statSync(file).isFile(), `missing installed package file: ${file}`);
  assert(fs.realpathSync(file).startsWith(`${root}/`));
}
assert(!Object.prototype.hasOwnProperty.call(process.env, 'ELECTRON_DISABLE_SANDBOX'));

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
assert.strictEqual(result.schemaVersion, 1);
for (const [key, value] of Object.entries(result)) {
  if (key !== 'schemaVersion' && key !== 'manifestChannels')
    assert.strictEqual(value, true, key);
}
assert(result.manifestChannels > 0);
process.stdout.write(`${JSON.stringify(result)}\n`);
