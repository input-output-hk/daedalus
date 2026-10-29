'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path').win32;
const vm = require('node:vm');
const source = fs.readFileSync(process.argv[2], 'utf8');
const root = 'C:\\Program Files\\Daedalus Pilot';
function launch(profile, overrides = {}) {
  const entry = path.join(root, 'config', 'profiles', profile, 'frontend');
  const calls = [];
  const context = {
    process: {
      resourcesPath: path.join(root, 'resources'),
      argv: ['Daedalus Pilot.exe', entry],
      env: {DAEDALUS_CLUSTER: profile, DAEDALUS_CONFIG_FILE: path.join(path.dirname(entry), 'daedalus-config.json')},
      ...overrides,
    },
    require(name) {
      if (name === 'path') return path;
      if (name === 'electron') return {
        app: {setName: value => calls.push(['name', value]), setAppPath: value => calls.push(['path', value]), exit: code => calls.push(['exit', code])},
        dialog: {showErrorBox: () => calls.push(['repair'])},
      };
      if (name === path.join(entry, 'package.json')) return {productName: `name:${profile}`};
      if (name === path.join(entry, 'bootstrap.cjs')) {calls.push(['load', entry]); return;}
      throw Error(`Unexpected module ${name}`);
    },
  };
  vm.runInNewContext(source, context);
  return calls;
}
for (const profile of ['preview', 'preprod', 'mainnet-flight']) {
  const calls = launch(profile);
  assert.equal(calls[0][1], `name:${profile}`);
  assert.equal(calls.at(-1)[0], 'load');
}
for (const overrides of [
  {argv: ['Daedalus Pilot.exe']},
  {argv: ['Daedalus Pilot.exe', 'C:\\untrusted\\frontend']},
  {env: {}},
  {env: {DAEDALUS_CLUSTER: 'preview', DAEDALUS_CONFIG_FILE: path.join(root, 'config/profiles/preprod/daedalus-config.json')}},
]) assert.deepEqual(launch('preview', overrides), [['repair'], ['exit', 1]]);
assert.deepEqual(launch('mainnet'), [['repair'], ['exit', 1]]);
console.log('Pilot Windows bootstrap: three explicit entries and rejection cases passed');
if (process.argv[3]) for (const profile of ['preview', 'preprod', 'mainnet-flight']) {
  let handler, identity, windowDetails, windowIcon, loaded = false;
  const generated = fs.readFileSync(require('node:path').join(process.argv[3], 'config/profiles', profile, 'frontend/bootstrap.cjs'), 'utf8');
  vm.runInNewContext(generated, {
    process: {resourcesPath: path.join(root, 'resources')},
    require(name) {
      if (name === 'path') return path;
      if (name === 'fs') return {existsSync: () => false};
      if (name === 'electron') return {app: {
        setAppUserModelId: value => {identity = value;},
        getPath: key => `C:\\User\\${key}`,
        on: (event, listener) => {assert.equal(event, 'browser-window-created'); handler = listener;},
      }, shell: {}};
      assert.equal(name, path.join(root, 'resources/shared-app/dist/main/index.js'));
      loaded = true;
    },
  });
  handler({}, {setIcon: value => {windowIcon = value;}, setAppDetails: value => {windowDetails = value;}});
  assert.equal(identity, `org.daedalus.pilot.${profile}`);
  assert.equal(windowDetails.appId, identity);
  assert.equal(windowIcon, path.join(root, 'icons', `${profile}.ico`));
  assert.equal(windowDetails.appIconPath, windowIcon);
  assert.equal(windowDetails.relaunchCommand, `"${path.join(root, 'cardano-watchdog.exe')}" --config "${path.join(root, 'config/profiles', profile, 'daedalus-config.json')}"`);
  assert.ok(loaded);
}
console.log('Generated Windows identities, icons and watchdog taskbar relaunch passed');
