'use strict';

// The shortcut's watchdog configuration supplies an explicit installed frontend
// directory. Apply its desktop identity before loading the shared application;
// network and state selection remain in that configuration.
const { app, dialog } = require('electron');
const path = require('path');
const root = path.dirname(process.resourcesPath);
const entries = ['preview', 'preprod', 'mainnet-flight'].map((profile) =>
  path.join(root, 'config', 'profiles', profile, 'frontend')
);
const selections = process.argv.slice(1).filter((argument) =>
  entries.some((entry) => entry.toLowerCase() === argument.toLowerCase())
);
const selected = selections.length === 1 ? selections[0] : null;
const config = selected && path.join(path.dirname(selected), 'daedalus-config.json');
if (!selected || !process.env.DAEDALUS_CLUSTER ||
    config.toLowerCase() !== (process.env.DAEDALUS_CONFIG_FILE || '').toLowerCase()) {
  dialog.showErrorBox(
    'Start Daedalus using a shortcut',
    'Choose Preview, Pre-Prod or Flight from the installed Daedalus Pilot shortcuts. Reinstall Pilot if the shortcuts are missing.'
  );
  app.exit(1);
} else {
  const entry = entries.find((value) => value.toLowerCase() === selected.toLowerCase());
  const metadata = require(path.join(entry, 'package.json'));
  app.setName(metadata.productName);
  app.setAppPath(entry);
  require(path.join(entry, 'bootstrap.cjs'));
}
