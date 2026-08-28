import fs from 'fs';
import os from 'os';
import path from 'path';
import { After, Given, Then, When } from 'cucumber';
import { expect } from 'chai';

import { GrantRepository } from '../../../../source/main/cip30/GrantRepository';
import { DappRouteLeaseService } from '../../../../source/main/dapp/DappRouteLease';

After(function () {
  if (this.context.dappGrantDirectory)
    fs.rmSync(this.context.dappGrantDirectory, {
      recursive: true,
      force: true,
    });
});

Given(
  /^dApp route authority is configured for the current network$/,
  function () {
    this.context.routeService = new DappRouteLeaseService('11'.repeat(32));
  }
);

When(/^the trusted UI opens the dApp route for wallet "([^"]*)"$/, function (
  walletId
) {
  const current = this.context.routeService.observeTrustedRoute(
    `file:///app/index.html#/wallets/${encodeURIComponent(walletId)}/dapps`
  );
  if (!this.context.firstRoute) this.context.firstRoute = current;
});

Then(/^the active dApp route is bound to wallet "([^"]*)"$/, function (
  walletId
) {
  expect(this.context.routeService.current?.walletId).to.equal(walletId);
});

Then(/^the previous dApp route authority is stale$/, function () {
  expect(
    this.context.routeService.isCurrent(this.context.firstRoute)
  ).to.be.false;
});

Given(
  /^a read grant exists for wallet "([^"]*)" and origin "([^"]*)"$/,
  function (walletId, origin) {
    const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'daedalus-dapp-'));
    const file = path.join(directory, 'grants.json');
    const repository = new GrantRepository(file);
    repository.put({
      origin,
      walletId,
      networkGenesis: '11'.repeat(32),
      networkMagic: 764824073,
      readScopes: ['connection', 'read'],
      enabledExtensionScopes: [95],
      launch: { kind: 'diagnostics' },
      grantedAt: '2026-08-28T00:00:00.000Z',
    });
    this.context.dappGrantDirectory = directory;
    this.context.dappGrantFile = file;
  }
);

When(/^the grant repository restarts$/, function () {
  this.context.restartedGrantRepository = new GrantRepository(
    this.context.dappGrantFile
  );
});

Then(
  /^the read grant is reusable for wallet "([^"]*)" and origin "([^"]*)"$/,
  function (walletId, origin) {
    expect(
      this.context.restartedGrantRepository.find({
        origin,
        walletId,
        networkGenesis: '11'.repeat(32),
        launch: { kind: 'diagnostics' },
        scopes: ['connection', 'read'],
        extensions: [95],
      })
    ).to.include({ walletId, origin });
  }
);
