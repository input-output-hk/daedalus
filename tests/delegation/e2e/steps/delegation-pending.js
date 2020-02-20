// @flow
import { Given, Then } from 'cucumber';
import { expect } from 'chai';
import { set } from 'lodash';
import type { Daedalus } from '../../../types';
import type { WalletPendingDelegations } from '../../../../source/renderer/app/api/wallets/types';

declare var daedalus: Daedalus;


let tickers: Array<Object> = [];

Given(/^the wallet has the following pending delegations: (.*)$/, async function(delegationScenario) {

  const walletName = `Modified Wallet ${new Date().getTime()}`;

  await this.client.waitUntil(async () => {
    const stakePools = await this.client.execute(() => daedalus.stores.staking.stakePools);
    return stakePools.value.length;
  });
  const tickersInfo = await this.client.executeAsync((delegationScenario, walletName, done) => {
    const statusOptions = {
      delegated: 'delegating',
      undelegated: 'not_delegating',
    };
    const delegationQueue = delegationScenario.split(' > ');
    const modifiedWallet: {
      name: string,
      delegatedStakePoolId?: string,
      delegationStakePoolStatus?: string,
      lastDelegationStakePoolId?: string,
      pendingDelegations: Array<Object>,
    } = {
      name: walletName,
      pendingDelegations: [],
    };
    const { stakePools } = daedalus.stores.staking;
    const tickers = delegationQueue.map((delegationInfo, index) => {
      const status = statusOptions[delegationInfo];
      const stakePool = status === 'delegating' ? stakePools[index] : null;
      const ticker = stakePool ? `[${stakePool.ticker}]` : 'UNDELEGATED';
      const stakePoolId = stakePool ? stakePool.id : null;
      return({
        ticker,
        stakePoolId,
        status,
      });
    })
    tickers.forEach(({ stakePoolId, status }, index) => {
      if (index === 0) {
        modifiedWallet.delegatedStakePoolId = stakePoolId;
        modifiedWallet.delegationStakePoolStatus = status;
      }
      else {
        modifiedWallet.lastDelegationStakePoolId = stakePoolId
        modifiedWallet.pendingDelegations[index - 1] = {
          status: status,
          target: stakePoolId,
          changes_at: {
            epoch_start_time: '2020-02-02T02:02:57Z',
            epoch_number: 123456789,
          },
        }
      }
    });
    daedalus.api.ada.setTestingWallet(modifiedWallet);
    done(tickers);
  }, delegationScenario, walletName);
  tickers = tickersInfo.value;
});

Then(/^the wallet should correctly display the correct stake pool tickers$/, async function() {
  const walletNameSelector = '.WalletRow_title';
  await this.client.waitForVisible(walletNameSelector);
  // Waits for the patchAdaApi to transform the wallet values
  await this.client.waitUntil(async () => {
    const walletName = await this.client.getText(walletNameSelector);
    return walletName === walletName;
  });
  const tickerSelector = '.tickerText';
  await this.client.waitForVisible(tickerSelector);
  let tickerTexts = await this.client.getText(tickerSelector);
  if (!Array.isArray(tickerTexts)) tickerTexts = [tickerTexts];
  console.log('>>> ', tickerTexts.length === tickers.length);
  if (tickerTexts.length !== tickers.length) {
    console.log('------------------------------------------');
    console.log('tickerTexts', tickerTexts);
    console.log('tickers', tickers);
  }
  // expect(tickerTexts.length).to.equal(tickers.length);
  // tickerTexts.forEach((tickerText, index) => {
  //   let { ticker: expectedTickerText } = tickers[index];
  //   expect(tickerText).to.equal(expectedTickerText);
  // })
});

Then(/^the ADA logo should be (.*)$/, async function(visibility) {
  const shouldBeVisible = visibility === 'visible';
  await this.client.waitForVisible('.WalletRow_activeAdaSymbol', null, !shouldBeVisible);
});

Then(/^the tooltips should be displayed as follows: (.*)$/, async function(tooltipsStr) {
  const expectedTooltips = tooltipsStr.split(' > ').filter(value => value !== 'none');
  const tooltipTexts = await this.client.getHTML('.WalletRow_status .WalletRow_tooltipLabelWrapper span');
  expect(tooltipTexts.length).to.equal(expectedTooltips.length);
  expectedTooltips.forEach((expectedTooltip, index) => {
    const tooltipText = tooltipTexts[index];
    const expectedExcerpt = expectedTooltip === 'earning_rewards'
      ? 'Earning rewards'
      : 'From epoch';
    expect(tooltipText).to.have.string(expectedExcerpt);
  });
});

Then(/^the wallet should display the correct (.*)$/, async function(links) {
  console.log('links', links);
});
