import { Given, Then } from "cucumber";
import { expect } from "chai";
import { last } from "lodash";

let walletsTickers = [];
Given(/^the wallets have the following pending delegations:$/, async function (delegationScenariosTable) {
  const delegationScenarios = delegationScenariosTable.hashes();
  await this.client.waitUntil(async () => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    const stakePools = await this.client.execute(() => daedalus.stores.staking.stakePools);
    return stakePools.value.length;
  });
  const walletsTickersInfo = await this.client.executeAsync((delegationScenarios, done) => {
    const statusOptions = {
      delegated: 'delegating',
      undelegated: 'not_delegating'
    };
    const walletsTickers = [];
    const modifiedWallets = [];

    for (let index = 0; index < delegationScenarios.length; index++) {
      const delegationQueue = delegationScenarios[index].DELEGATION_SCENARIO.split(' > ');
      const modifiedWallet: {
        name: string;
        delegatedStakePoolId?: string;
        delegationStakePoolStatus?: string;
        lastDelegationStakePoolId?: string;
        pendingDelegations: Array<Record<string, any>>;
        syncState: Record<string, any>;
      } = {
        name: `Modified Wallet ${index + 1}`,
        pendingDelegations: [],
        syncState: {
          status: 'ready'
        }
      };
      const {
        stakePools
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      } = daedalus.stores.staking;
      const tickers = delegationQueue.map((delegationInfo, index) => {
        const status = statusOptions[delegationInfo];
        const stakePool = status === 'delegating' ? stakePools[index] : null;
        const ticker = stakePool ? `[${stakePool.ticker}]` : 'UNDELEGATED';
        const stakePoolId = stakePool ? stakePool.id : null;
        return {
          status,
          ticker,
          stakePoolId
        };
      });
      walletsTickers.push(tickers);
      tickers.forEach(({
        stakePoolId,
        status
      }, index) => {
        if (index === 0) {
          modifiedWallet.delegatedStakePoolId = stakePoolId;
          modifiedWallet.delegationStakePoolStatus = status;
        } else {
          modifiedWallet.lastDelegationStakePoolId = stakePoolId;
          modifiedWallet.pendingDelegations[index - 1] = {
            status,
            target: stakePoolId,
            changes_at: {
              epoch_start_time: '2020-02-02T02:02:57Z',
              epoch_number: 123456789
            }
          };
        }
      });
      modifiedWallets.push(modifiedWallet);
    }

    // @ts-ignore
    daedalus.api.ada.setTestingWallets(modifiedWallets);
    done(walletsTickers);
  }, delegationScenarios);
  walletsTickers = walletsTickersInfo.value;
});
Then(/^the wallets should correctly display the correct stake pool tickers$/, {
  timeout: 60000
}, async function () {
  const walletNameSelector = '.WalletRow_title';
  await this.client.waitForVisible(walletNameSelector);
  // Waits for the patchAdaApi to transform the wallet values
  await this.client.waitUntil(async () => {
    const walletNames = await this.waitAndGetText(walletNameSelector);
    return last(walletNames) === `Modified Wallet ${walletsTickers.length}`;
  });
  const tickerSelector = '.tickerText';
  await this.client.waitForVisible(tickerSelector);

  for (let index = 0; index < walletsTickers.length; index++) {
    const expectedTickers = walletsTickers[index];
    let tickerTexts = await this.waitAndGetText(`.WalletRow_component:nth-child(${index + 1}) ${tickerSelector}`);
    if (!Array.isArray(tickerTexts)) tickerTexts = [tickerTexts];
    expect(tickerTexts.length).to.equal(expectedTickers.length);
    tickerTexts.forEach((tickerText, index) => {
      const {
        ticker: expectedTickerText
      } = expectedTickers[index];
      expect(tickerText).to.equal(expectedTickerText);
    });
  }
});
Then(/^the ADA logo should be displayed as follows:$/, async function (visibilityTable) {
  const visibility = visibilityTable.hashes();

  for (let index = 0; index < visibility.length; index++) {
    const shouldBeVisible = visibility[index].ADA_LOGO === 'visible';
    await this.client.waitForVisible(`.WalletRow_component:nth-child(${index + 1}) .WalletRow_activeAdaSymbol`, null, !shouldBeVisible);
  }
});
Then(/^the tooltips should be displayed as follows:$/, async function (tooltipsTable) {
  const tooltipsScenarios = tooltipsTable.hashes();

  for (let index = 0; index < tooltipsScenarios.length; index++) {
    const expectedTooltips = tooltipsScenarios[index].TOOLTIPS.split(' > ').filter(value => value !== 'none');

    if (expectedTooltips.length) {
      let tooltipTexts = await this.client.getHTML(`.WalletRow_component:nth-child(${index + 1}) .WalletRow_status .WalletRow_tooltipLabelWrapper span`);
      if (!Array.isArray(tooltipTexts)) tooltipTexts = [tooltipTexts];
      expect(tooltipTexts.length).to.equal(expectedTooltips.length);
      expectedTooltips.forEach((expectedTooltip, index) => {
        const tooltipText = tooltipTexts[index];
        const expectedExcerpt = expectedTooltip === 'earning_rewards' ? 'Earning rewards' : 'From epoch';
        expect(tooltipText).to.have.string(expectedExcerpt);
      });
    }
  }
});
Then(/^the action links should be displayed as follows:$/, async function (linksTable) {
  const linksScenarios = linksTable.hashes();

  for (let index = 0; index < linksScenarios.length; index++) {
    const expectedLinkExcerpts = linksScenarios[index].LINKS.split(' or ');
    const linksHTML = await this.client.getHTML(`.WalletRow_component:nth-child(${index + 1}) .WalletRow_action`);
    expectedLinkExcerpts.forEach(expectedLinkExcerpt => {
      expect(linksHTML).to.have.string(expectedLinkExcerpt);
    });
  }
});