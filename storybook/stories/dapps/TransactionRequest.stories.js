// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import BigNumber from 'bignumber.js';
import { action } from '@storybook/addon-actions';
import { withKnobs, select, number, boolean } from '@storybook/addon-knobs';
import { withState } from '@dump247/storybook-state';
import StoryDecorator from '../_support/StoryDecorator';
import DappTransactionRequest from '../../../source/renderer/app/components/dapp/DappTransactionRequest';
import Notification from '../../../source/renderer/app/components/notifications/Notification';
import { WALLETS_V2 } from '../_support/StoryProvider';
import { generateAssetToken } from '../_support/utils';

const allAssets = [
  generateAssetToken(
    '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'token1rjklcrnsdzqp65wjgrg55sy9723kw09m5z2345',
    50
  ),
  generateAssetToken(
    '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    '',
    'token1rjklcrnsdzqp65wjgrg55sy9723kw09m5z4567',
    50,
    {
      name: 'MakerDAO',
      ticker: 'DAI',
      description: 'Test description',
      unit: {
        name: 'DAI',
        decimals: 6,
      },
      url: 'http://example.com',
      logo: '',
    }
  ),
];

storiesOf('dApps|TransactionRequest', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add(
    'Request',
    withState({ selectedWallet: null }, (store) => {
      const { selectedWallet } = store.state;
      const wallets = boolean('Has wallets?', true)
        ? WALLETS_V2.map((wallet, index) => {
            let assetsList = allAssets;
            let { name } = wallet;
            if (index === 0) {
              assetsList = assetsList.slice(0, 1);
              name = `${wallet.name} - Missing token`;
            }
            if (index === 1) {
              name = `${wallet.name} - Insuficient Balance`;
            }
            const assets = {
              total: assetsList,
              available: assetsList,
            };
            return {
              ...wallet,
              assets,
              name,
            };
          })
        : [];
      const assetsAmounts = [...Array(allAssets.length)].map(
        (x, index) => new BigNumber(index + 10)
      );
      if (selectedWallet && selectedWallet.id === '2') {
        assetsAmounts[1] = new BigNumber(300);
      }
      return (
        <DappTransactionRequest
          address="addr1zCqrhsvWEPg886YEtnjN3vLXhFBHsc6j7oZ3pXzuwgZquGUT4fuztk43fHZnBhQKMnojvyxhFBHsc6j7oZ3pXzuwgZq"
          onAddWallet={action('onAddWallet')}
          onClose={action('onClose')}
          onSubmit={action('onSubmit')}
          onSelectWallet={(walletId) => {
            const newSelectedWallet = wallets.find(({ id }) => id === walletId);
            store.set({ selectedWallet: newSelectedWallet });
          }}
          selectedWallet={selectedWallet}
          triggeredFrom={select(
            'triggeredFrom',
            {
              safari: 'safari',
              chrome: 'chrome',
            },
            'safari'
          )}
          wallets={wallets}
          assets={allAssets}
          assetsAmounts={assetsAmounts}
          feesAmount={new BigNumber(number('feesAmount', 100))}
          transactionFee={new BigNumber(1)}
          additionalData={JSON.stringify(
            {
              appName: 'daedalus',
              action: 'convert',
              state: 'true',
            },
            null,
            2
          )}
          metadata={JSON.stringify(
            {
              id: 1569325866799,
              title: {
                'en-US': 'Incident 1 in English',
                'ja-JP': 'Incident 1 in Japanese',
              },
              content: {
                'en-US': '# h1 English incident content',
                'ja-JP': '# h1 Japanese incident content',
              },
              target: {
                daedalusVersion: '>=2.0.0-ITN1',
                platforms: ['darwin', 'win32', 'linux'],
              },
              action: {
                label: {
                  'en-US': 'Visit en-US',
                  'ja-JP': 'Visit ja-JP',
                },
                url: {
                  'en-US': 'https://iohk.zendesk.com/hc/en-us/articles/',
                  'ja-JP': 'https://iohk.zendesk.com/hc/ja/articles/',
                },
              },
              publishedAt: {
                'en-US': 1569325866799,
                'ja-JP': 1569325866799,
              },
              type: 'incident',
            },
            null,
            2
          )}
        />
      );
    })
  )
  .add('Notifications', ({ locale }: { locale: string }) => {
    let text1 = 'Opening transaction received via link...';
    let text2 = 'Transaction received via link';
    let actionBtn1 = 'Reject';
    let actionBtn2 = 'View';
    if (locale !== 'en-US') {
      text1 = 'リンク経由で受信したトランザクションを開いています...';
      text2 = 'リンク経由で受信したトランザクション';
      actionBtn1 = '拒否する';
      actionBtn2 = '表示する';
    }
    const actions = [
      {
        label: actionBtn1,
      },
      {
        label: actionBtn2,
        primary: true,
      },
    ];
    return (
      <div style={{ overflow: 'hidden', height: 200 }}>
        <div>
          <Notification isVisible clickToClose={false} hasCloseButton={false}>
            {text1}
          </Notification>
        </div>
        <div style={{ position: 'relative', marginTop: 72 }}>
          <Notification isVisible onClose={action('onClose')} actions={actions}>
            {text2}
          </Notification>
        </div>
      </div>
    );
  });
