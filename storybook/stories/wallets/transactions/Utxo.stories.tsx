import React from 'react';
import { storiesOf } from '@storybook/react';
import { number } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
import { getUtxoChartData } from '../../../../source/renderer/app/utils/utxoUtils';
// Screens
import WalletUtxo from '../../../../source/renderer/app/components/wallet/utxo/WalletUtxo';

const cfg = {
  range: true,
  step: 1,
  min: 0,
  max: 20,
};

/* eslint-disable consistent-return */
storiesOf('Wallets|Transactions', module)
  .addDecorator(WalletsWrapper) // ====== Stories ======
  .add('UTXO Distribution', () => (
    <WalletUtxo
      walletAmount={
        new BigNumber(
          number('Amount', 66.998, {
            range: true,
            step: 1,
            min: 0,
            max: 9999,
          })
        )
      }
      walletUtxosAmount={number('UTXOs', 100, {
        range: true,
        step: 1,
        min: 0,
        max: 1000,
      })}
      chartData={getUtxoChartData({
        [10]: number('1. 0.00001', 0, cfg),
        [100]: number('2. 0.0001', 2, cfg),
        [1000]: number('3. 0.001', 0, cfg),
        [10000]: number('4. 0.01', 1, cfg),
        [100000]: number('5. 0.1', 0, cfg),
        [1000000]: number('6. 1', 0, cfg),
        [10000000]: number('7. 10', 0, cfg),
        [100000000]: number('8. 100', 0, cfg),
        [1000000000]: number('9. 1000', 0, cfg),
        [10000000000]: number('10. 10K', 0, cfg),
        [100000000000]: number('11. 10K+ - 100K', 0, cfg),
        [1000000000000]: number('12. 10K+ - 1M', 0, cfg),
        [10000000000000]: number('13. 10K+ - 10M', 0, cfg),
        [100000000000000]: number('14. 10K+ - 100M', 0, cfg),
        [1000000000000000]: number('15. 10K+ - 1B', 0, cfg),
        [10000000000000000]: number('16. 10K+ - 10B', 0, cfg),
        [45000000000000000]: number('17. 10K+ - 45B', 0, cfg),
      })}
      onExternalLinkClick={() => {}}
      pendingTxnsCount={0}
    />
  ));
