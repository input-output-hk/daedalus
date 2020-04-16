// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { boolean } from '@storybook/addon-knobs';
import { isIncentivizedTestnetTheme } from '../../_support/utils';

// Screens
import WalletAdd from '../../../../source/renderer/app/components/wallet/WalletAdd';

const wrapperStyles = {
  alignItems: 'center',
  backgroundColor: 'var(--theme-main-body-background-color)',
  display: 'flex',
  flexDirection: 'column',
  height: '100%',
  justifyContent: 'center',
};
/* eslint-disable consistent-return */
storiesOf('Wallets|Add Wallet', module)
  // ====== Stories ======
  .add('Add', (props: { currentTheme: string }) => (
    <div style={wrapperStyles}>
      <WalletAdd
        onCreate={() => {}}
        onRestore={() => {}}
        onImportFile={() => {}}
        isMainnet={boolean('isMainnet', true)}
        isTestnet={boolean('isTestnet', false)}
        isProduction
        isIncentivizedTestnet={isIncentivizedTestnetTheme(props.currentTheme)}
        isMaxNumberOfWalletsReached={boolean(
          'isMaxNumberOfWalletsReached',
          false
        )}
      />
    </div>
  ));
