import React from 'react';
import { boolean } from '@storybook/addon-knobs';
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

export default {
  title: 'Wallets / Add Wallet',
};

export const Add = () => (
  // @ts-ignore ts-migrate(2322) FIXME: Type '{ alignItems: string; backgroundColor: strin... Remove this comment to see the full error message
  <div style={wrapperStyles}>
    <WalletAdd
      onCreate={() => {}}
      onRestore={() => {}}
      onImport={() => {}}
      onConnect={() => {}}
      isMainnet={boolean('isMainnet', true)}
      isTestnet={boolean('isTestnet', false)}
      isProduction
      isMaxNumberOfWalletsReached={boolean(
        'isMaxNumberOfWalletsReached',
        false
      )}
    />
  </div>
);
