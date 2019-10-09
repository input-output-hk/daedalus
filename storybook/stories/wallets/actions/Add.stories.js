// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
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
/* eslint-disable consistent-return */
storiesOf('Wallets|Actions', module)
  // ====== Stories ======
  .add('Add', () => (
    <div style={wrapperStyles}>
      <WalletAdd
        onCreate={() => {}}
        onRestore={() => {}}
        onImportFile={() => {}}
        isRestoreActive={boolean('isRestoreActive', false)}
        isMainnet={boolean('isMainnet', false)}
        isTestnet={boolean('isTestnet', false)}
        isMaxNumberOfWalletsReached={boolean(
          'isMaxNumberOfWalletsReached',
          false
        )}
      />
    </div>
  ));
