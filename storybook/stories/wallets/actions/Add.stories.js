// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { boolean } from '@storybook/addon-knobs';

// Assets and helpers
// import WalletsWrapper from '../utils/WalletsWrapper';

// Screens
import WalletAdd from '../../../../source/renderer/app/components/wallet/WalletAdd';

/* eslint-disable consistent-return */
storiesOf('Wallets|Actions', module)
  // .addDecorator(WalletsWrapper)

  // ====== Stories ======

  .add('Add', () => (
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
  ));
