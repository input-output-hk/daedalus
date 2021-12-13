import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import WalletsWrapper from '../_utils/WalletsWrapper';
import ExportWalletToFileDialog from '../../../../source/renderer/app/components/wallet/settings/ExportWalletToFileDialog';

storiesOf('Wallets|Export to File', module)
  .addDecorator(WalletsWrapper) // ====== Stories ======
  .add('default', () => (
    <div>
      <ExportWalletToFileDialog
        walletName="Test Wallet"
        isSubmitting={false}
        // @ts-ignore ts-migrate(2322) FIXME: Type 'HandlerFunction' is not assignable to type '... Remove this comment to see the full error message
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ))
  .add('submitting', () => (
    <div>
      <ExportWalletToFileDialog
        walletName="Test Wallet"
        isSubmitting
        // @ts-ignore ts-migrate(2322) FIXME: Type 'HandlerFunction' is not assignable to type '... Remove this comment to see the full error message
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ))
  .add('spending password', () => (
    <div>
      <ExportWalletToFileDialog
        walletName="Test Wallet"
        isSubmitting={false}
        // @ts-ignore ts-migrate(2322) FIXME: Type 'HandlerFunction' is not assignable to type '... Remove this comment to see the full error message
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ));
