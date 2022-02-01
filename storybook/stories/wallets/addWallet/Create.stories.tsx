import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import CreateWalletScreens from '../_utils/CreateWalletScreens';
import WalletCreateDialog from '../../../../source/renderer/app/components/wallet/WalletCreateDialog';
// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';

storiesOf('Wallets|Add Wallet', module)
  .addDecorator(WalletsWrapper)
  .add('Create - New process', () => <CreateWalletScreens />)
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ locale }: { locale: string; }... Remove this comment to see the full error message
  .add('Create - Old process', ({ locale }: { locale: string }) => {
    return (
      <WalletCreateDialog
        onSubmit={action('onSubmit')}
        onCancel={action('onCancel')}
        currentLocale={locale}
      />
    );
  });
