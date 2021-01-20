// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';

// Helpers
import StoryDecorator from '../../_support/StoryDecorator';

// Screens
import WalletTokenSendConfirmationDialog
  from '../../../../source/renderer/app/components/wallet/WalletTokenSendConfirmationDialog';

storiesOf('Wallet Token Dialog|Send Confirmation', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Wallet Token Send Confirmation Dialog', () => (
    <div>
      <WalletTokenSendConfirmationDialog
        amount={amount}
        receiver={receiver}
        totalAmount={totalAmount}
        transactionFee={transactionFee}
        amountToNaturalUnits={amountToNaturalUnits}
        onSubmit={this.handleWalletSendFormSubmit}
        isSubmitting={isSubmitting}
        isFlight={isFlight}
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
          sendMoneyRequest.reset();
          resetHardwareWalletTransaction({ cancelDeviceAction: true });
        }}
        error={error}
        currencyUnit={currencyUnit}
        onExternalLinkClick={onExternalLinkClick}
        hwDeviceStatus={hwDeviceStatus}
        isHardwareWallet={isHardwareWallet}
        onInitiateTransaction={this.handleInitiateTransaction}
        walletName={activeWallet.name}
        nativeTokens={nativeTokens}
      />
    </div>
  ));
