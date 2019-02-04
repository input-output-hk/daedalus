// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { linkTo } from '@storybook/addon-links';
import { withKnobs, text, boolean, number } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import faker from 'faker';
import startCase from 'lodash/startCase';

// Assets and helpers
import StoryLayout from './support/StoryLayout';
import StoryProvider from './support/StoryProvider';
import StoryDecorator from './support/StoryDecorator';
import { generateWallet, generateTransaction, generateAddress, promise } from './support/utils';
import { formattedWalletAmount } from '../../source/renderer/app/utils/formatters';
import { transactionTypes } from '../../source/renderer/app/domains/WalletTransaction';
import WalletWithNavigation from '../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';

// Screens
import WalletSummary from '../../source/renderer/app/components/wallet/summary/WalletSummary';
import WalletSendForm from '../../source/renderer/app/components/wallet/WalletSendForm';
import WalletReceive from '../../source/renderer/app/components/wallet/receive/WalletReceive';
import WalletTransactionsList from '../../source/renderer/app/components/wallet/transactions/WalletTransactionsList';
import WalletSettings from '../../source/renderer/app/components/wallet/WalletSettings';
import { WalletAssuranceModeOptions } from '../../source/renderer/app/domains/Wallet';
import ChangeSpendingPasswordDialog from '../../source/renderer/app/components/wallet/settings/ChangeSpendingPasswordDialog';
import DeleteWalletConfirmationDialog from '../../source/renderer/app/components/wallet/settings/DeleteWalletConfirmationDialog';
import ExportWalletToFileDialog from '../../source/renderer/app/components/wallet/settings/ExportWalletToFileDialog';

storiesOf('WalletScreens', module)

  .addDecorator((story, context) => {

    const storyWithKnobs = withKnobs(story, context);

    return (
      <StoryDecorator>
        <StoryProvider>
          <StoryLayout
            activeSidebarCategory="/wallets"
            storyName={context.story}
          >
            {
              context.story !== 'Empty'
                ? (
                  <WalletWithNavigation
                    isActiveScreen={item => item === context.story.toLocaleLowerCase()}
                    onWalletNavItemClick={linkTo('WalletScreens', item => startCase(item))}
                  >
                    {storyWithKnobs}
                  </WalletWithNavigation>
                )
                : storyWithKnobs
            }
          </StoryLayout>
        </StoryProvider>
      </StoryDecorator>
    );
  })

  // ====== Stories ======

  .add('Empty', () => null)

  .add('Wallet Navigation', () => (
    <div>&nbsp;</div>
  ))

  .add('Summary', () => (
    <WalletSummary
      wallet={generateWallet('Wallet name', '45119903750165')}
      pendingAmount={{
        incoming: new BigNumber(number('Incoming', 1)),
        outgoing: new BigNumber(number('Outgoing', 2)),
        total: new BigNumber(3)
      }}
      numberOfTransactions={number('Number of transactions', 20303585)}
      isLoadingTransactions={boolean('isLoadingTransactions', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
    />
  ))

  .add('Send', () => (
    <WalletSendForm
      currencyUnit="Ada"
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      validateAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      addressValidator={() => {}}
      openDialogAction={() => {}}
      isDialogOpen={() => boolean('hasDialog', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
    />
  ))

  .add('Receive', () => (
    <WalletReceive
      walletAddress={text('Wallet address', '5628aab8ac98c963e4a2e8cfce5aa1cbd4384fe2f9a0f3c5f791bfb83a5e02ds')}
      isWalletAddressUsed={boolean('isWalletAddressUsed', false)}
      walletAddresses={[
        ...Array.from(Array(number('Addresses', 1))).map(() => generateAddress()),
        ...Array.from(Array(number('Addresses (used)', 1))).map(() => generateAddress(true)),
      ]}
      onGenerateAddress={() => {}}
      onCopyAddress={() => {}}
      isSidebarExpanded
      walletHasPassword={boolean('walletHasPassword', false)}
      isSubmitting={boolean('isSubmitting', false)}
    />
  ))

  .add('Transactions', () => (
    <WalletTransactionsList
      transactions={
        [
          ...Array.from(Array(number('Transactions Sent', 1))).map((x, i) => (
            generateTransaction(
              transactionTypes.EXPEND,
              moment().subtract(i, 'days').toDate(),
              new BigNumber(faker.random.number(5))
            )
          )),
          ...Array.from(Array(number('Transactions Received', 1))).map((x, i) => (
            generateTransaction(
              transactionTypes.INCOME,
              moment().subtract(i, 'days').toDate(),
              new BigNumber(faker.random.number(5))
            )
          )),
        ]
      }
      isLoadingTransactions={boolean('isLoadingTransactions', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      hasMoreToLoad={false}
      assuranceMode={{ low: 1, medium: 2 }}
      walletId="test-wallet"
      formattedWalletAmount={formattedWalletAmount}
      totalAvailable={number('Transactions Sent', 1) + number('Transactions Received', 1)}
    />
  ))

  .add('Settings', () => (
    <WalletSettings
      activeField={null}
      assuranceLevels={[
        {
          value: WalletAssuranceModeOptions.NORMAL,
          label: {
            id: 'global.assuranceLevel.normal',
            defaultMessage: '!!!Normal',
            description: ''
          }
        },
        {
          value: WalletAssuranceModeOptions.STRICT,
          label: {
            id: 'global.assuranceLevel.strict',
            defaultMessage: '!!!Strict',
            description: ''
          }
        }
      ]}
      isDialogOpen={(dialog) => {
        if (dialog === ChangeSpendingPasswordDialog) {
          return boolean('Change Password - Show dialog', false);
        }
        if (dialog === DeleteWalletConfirmationDialog) {
          return boolean('Delete Wallet - Show dialog', false);
        }
        if (dialog === ExportWalletToFileDialog) {
          return boolean('Export Wallet - Show dialog', false);
        }
      }}
      isInvalid={false}
      isSubmitting={false}
      isSpendingPasswordSet={boolean('isSpendingPasswordSet', false)}
      lastUpdatedField={null}
      nameValidator={() => true}
      onCancelEditing={() => {}}
      onFieldValueChange={() => {}}
      onStartEditing={() => {}}
      onStopEditing={() => {}}
      openDialogAction={() => {}}
      walletAssurance={WalletAssuranceModeOptions.NORMAL}
      walletName={text('Wallet Name', 'Wallet Name')}
      spendingPasswordUpdateDate={moment().subtract(1, 'month').toDate()}
      changeSpendingPasswordDialog={(
        <ChangeSpendingPasswordDialog
          currentPasswordValue="current"
          newPasswordValue="new"
          repeatedPasswordValue="new"
          isSpendingPasswordSet={boolean('isSpendingPasswordSet', false)}
          onSave={action('Change Password - onSave')}
          onCancel={action('Change Password - onCancel')}
          onPasswordSwitchToggle={action('Change Password - onPasswordSwitchToggle')}
          onDataChange={action('Change Password - onDataChange')}
          isSubmitting={boolean('Change Password - isSubmitting', false)}
          error={null}
        />
      )}
      deleteWalletDialogContainer={(
        <DeleteWalletConfirmationDialog
          walletName={text('DeleteWalletConfirmationDialog: Wallet Name', 'Wallet To Delete')}
          hasWalletFunds={boolean('hasWalletFunds', false)}
          countdownFn={() => number('Delete Wallet Countdown', 9)}
          isBackupNoticeAccepted={boolean('isBackupNoticeAccepted', false)}
          onAcceptBackupNotice={action('Delete Wallet - onAcceptBackupNotice')}
          onContinue={action('Delete Wallet - onContinue')}
          onCancel={action('Delete Wallet - onCancel')}
          confirmationValue={text('Delete Wallet Confirmation Value')}
          onConfirmationValueChange={action('Delete Wallet - onConfirmationValueChange')}
          isSubmitting={boolean('Delete Wallet - isSubmitting', false)}
        />
      )}
      exportWalletDialogContainer={(
        <ExportWalletToFileDialog
          walletName={text('Wallet Name', 'Wallet Name')}
          hasSpendingPassword={boolean('isSpendingPasswordSet', false)}
          isSubmitting={boolean('Export Wallet - isSubmitting', false)}
          onSubmit={action('Export Wallet - onSubmit')}
          onClose={action('Export Wallet - onClose')}
        />
      )}
    />
  ));
