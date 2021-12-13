import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { boolean, number, select } from '@storybook/addon-knobs';
import WalletsWrapper from '../_utils/WalletsWrapper';
import VerticalFlexContainer from '../../../../source/renderer/app/components/layout/VerticalFlexContainer';
import WalletImportFileDialog from '../../../../source/renderer/app/components/wallet/wallet-import/WalletImportFileDialog';
import WalletSelectImportDialog from '../../../../source/renderer/app/components/wallet/wallet-import/WalletSelectImportDialog';
import { isValidWalletName } from '../../../../source/renderer/app/utils/validations';
import { WalletImportStatuses } from '../../../../source/renderer/app/types/walletExportTypes';

const getWallet = (
  index: number,
  hasName: boolean,
  statusSelect?: Record<string, any>
) => ({
  encrypted_root_private_key: '',
  name: hasName ? `Wallet ${index}` : null,
  id: `wallet-${index}`,
  passphrase_hash: '',
  is_passphrase_empty: false,
  hasName,
  import: {
    status:
      index === 0 && hasName ? statusSelect : WalletImportStatuses.UNSTARTED,
    error: null,
  },
});

storiesOf('Wallets|Import File', module)
  .addDecorator(WalletsWrapper)
  .add('Step 1 - Import File', () => (
    <VerticalFlexContainer>
      <WalletImportFileDialog
        isSubmitting={false}
        exportSourcePath="/Users/daedalus/Library/Application Support/Daedalus"
        defaultExportSourcePath="/Users/daedalus/Library/Application Support/Daedalus"
        exportErrors=""
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        pendingImportWalletsCount={1}
        onOpen={action('onOpen')}
        onContinue={action('onContinue')}
        onClose={action('onClose')}
        onOpenExternalLink={action('onOpenExternalLink')}
        onSelectExportSourcePath={action('onSelectExportSourcePath')}
        onResetExportSourcePath={action('onResetExportSourcePath')}
      />
    </VerticalFlexContainer>
  ))
  .add('Step 2 - Wallets', () => {
    const statusSelect = select(
      '1st wallet status',
      WalletImportStatuses,
      WalletImportStatuses.PENDING
    );
    const namedWallets = [
      ...Array(number('Wallets with name', 5)),
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
    ].map((x, index: number) => getWallet(index, true, statusSelect));
    const unnamedWallets = [
      ...Array(number('Wallets with no name', 5)),
    ].map((x, index: number) => getWallet(index, false));
    return (
      <VerticalFlexContainer>
        <WalletSelectImportDialog
          isSubmitting={boolean('isSubmitting', false)}
          nameValidator={(name) => isValidWalletName(name)} // @ts-ignore
          exportedWallets={[...namedWallets, ...unnamedWallets]}
          pendingImportWalletsCount={number('pendingImportWalletsCount', 0)}
          onContinue={action('onContinue')}
          onOpenExternalLink={action('onOpenExternalLink')}
          onWalletNameChange={action('onWalletNameChange')}
          onToggleWalletImportSelection={action(
            'onToggleWalletImportSelection'
          )}
          isMaxNumberOfWalletsReached={false}
          onClose={action('onClose')}
        />
      </VerticalFlexContainer>
    );
  });
