// @flow
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

const getWallet = (index: number, hasName: boolean, statusSelect?: Object) => ({
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
        exportErrors=""
        onConfirm={action('onConfirm')}
        onClose={action('onClose')}
        onOpenExternalLink={action('onOpenExternalLink')}
        onSelectExportSourcePath={action('onSelectExportSourcePath')}
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
    ].map((x, index: number) => getWallet(index, true, statusSelect));
    const unnamedWallets = [
      ...Array(number('Wallets with no name', 5)),
    ].map((x, index: number) => getWallet(index, false));
    return (
      <VerticalFlexContainer>
        <WalletSelectImportDialog
          isSubmitting={boolean('isSubmitting', false)}
          nameValidator={name => isValidWalletName(name)}
          // $FlowFixMe
          exportedWallets={[...namedWallets, ...unnamedWallets]}
          pendingImportWalletsCount={number('pendingImportWalletsCount', 0)}
          onConfirm={action('onConfirm')}
          onWalletNameChange={action('onWalletNameChange')}
          onToggleWalletImportSelection={action(
            'onToggleWalletImportSelection'
          )}
          onClose={action('onClose')}
        />
      </VerticalFlexContainer>
    );
  });
