// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { boolean, number } from '@storybook/addon-knobs';
import WalletsWrapper from '../_utils/WalletsWrapper';
import { exportedByronWallets } from '../_utils/wallets';
import VerticalFlexContainer from '../../../../source/renderer/app/components/layout/VerticalFlexContainer';
import WalletImportFileDialog from '../../../../source/renderer/app/components/wallet/wallet-import/WalletImportFileDialog';
import WalletSelectImportDialog from '../../../../source/renderer/app/components/wallet/wallet-import/WalletSelectImportDialog';
import { isValidWalletName } from '../../../../source/renderer/app/utils/validations';

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
  .add('Step 2 - Wallets', () => (
    <VerticalFlexContainer>
      <WalletSelectImportDialog
        isSubmitting={boolean('isSubmitting', false)}
        nameValidator={name => isValidWalletName(name)}
        exportedWallets={exportedByronWallets}
        pendingImportWalletsCount={number('pendingImportWalletsCount', 0)}
        onConfirm={action('onConfirm')}
        onWalletNameChange={action('onWalletNameChange')}
        onToggleWalletImportSelection={action('onToggleWalletImportSelection')}
        onClose={action('onClose')}
      />
    </VerticalFlexContainer>
  ));
