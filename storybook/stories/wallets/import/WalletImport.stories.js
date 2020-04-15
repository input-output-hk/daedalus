// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import WalletsWrapper from '../_utils/WalletsWrapper';
import VerticalFlexContainer from '../../../../source/renderer/app/components/layout/VerticalFlexContainer';
import WalletImportFileDialog from '../../../../source/renderer/app/components/wallet/wallet-import/WalletImportFileDialog';

storiesOf('Wallets|Import File', module)
  .addDecorator(WalletsWrapper)
  .add('Wallets Import File Dialog', () => (
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
  ));
