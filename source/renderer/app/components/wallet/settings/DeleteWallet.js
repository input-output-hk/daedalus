// @flow
import type { Node } from 'react';
import React from 'react';
import { observer } from 'mobx-react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import styles from './WalletSettings.scss';
import type { ReactIntlMessage } from '../../../types/i18nTypes';
import BorderedBox from '../../widgets/BorderedBox';
import WalletSettingsActionButton from './WalletSettingsActionButton';
import WalletSettingsActionConfirmationDialog from './WalletSettingsActionConfirmationDialog';

export const messages: { [string]: ReactIntlMessage } = defineMessages({
  deleteWalletHeader: {
    id: 'wallet.settings.deleteWallet.header',
    defaultMessage: '!!!Delete wallet',
    description: 'Delete wallet header on the wallet settings page.',
  },
  deleteWalletWarning1: {
    id: 'wallet.settings.deleteWallet.warning1',
    defaultMessage:
      '!!!Once you delete this wallet it will be removed from the Daedalus interface and you will lose access to any remaining funds in the wallet. The only way to regain access after deletion is by restoring it using your wallet recovery phrase.',
    description: 'Delete wallet warning explaining the consequences.',
  },
  deleteWalletWarning2: {
    id: 'wallet.settings.deleteWallet.warning2',
    defaultMessage:
      '!!!You may wish to verify your recovery phrase before deletion to ensure that you can restore this wallet in the future, if desired.',
    description: 'Delete wallet warning explaining the consequences.',
  },
  deleteButton: {
    id: 'wallet.settings.deleteWalletButtonLabel',
    defaultMessage: '!!!Delete wallet',
    description: 'Label for the delete button on wallet settings',
  },
});

type Props = {
  openDialogAction: Function,
  isDialogOpen: Function,
  deleteWalletDialogContainer: Node,
  onBlockForm: Function,
  intl: intlShape.isRequired,
};

const DeleteWallet = observer(
  ({
    openDialogAction,
    isDialogOpen,
    deleteWalletDialogContainer,
    onBlockForm,
    intl,
  }: Props) => {
    const label = intl.formatMessage(messages.deleteButton);

    return (
      <>
        <BorderedBox className={styles.deleteWalletBox}>
          <div className={styles.title}>
            {intl.formatMessage(messages.deleteWalletHeader)}
          </div>
          <div className={styles.contentBox}>
            <div>
              <p>{intl.formatMessage(messages.deleteWalletWarning1)}</p>
              <p>{intl.formatMessage(messages.deleteWalletWarning2)}</p>
            </div>
            <WalletSettingsActionButton
              label={label}
              onClick={React.useCallback(() => {
                onBlockForm();
                openDialogAction({
                  dialog: WalletSettingsActionConfirmationDialog,
                });
              })}
            />
          </div>
        </BorderedBox>
        {isDialogOpen(WalletSettingsActionConfirmationDialog)
          ? deleteWalletDialogContainer
          : false}
      </>
    );
  }
);

export default injectIntl(DeleteWallet);
