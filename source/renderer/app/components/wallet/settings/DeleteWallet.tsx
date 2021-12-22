// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import React from 'react';
import { observer } from 'mobx-react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletSettings.scss' or its ... Remove this comment to see the full error message
import styles from './WalletSettings.scss';
import type { ReactIntlMessage } from '../../../types/i18nTypes';
import BorderedBox from '../../widgets/BorderedBox';
import WalletSettingsRemoveButton from './WalletSettingsRemoveButton';
import WalletSettingsActionConfirmationDialog from './WalletSettingsRemoveConfirmationDialog';

export const messages: Record<string, ReactIntlMessage> = defineMessages({
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
  openDialogAction: (...args: Array<any>) => any;
  isDialogOpen: (...args: Array<any>) => any;
  deleteWalletDialogContainer: Node;
  onBlockForm: (...args: Array<any>) => any;
  intl: intlShape.isRequired;
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
            <WalletSettingsRemoveButton
              label={label}
              // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
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
