// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import React from 'react';
import { observer } from 'mobx-react';
import {
  defineMessages,
  FormattedHTMLMessage,
  injectIntl,
  intlShape,
} from 'react-intl';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletSettings.scss' or its ... Remove this comment to see the full error message
import styles from './WalletSettings.scss';
import type { ReactIntlMessage } from '../../../types/i18nTypes';
import BorderedBox from '../../widgets/BorderedBox';
import WalletSettingsRemoveButton from './WalletSettingsRemoveButton';
import WalletSettingsActionConfirmationDialog from './WalletSettingsRemoveConfirmationDialog';

export const messages: Record<string, ReactIntlMessage> = defineMessages({
  unpairWalletHeader: {
    id: 'wallet.settings.unpairWallet.header',
    defaultMessage: '!!!Unpair wallet',
    description: 'Unpair wallet header on the wallet settings page.',
  },
  unpairWalletWarning: {
    id: 'wallet.settings.unpairWallet.warning',
    defaultMessage:
      '!!!Once you unpair this wallet it will be removed from the Daedalus interface and you will lose access to any remaining funds in the wallet. The only way to regain access after deletion is by restoring it using your wallet recovery phrase.',
    description: 'Unpair wallet warning explaining the consequences.',
  },
  unpairButton: {
    id: 'wallet.settings.unpairWalletButtonLabel',
    defaultMessage: '!!!Unpair wallet',
    description: 'Label for the unpair button on wallet settings',
  },
});
type Props = {
  openDialogAction: (...args: Array<any>) => any;
  isDialogOpen: (...args: Array<any>) => any;
  unpairWalletDialogContainer: Node;
  onBlockForm: (...args: Array<any>) => any;
  intl: intlShape.isRequired;
};
const UnpairWallet = observer(
  ({
    openDialogAction,
    isDialogOpen,
    unpairWalletDialogContainer,
    onBlockForm,
    intl,
  }: Props) => {
    const label = intl.formatMessage(messages.unpairButton);
    return (
      <>
        <BorderedBox className={styles.unpairWalletBox}>
          <div className={styles.title}>
            {intl.formatMessage(messages.unpairWalletHeader)}
          </div>
          <div className={styles.contentBox}>
            <div>
              <p>
                <FormattedHTMLMessage {...messages.unpairWalletWarning} />
              </p>
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
          ? unpairWalletDialogContainer
          : false}
      </>
    );
  }
);
export default injectIntl(UnpairWallet);
