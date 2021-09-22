// @flow
import type { Node } from 'react';
import React from 'react';
import { observer } from 'mobx-react';
import {
  defineMessages,
  FormattedHTMLMessage,
  injectIntl,
  intlShape,
} from 'react-intl';
import styles from './WalletSettings.scss';
import type { ReactIntlMessage } from '../../../types/i18nTypes';
import BorderedBox from '../../widgets/BorderedBox';
import WalletSettingsActionButton from './WalletSettingsActionButton';
import WalletSettingsActionConfirmationDialog from './WalletSettingsActionConfirmationDialog';

export const messages: { [string]: ReactIntlMessage } = defineMessages({
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
  openDialogAction: Function,
  isDialogOpen: Function,
  unpairWalletDialogContainer: Node,
  onBlockForm: Function,
  intl: intlShape.isRequired,
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
          ? unpairWalletDialogContainer
          : false}
      </>
    );
  }
);

export default injectIntl(UnpairWallet);
