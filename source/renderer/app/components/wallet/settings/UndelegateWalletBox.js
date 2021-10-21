// @flow
import type { Node } from 'react';
import React, { useCallback } from 'react';
import { injectIntl, intlShape } from 'react-intl';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './WalletSettings.scss';
import UndelegateWalletButton from './UndelegateWalletButton';
import DelegateWalletButton from './DelegateWalletButton';
import UndelegateWalletConfirmationDialog from './UndelegateWalletConfirmationDialog';
import getWalletSettingsMessages from './WalletSettings.messages';

type Props = {
  intl: intlShape.isRequired,
  isDelegating: boolean,
  isDialogOpen: Function,
  isRestoring: boolean,
  isSyncing: boolean,
  onBlockForm: Function,
  onDelegateClick: Function,
  openDialogAction: Function,
  undelegateWalletDialogContainer: Node,
  updateDataForActiveDialogAction: Function,
  walletId: string,
};

const UndelegateWalletBox = ({
  intl,
  isDelegating,
  isRestoring,
  isSyncing,
  isDialogOpen,
  undelegateWalletDialogContainer,
  walletId,
  onBlockForm,
  onDelegateClick,
  openDialogAction,
  updateDataForActiveDialogAction,
}: Props): Node => {
  const messages = getWalletSettingsMessages();

  const headerMessage = isDelegating
    ? intl.formatMessage(messages.undelegateWalletHeader)
    : intl.formatMessage(messages.delegateWalletHeader);
  let warningMessage = null;
  if (isDelegating) {
    warningMessage =
      isRestoring || isSyncing
        ? intl.formatMessage(messages.undelegateWalletDisabledWarning)
        : intl.formatMessage(messages.undelegateWalletWarning);
  } else {
    warningMessage =
      isRestoring || isSyncing
        ? intl.formatMessage(messages.delegateWalletDisabledWarning)
        : intl.formatMessage(messages.delegateWalletWarning);
  }

  const onUndelegateWalletClick = useCallback(async () => {
    onBlockForm();
    openDialogAction({
      dialog: UndelegateWalletConfirmationDialog,
    });
    updateDataForActiveDialogAction({
      data: { walletId },
    });
  });

  return (
    <>
      <BorderedBox className={styles.undelegateWalletBox}>
        <span>{headerMessage}</span>
        <div className={styles.contentBox}>
          <div>
            <p>{warningMessage}</p>
          </div>
          {isDelegating ? (
            <UndelegateWalletButton
              disabled={isRestoring || isSyncing}
              onUndelegate={onUndelegateWalletClick}
            />
          ) : (
            <DelegateWalletButton
              disabled={isRestoring || isSyncing}
              onDelegate={onDelegateClick}
            />
          )}
        </div>
      </BorderedBox>
      {isDialogOpen(UndelegateWalletConfirmationDialog)
        ? undelegateWalletDialogContainer
        : false}
    </>
  );
};

export default injectIntl(UndelegateWalletBox);
