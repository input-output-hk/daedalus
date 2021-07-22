// @flow
import React, { useState } from 'react';
import classnames from 'classnames';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import { observer } from 'mobx-react';
import styles from './DappTransactionRequest.scss';
import Dialog from '../widgets/Dialog';
import globalMessages from '../../i18n/global-messages';
import Wallet from '../../domains/Wallet';
import WalletsDropdown from '../widgets/forms/WalletsDropdown';

const messages = defineMessages({
  title: {
    id: 'dapp.transaction.request.title',
    defaultMessage: '!!!Transaction request',
    description: '"title" in the dApp transaction request dialog',
  },
  subtitle: {
    id: 'dapp.transaction.request.subtitle',
    defaultMessage: '!!!triggered from {triggedFrom}',
    description: '"subtitle" in the dApp transaction request dialog',
  },
});

type Props = {
  intl: intlShape.isRequired,
  onClose: Function,
  onSelectWallet: Function,
  onSubmit: Function,
  selectedWallet: ?Wallet,
  triggedFrom: string,
  wallets: Array<Wallet>,
};

// type SelectedWallet = ?Wallet;

const DappTransactionRequest = observer((props: Props) => {
  // const [selectedWallet, setSelectedWallet] = useState<SelectedWallet>(null);
  const {
    intl,
    onClose,
    onSelectWallet,
    onSubmit,
    selectedWallet,
    triggedFrom,
    wallets,
  } = props;
  const actions = [
    {
      label: intl.formatMessage(globalMessages.cancel),
      onClick: onClose,
    },
    {
      label: intl.formatMessage(globalMessages.dialogButtonContinueLabel),
      primary: true,
      onClick: onSubmit,
    },
  ];
  const componentStyles = classnames([styles.component]);

  const walletsOptions = wallets;

  return (
    <Dialog
      className={componentStyles}
      title={intl.formatMessage(messages.title)}
      subtitle={intl.formatMessage(messages.subtitle, { triggedFrom })}
      actions={actions}
    >
      <WalletsDropdown
        getStakePoolById={() => {}}
        numberOfStakePools={100}
        wallets={walletsOptions}
        onChange={onSelectWallet}
        placeholder="!!!Select a wallet"
        value={selectedWallet ? selectedWallet.id : null}
      />
    </Dialog>
  );
});

export default injectIntl(DappTransactionRequest);
