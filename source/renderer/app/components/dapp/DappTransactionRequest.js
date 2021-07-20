// @flow
import React, { useState } from 'react';
import classnames from 'classnames';
import { differenceWith } from 'lodash';
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
  onSubmit: Function,
  triggedFrom: string,
  wallets: Array<Wallet>,
};

type SelectedWallets = Array<Wallet>;

const DappTransactionRequest = observer((props: Props) => {
  const [selectedWallets, setSelectedWallets] = useState<SelectedWallets>([]);
  console.log('selectedWallets', selectedWallets);
  const { intl, onClose, onSubmit, triggedFrom, wallets } = props;
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

  const onSelectWallet = (walletId) => {
    const wallet = wallets.find(({ id }) => walletId === id);
    console.log('wallet', wallet);
    if (wallet) {
      setSelectedWallets([...selectedWallets, wallet]);
    }
  };

  const filteredWallets = differenceWith(
    wallets,
    selectedWallets,
    (wallet, selectedWallet) => wallet.id === selectedWallet.id
  );

  const getWalletDropdown = (index: number) => {
    const wallet = selectedWallets[index];
    const walletsList = [...filteredWallets];
    if (wallet) walletsList.unshift(wallet);
    return (
      <WalletsDropdown
        getStakePoolById={() => {}}
        numberOfStakePools={100}
        wallets={walletsList}
        onChange={onSelectWallet}
        placeholder="!!!Select a wallet"
        value={wallet ? wallet.id : null}
      />
    );
  };

  return (
    <Dialog
      className={componentStyles}
      title={intl.formatMessage(messages.title)}
      subtitle={intl.formatMessage(messages.subtitle, { triggedFrom })}
      actions={actions}
    >
      {getWalletDropdown(0)}
      {!!selectedWallets.length &&
        selectedWallets.map((x, index) => getWalletDropdown(index + 1))}
    </Dialog>
  );
});

export default injectIntl(DappTransactionRequest);
