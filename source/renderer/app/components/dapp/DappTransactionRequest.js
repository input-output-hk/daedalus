// @flow
import React from 'react';
import BigNumber from 'bignumber.js';
import classnames from 'classnames';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import { observer } from 'mobx-react';
import styles from './DappTransactionRequest.scss';
import Dialog from '../widgets/Dialog';
import globalMessages from '../../i18n/global-messages';
import Wallet from '../../domains/Wallet';
import WalletsDropdown from '../widgets/forms/WalletsDropdown';
import AssetsTransactionConfirmation from '../assets/AssetsTransactionConfirmation';
import type { AssetToken } from '../../api/assets/types';

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
  fromWalletLabel: {
    id: 'dapp.transaction.request.fromWalletLabel',
    defaultMessage: '!!!From wallet',
    description: '"fromWalletLabel" in the dApp transaction request dialog',
  },
  receiverLabel: {
    id: 'dapp.transaction.request.receiver.label',
    defaultMessage: '!!!Receiver',
    description: '"receiver" in the dApp transaction request dialog',
  },
  walletsDropdownPlaceholder: {
    id: 'dapp.transaction.request.walletsDropdown.placeholder',
    defaultMessage: '!!!Select a wallet',
    description:
      '"walletsDropdownPlaceholder" in the dApp transaction request dialog',
  },
  additionalDataLabel: {
    id: 'dapp.transaction.request.additionalData.label',
    defaultMessage: '!!!Addicional data',
    description: '"additionalDataLabel" in the dApp transaction request dialog',
  },
  metaDataLabel: {
    id: 'dapp.transaction.request.metaData.label',
    defaultMessage: '!!!Meta data',
    description: '"metaDataLabel" in the dApp transaction request dialog',
  },
});

type Props = {
  address: string,
  additionalData: Object,
  assets: Array<AssetToken>,
  feesAmount?: BigNumber,
  intl: intlShape.isRequired,
  onClose: Function,
  onSelectWallet: Function,
  onSubmit: Function,
  selectedWallet: ?Wallet,
  triggedFrom: string,
  wallets: Array<Wallet>,
};

const DappTransactionRequest = observer((props: Props) => {
  const {
    address,
    additionalData,
    assets,
    feesAmount,
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
      <p className={styles.label}>
        {intl.formatMessage(messages.fromWalletLabel)}
      </p>
      <WalletsDropdown
        className={styles.walletsDropdown}
        getStakePoolById={() => {}}
        numberOfStakePools={100}
        onChange={onSelectWallet}
        placeholder={intl.formatMessage(messages.walletsDropdownPlaceholder)}
        value={selectedWallet ? selectedWallet.id : null}
        wallets={walletsOptions}
      />
      <hr />
      <p className={styles.label}>
        {intl.formatMessage(messages.receiverLabel)}
      </p>
      <p className={styles.address}>{address}</p>
      <AssetsTransactionConfirmation
        assets={assets}
        feesAmount={feesAmount}
        className={styles.assetsTransaction}
      />
      <p className={styles.label}>
        {intl.formatMessage(messages.additionalDataLabel)}
      </p>
      <div className={styles.additionalData}>
        <pre>{JSON.stringify(additionalData)}</pre>
      </div>
      <p className={styles.label}>
        {intl.formatMessage(messages.metaDataLabel)}
      </p>
      <div className={styles.metaData}>Meta data content</div>
    </Dialog>
  );
});

export default injectIntl(DappTransactionRequest);
