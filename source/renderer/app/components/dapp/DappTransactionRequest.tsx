import React, { useState, useCallback, useMemo } from 'react';
import BigNumber from 'bignumber.js';
import classnames from 'classnames';
import { Select } from 'react-polymorph/lib/components/Select';
import {
  defineMessages,
  intlShape,
  injectIntl,
  FormattedHTMLMessage,
} from 'react-intl';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DappTransactionRequest.scss'... Remove this comment to see the full error message
import styles from './DappTransactionRequest.scss';
import Dialog from '../widgets/Dialog';
import globalMessages from '../../i18n/global-messages';
import Wallet from '../../domains/Wallet';
import WalletsDropdown from '../widgets/forms/WalletsDropdown';
import AssetsTransactionConfirmation from '../assets/AssetsTransactionConfirmation';
import { formattedWalletAmount } from '../../utils/formatters';
import { isTokenMissingInWallet, tokenHasBalance } from '../../utils/assets';
import type { AssetToken } from '../../api/assets/types';

const messages = defineMessages({
  title: {
    id: 'dapp.transaction.request.title',
    defaultMessage: '!!!Transaction request',
    description: '"title" in the dApp transaction request dialog',
  },
  subtitle: {
    id: 'dapp.transaction.request.subtitle',
    defaultMessage: '!!!triggered from {triggeredFrom}',
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
  addWalletLabel: {
    id: 'dapp.transaction.request.walletsDropdown.addWalletLabel',
    defaultMessage: '!!!Add a wallet',
    description: '"addWalletLabel" in the dApp transaction request dialog',
  },
  transactionFeeLabel: {
    id: 'dapp.transaction.request.transactionFee.label',
    defaultMessage: '!!!Transaction fee',
    description: '"transactionFeeLabel" in the dApp transaction request dialog',
  },
  additionalDataLabel: {
    id: 'dapp.transaction.request.additionalData.label',
    defaultMessage: '!!!Additional data',
    description: '"additionalDataLabel" in the dApp transaction request dialog',
  },
  metaDataLabel: {
    id: 'dapp.transaction.request.metaData.label',
    defaultMessage: '!!!Meta data',
    description: '"metaDataLabel" in the dApp transaction request dialog',
  },
  insufficientBalanceErrorMessage: {
    id: 'dapp.transaction.request.error.notEnoughAda',
    defaultMessage:
      '!!!This wallet does not contain the minimum amount of {adaBalanceRequired} which is required for delegation to be available. Please select a wallet with <b>a minimum amount of {adaBalanceRequired}</b>.',
    description:
      '"Not enough ada" error in the dApp transaction request dialog',
  },
});
type Props = {
  adaAmount: BigNumber;
  additionalData?: string;
  address: string;
  assets: Array<AssetToken>;
  assetsAmounts: Array<BigNumber>;
  intl: intlShape.isRequired;
  metadata?: string;
  onAddWallet: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  onSelectWallet: (...args: Array<any>) => any;
  onSubmit: (...args: Array<any>) => any;
  selectedWallet: Wallet | null | undefined;
  transactionFee: BigNumber;
  triggeredFrom: string;
  wallets: Array<Wallet>;
};
const DappTransactionRequest = observer((props: Props) => {
  const [isAdditionalDataVisible, toggleAdditionalData] = useState<boolean>(
    false
  );
  const [isMetadataVisible, toggleMetadata] = useState<boolean>(false);
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  const getToggleLabel = useCallback((isVisible: boolean) =>
    isVisible
      ? intl.formatMessage(globalMessages.hide)
      : intl.formatMessage(globalMessages.view)
  );
  const {
    adaAmount,
    address,
    additionalData,
    assets,
    assetsAmounts,
    intl,
    metadata,
    onClose,
    onAddWallet,
    onSelectWallet,
    onSubmit,
    selectedWallet,
    transactionFee,
    triggeredFrom,
    wallets,
  } = props;
  let hasAmountError = false;
  const hasTokenError = useMemo(
    () =>
      assets.reduce((result, token, index) => {
        if (!selectedWallet) return false;
        if (result) return true;
        return (
          isTokenMissingInWallet(selectedWallet, token) ||
          !tokenHasBalance(token, assetsAmounts[index])
        );
      }, false),
    [assets, selectedWallet]
  );
  const adaBalanceRequired = adaAmount.plus(transactionFee);
  const walletsDropdownHasError = selectedWallet?.amount.isLessThan(
    adaBalanceRequired
  );

  if (walletsDropdownHasError) {
    hasAmountError = true;
  }

  const adaAmountErrorMessage = walletsDropdownHasError ? (
    <FormattedHTMLMessage
      {...messages.insufficientBalanceErrorMessage}
      values={{
        adaBalanceRequired: formattedWalletAmount(adaBalanceRequired),
      }}
    />
  ) : null;
  const walletsDropdownStyles = classnames([
    styles.walletsDropdown,
    walletsDropdownHasError || hasTokenError ? styles.error : null,
    hasAmountError ? styles.amountError : null,
  ]);
  const canSubmit =
    !!selectedWallet && !hasTokenError && !walletsDropdownHasError;
  const actions = [
    {
      label: intl.formatMessage(globalMessages.cancel),
      onClick: onClose,
    },
    {
      label: intl.formatMessage(globalMessages.dialogButtonContinueLabel),
      primary: true,
      onClick: onSubmit,
      disabled: !canSubmit,
    },
  ];
  const componentStyles = classnames([styles.component]);
  return (
    <Dialog
      className={componentStyles}
      title={intl.formatMessage(messages.title)}
      subtitle={intl.formatMessage(messages.subtitle, {
        triggeredFrom,
      })}
      actions={actions}
    >
      <p className={styles.label}>
        {intl.formatMessage(messages.fromWalletLabel)}
      </p>
      {wallets.length ? (
        <WalletsDropdown
          className={walletsDropdownStyles}
          getStakePoolById={() => {}}
          numberOfStakePools={100}
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ className: any; getStakePoolById: () => vo... Remove this comment to see the full error message
          onChange={onSelectWallet}
          placeholder={intl.formatMessage(messages.walletsDropdownPlaceholder)}
          value={selectedWallet?.id}
          wallets={wallets}
        />
      ) : (
        <Select
          onChange={onAddWallet}
          placeholder={intl.formatMessage(messages.walletsDropdownPlaceholder)}
          value=""
          options={[
            {
              label: intl.formatMessage(messages.addWalletLabel),
              value: 'add-wallet',
            },
          ]}
          className={styles.addWalletSelect}
        />
      )}

      <hr />
      <p className={styles.label}>
        {intl.formatMessage(messages.receiverLabel)}
      </p>
      <p className={styles.address}>{address}</p>
      <AssetsTransactionConfirmation
        adaAmount={adaAmount}
        assets={assets}
        assetsAmounts={assetsAmounts}
        wallet={selectedWallet}
        adaError={adaAmountErrorMessage}
      />
      <p className={styles.label}>
        {intl.formatMessage(messages.transactionFeeLabel)}
      </p>
      <div className={styles.transactionFee}>
        +{formattedWalletAmount(transactionFee)}
      </div>
      <p className={styles.labelData}>
        {intl.formatMessage(messages.additionalDataLabel)}
        <button
          className={styles.toggleButton}
          onClick={() => toggleAdditionalData(!isAdditionalDataVisible)}
        >
          {getToggleLabel(isAdditionalDataVisible)}
        </button>
      </p>
      {isAdditionalDataVisible && (
        <div className={styles.additionalData}>
          <pre>{additionalData}</pre>
        </div>
      )}
      <p className={styles.labelData}>
        {intl.formatMessage(messages.metaDataLabel)}
        <button
          className={styles.toggleButton}
          onClick={() => toggleMetadata(!isMetadataVisible)}
        >
          {getToggleLabel(isMetadataVisible)}
        </button>
      </p>
      {isMetadataVisible && (
        <div className={styles.metadata}>
          <pre>{metadata}</pre>
        </div>
      )}
    </Dialog>
  );
});
export default injectIntl(DappTransactionRequest);
