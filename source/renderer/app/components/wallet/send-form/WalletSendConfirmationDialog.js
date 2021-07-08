// @flow
import React, { useCallback, useEffect, useState } from 'react';
import { Observer } from 'mobx-react';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { intlShape, FormattedHTMLMessage, injectIntl } from 'react-intl';
import type { Element } from 'react';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './WalletSendConfirmationDialog.scss';
import { FormattedHTMLMessageWithLink } from '../../widgets/FormattedHTMLMessageWithLink';
import HardwareWalletStatus from '../../hardware-wallet/HardwareWalletStatus';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import { HwDeviceStatuses } from '../../../domains/Wallet';
import { getMessages } from './WalletSendConfirmationDialog.messages';

import type { HwDeviceStatus } from '../../../domains/Wallet';

type Props = {
  amount: string,
  receiver: string,
  totalAmount: ?string,
  transactionFee: ?string,
  onSubmit: Function,
  amountToNaturalUnits: (amountWithFractions: string) => string,
  onCancel: Function,
  isSubmitting: boolean,
  error: ?LocalizableError,
  currencyUnit: string,
  onInitiateTransaction: Function,
  intl: intlShape.isRequired,
  isHardwareWallet: boolean,
  hwDeviceStatus: HwDeviceStatus,
  isFlight: boolean,
  onExternalLinkClick: Function,
  walletName: string,
  isTrezor: boolean,
};

type InputField = {
  value?: string,
  error?: string,
};

type CheckboxField = {
  value?: boolean,
  error?: string,
};

const WalletSendConfirmationDialog = (props: Props) => {
  const [areTermsAccepted, setAreTermsAccepted] = useState<boolean>(false);
  const [passphraseField, setPassphraseField] = useState<InputField>({});
  const [
    flightCandidateCheckboxField,
    setFlightCandidateCheckboxField,
  ] = useState<CheckboxField>({});

  const [sendButtonDisabled, setSendButtonDisabled] = useState<boolean>(true);

  const {
    onCancel,
    amount,
    amountToNaturalUnits,
    error,
    receiver,
    totalAmount,
    transactionFee,
    isSubmitting,
    isFlight,
    isTrezor,
    currencyUnit,
    onExternalLinkClick,
    onInitiateTransaction,
    onSubmit,
    hwDeviceStatus,
    walletName,
    intl,
    isHardwareWallet,
  } = props;

  useEffect(() => {
    const disabled =
      (!isHardwareWallet && !!passphraseField?.error) ||
      (isHardwareWallet &&
        hwDeviceStatus !== HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED) ||
      (!areTermsAccepted && isFlight);
    setSendButtonDisabled(disabled);
  }, [passphraseField?.value]);

  useEffect(() => {
    setSendButtonDisabled(true);
  }, []);

  const renderConfirmationElement = useCallback((): Element<any> | null => {
    if (!isFlight || (isFlight && areTermsAccepted)) {
      return isHardwareWallet ? (
        <div className={styles.hardwareWalletStatusWrapper}>
          <HardwareWalletStatus
            hwDeviceStatus={hwDeviceStatus}
            walletName={walletName}
            isTrezor={isTrezor}
            onExternalLinkClick={onExternalLinkClick}
          />
        </div>
      ) : (
        <Input
          type="password"
          className={styles.passphrase}
          error={passphraseField?.error}
          skin={InputSkin}
          onChange={(value) => {
            setPassphraseField({
              ...passphraseField,
              value,
            });
          }}
          onKeyPress={(event) => {
            if (event.key === 'Enter') submit();
          }}
          value={passphraseField?.value}
          autoFocus
        />
      );
    }
    return null;
  });

  const onCheckboxClick = useCallback(() => {
    if (!flightCandidateCheckboxField?.value) {
      setAreTermsAccepted(areTermsAccepted);
      setFlightCandidateCheckboxField({
        ...flightCandidateCheckboxField,
        value: true,
      });
      if (isHardwareWallet) {
        onInitiateTransaction();
      }
    } else {
      setFlightCandidateCheckboxField({
        ...flightCandidateCheckboxField,
        value: false,
      });
    }
  });

  const submit = useCallback(() => {
    const transactionData = {
      receiver,
      amount: amountToNaturalUnits(amount),
      passphrase: passphraseField?.value,
      isHardwareWallet,
    };
    onSubmit(transactionData);
  });

  const actions = [
    {
      label: intl.formatMessage(getMessages().backButtonLabel),
      onClick: !isSubmitting ? onCancel : () => {},
    },
    {
      label: !isSubmitting ? (
        intl.formatMessage(getMessages().sendButtonLabel)
      ) : (
        <LoadingSpinner />
      ),
      onClick: submit,
      primary: true,
      className: 'confirmButton',
      disabled: sendButtonDisabled,
    },
  ];

  const getErrorMessage = useCallback(() => {
    if (error) {
      const errorHasLink = error?.values?.linkLabel;
      const errorElement = errorHasLink ? (
        <FormattedHTMLMessageWithLink
          message={error}
          onExternalLinkClick={onExternalLinkClick}
        />
      ) : (
        intl.formatMessage(props?.error)
      );
      return <p className={styles.error}>{errorElement}</p>;
    }
  });

  return (
    <Dialog
      title={intl.formatMessage(getMessages().dialogTitle)}
      subtitle={walletName}
      actions={actions}
      closeOnOverlayClick
      primaryButtonAutoFocus
      onClose={!isSubmitting ? onCancel : () => {}}
      className={styles.dialog}
      closeButton={<DialogCloseButton />}
    >
      <div className={styles.passphraseFields}>
        <div className={styles.addressToLabelWrapper}>
          <div className={styles.addressToLabel}>
            {intl.formatMessage(getMessages().addressToLabel)}
          </div>
          <div className={styles.addressTo}>{receiver}</div>
        </div>

        <div className={styles.amountFeesWrapper}>
          <div className={styles.amountWrapper}>
            <div className={styles.amountLabel}>
              {intl.formatMessage(getMessages().amountLabel)}
            </div>
            <div className={styles.amount}>
              {amount}
              <span className={styles.currencyCode}>&nbsp;{currencyUnit}</span>
            </div>
          </div>

          <div className={styles.feesWrapper}>
            <div className={styles.feesLabel}>
              {intl.formatMessage(getMessages().feesLabel)}
            </div>
            <div className={styles.fees}>
              +{transactionFee}
              <span className={styles.currencyCode}>&nbsp;{currencyUnit}</span>
            </div>
          </div>
        </div>

        <div className={styles.totalAmountWrapper}>
          <div className={styles.totalAmountLabel}>
            {intl.formatMessage(getMessages().totalLabel)}
          </div>
          <div className={styles.totalAmount}>
            {totalAmount}
            <span className={styles.currencyCode}>&nbsp;{currencyUnit}</span>
          </div>
        </div>

        {isFlight && (
          <div className={styles.flightCandidateWarning}>
            <FormattedHTMLMessage
              {...getMessages().flightCandidateWarning}
              tagName="p"
            />
            <Checkbox
              error={flightCandidateCheckboxField.error}
              skin={CheckboxSkin}
              disabled={areTermsAccepted}
              onChange={onCheckboxClick}
              checked={flightCandidateCheckboxField.value}
            />
          </div>
        )}
        <Observer>{() => renderConfirmationElement()}</Observer>
      </div>

      {!!error && getErrorMessage()}
    </Dialog>
  );
};

export default injectIntl(WalletSendConfirmationDialog);
