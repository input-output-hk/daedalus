import React, { useEffect, useState } from 'react';
import BigNumber from 'bignumber.js';
import { injectIntl } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import { formattedWalletAmount } from '../../../utils/formatters';
import Wallet, { HwDeviceStatus } from '../../../domains/Wallet';
import HardwareWalletStatus from '../../hardware-wallet/HardwareWalletStatus';
import styles from './VotingPowerDelegationConfirmationDialog.scss';
import { DelegateVotesError } from '../../../stores/VotingStore';
import type { Intl } from '../../../types/i18nTypes';
import { messages } from './VotingPowerDelegationConfirmationDialog.messages';
import globalMessages from '../../../i18n/global-messages';

const mapOfTxErrorCodeToIntl: Record<
  DelegateVotesError,
  typeof messages[keyof typeof messages]
> = {
  generic: messages.errorGeneric,
  wrong_encryption_passphrase: globalMessages.invalidSpendingPassword,
};

export type VotingPowerDelegationConfirmationDialogState =
  | {
      error?: DelegateVotesError;
      passphrase: string;
      status: 'awaiting';
    }
  | {
      passphrase: string;
      status: 'confirmed';
    }
  | { status: 'submitting' };

type VotingPowerDelegationConfirmationDialogProps = {
  chosenOption: string;
  fees: BigNumber;
  hwDeviceStatus: HwDeviceStatus;
  intl: Intl;
  isTrezor: boolean;
  onClose: () => void;
  onExternalLinkClick: (...args: Array<any>) => any;
  onSubmit: (
    passphrase?: string
  ) => Promise<
    { success: true } | { success: false; errorCode: DelegateVotesError }
  >;
  redirectToWallet: (walletId: string) => void;
  selectedWallet: Wallet;
};

function VotingPowerDelegationConfirmationDialog({
  chosenOption,
  fees,
  hwDeviceStatus,
  intl,
  isTrezor,
  onClose,
  onExternalLinkClick,
  onSubmit,
  redirectToWallet,
  selectedWallet,
}: VotingPowerDelegationConfirmationDialogProps) {
  const [state, setState] = useState<
    VotingPowerDelegationConfirmationDialogState
  >({
    passphrase: '',
    status: 'awaiting',
  });

  useEffect(() => {
    (async () => {
      if (state.status !== 'confirmed') return;

      const { passphrase, ...restState } = state;
      setState({
        ...restState,
        status: 'submitting',
      });

      const result = await onSubmit(passphrase);

      if (result.success === true) {
        redirectToWallet(selectedWallet.id);
        return;
      }

      setState({
        ...state,
        error: result.errorCode,
        status: 'awaiting',
      });
    })();
  }, [intl, onSubmit, redirectToWallet, state]);

  return (
    <Dialog
      title={intl.formatMessage(messages.title)}
      actions={[
        {
          label: intl.formatMessage(messages.buttonCancel),
          onClick: onClose,
          disabled: state.status === 'submitting',
        },
        {
          label: intl.formatMessage(messages.buttonConfirm),
          onClick: () => {
            if (state.status !== 'awaiting' || !state.passphrase) return;
            setState({
              passphrase: state.passphrase,
              status: 'confirmed',
            });
          },
          primary: true,
          disabled: state.status === 'submitting' || !state.passphrase,
        },
      ]}
      onClose={onClose}
      closeButton={<DialogCloseButton onClose={onClose} />}
    >
      <div className={styles.content}>
        <p className={styles.paragraphTitle}>
          {intl.formatMessage(messages.vote)}
        </p>
        <p className={styles.paragraphValue}>{chosenOption}</p>

        <p className={styles.paragraphTitle}>
          {intl.formatMessage(messages.fee)}
        </p>
        <p className={styles.paragraphValue}>{formattedWalletAmount(fees)}</p>

        {selectedWallet.isHardwareWallet ? (
          <HardwareWalletStatus
            hwDeviceStatus={hwDeviceStatus}
            walletName={selectedWallet.name}
            isTrezor={isTrezor}
            onExternalLinkClick={onExternalLinkClick}
          />
        ) : (
          <Input
            autoFocus
            value={state.status === 'awaiting' ? state.passphrase : ''}
            onChange={(passphrase) => {
              if (state.status !== 'awaiting') return;
              setState({
                ...state,
                passphrase,
              });
            }}
            disabled={state.status !== 'awaiting'}
            type={'password'}
            label={intl.formatMessage(messages.password)}
            skin={InputSkin}
          />
        )}

        {'error' in state && (
          <p className={styles.error}>
            {intl.formatMessage(mapOfTxErrorCodeToIntl[state.error])}
          </p>
        )}
      </div>
    </Dialog>
  );
}

export default injectIntl(VotingPowerDelegationConfirmationDialog);
