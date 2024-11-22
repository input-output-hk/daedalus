import React, { useEffect, useState } from 'react';
import BigNumber from 'bignumber.js';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import { formattedWalletAmount } from '../../../utils/formatters';
import Wallet, { HwDeviceStatus } from '../../../domains/Wallet';
import HardwareWalletStatus from '../../hardware-wallet/HardwareWalletStatus';
import styles from './VotingPowerDelegationConfirmationDialog.scss';

export type VotingPowerDelegationConfirmationDialogState =
  | {
      error?: string;
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
  isTrezor: boolean;
  onClose: () => void;
  onExternalLinkClick: (...args: Array<any>) => any;
  onSubmit: (
    passphrase?: string
  ) => Promise<{ success: true } | { success: false; error: string }>;
  selectedWallet: Wallet;
};

export function VotingPowerDelegationConfirmationDialog({
  chosenOption,
  fees,
  hwDeviceStatus,
  isTrezor,
  onClose,
  onExternalLinkClick,
  onSubmit,
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

      if (result.success === false) {
        setState({
          ...state,
          error: result.error,
          status: 'awaiting',
        });
      }
    })();
  }, [onSubmit, state]);

  return (
    <Dialog
      title={'Confirm transaction'}
      actions={[
        {
          label: 'Cancel',
          onClick: onClose,
          disabled: state.status === 'submitting',
        },
        {
          label: 'Confirm',
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
        <p className={styles.paragraphTitle}>Vote</p>
        <p className={styles.paragraphValue}>{chosenOption}</p>

        <p className={styles.paragraphTitle}>Transaction fee</p>
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
            label={'Spending password'}
            skin={InputSkin}
          />
        )}

        {'error' in state && <p className={styles.error}>{state.error}</p>}
      </div>
    </Dialog>
  );
}
