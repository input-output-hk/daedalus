import React, { useEffect, useState } from 'react';
import BigNumber from 'bignumber.js';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import { formattedWalletAmount } from '../../../utils/formatters';
import Wallet from '../../../domains/Wallet';

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
  onClose: () => void;
  onSubmit: (
    passphrase?: string
  ) => Promise<{ success: true } | { success: false; error: string }>;
  selectedWallet: Wallet;
};

export function VotingPowerDelegationConfirmationDialog({
  chosenOption,
  fees,
  onClose,
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
      <h4>Vote</h4>
      <br />
      {chosenOption}
      <br />
      <br />

      <h4>Average fees</h4>
      <br />
      {formattedWalletAmount(fees, false)}
      <br />
      <br />

      {selectedWallet.isHardwareWallet ? (
        <div>{'TODO <HardwareWalletStatus />'}</div>
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
          label={'Type your password'}
          skin={InputSkin}
        />
      )}

      {'error' in state && <h5>{state.error}</h5>}
    </Dialog>
  );
}
