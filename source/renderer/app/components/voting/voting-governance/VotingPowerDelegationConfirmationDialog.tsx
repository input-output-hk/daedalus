import React, { useState } from 'react';
import BigNumber from 'bignumber.js';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { noop } from 'lodash';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import { formattedWalletAmount } from '../../../utils/formatters';
import Wallet from '../../../domains/Wallet';

type VotingPowerDelegationConfirmationDialogProps = {
  fees: BigNumber;
  onClose: () => void;
  onConfirm: (passphrase: string) => void;
  selectedWallet: Wallet;
  vote: string;
};

type State = 'Awaiting' | 'Submitting';

export function VotingPowerDelegationConfirmationDialog({
  fees,
  onClose,
  onConfirm,
  selectedWallet,
  vote,
}: VotingPowerDelegationConfirmationDialogProps) {
  const [state, setState] = useState<State>();
  const [passphrase, setPassphrase] = useState('');

  const handleClose = state === 'Submitting' ? noop : onClose;

  const handleConfirm =
    state === 'Submitting'
      ? noop
      : () => {
          setState('Submitting');
          onConfirm(passphrase);
        };

  return (
    <Dialog
      title={'Confirm transaction'}
      actions={[
        {
          label: 'Cancel',
          onClick: handleClose,
          disabled: state === 'Submitting',
        },
        {
          label: 'Confirm',
          onClick: handleConfirm,
          primary: true,
          disabled: state === 'Submitting',
        },
      ]}
      onClose={handleClose}
      closeButton={<DialogCloseButton onClose={handleClose} />}
    >
      <h4>Vote</h4>
      <br />
      {vote}
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
          value={passphrase}
          onChange={(value) => setPassphrase(value)}
          type={'password'}
          label={'Type your password'}
          skin={InputSkin}
        />
      )}
    </Dialog>
  );
}
