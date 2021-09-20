// @flow

import React from 'react';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import styles from './DeleteWalletConfirmationDialog.scss';

type Props = {
  isBackupNoticeAccepted: boolean,
  confirmationValue: string,
  onAcceptBackupNotice: Function,
  onConfirmationValueChange: Function,
  submitOnEnter: Function,
  handleSubmit: Function,
  inputLabel: string,
  checkboxLabel: string,
};

const DeleteWalletConfirmation = ({
  isBackupNoticeAccepted,
  confirmationValue,
  onAcceptBackupNotice,
  onConfirmationValueChange,
  submitOnEnter,
  handleSubmit,
  checkboxLabel,
  inputLabel,
}: Props) => (
  <>
    <Checkbox
      label={checkboxLabel}
      onChange={onAcceptBackupNotice}
      checked={isBackupNoticeAccepted}
      skin={CheckboxSkin}
    />
    {isBackupNoticeAccepted && (
      <Input
        className={styles.confirmationInput}
        label={inputLabel}
        value={confirmationValue}
        // eslint-disable-next-line react/jsx-no-bind
        onKeyPress={submitOnEnter.bind(this, handleSubmit)}
        onChange={onConfirmationValueChange}
        skin={InputSkin}
      />
    )}
  </>
);

export default DeleteWalletConfirmation;
