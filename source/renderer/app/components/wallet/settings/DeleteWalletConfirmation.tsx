import React from 'react';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { submitOnEnter } from '../../../utils/form';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DeleteWalletConfirmationDial... Remove this comment to see the full error message
import styles from './DeleteWalletConfirmationDialog.scss';

type Props = {
  isBackupNoticeAccepted: boolean;
  confirmationValue: string;
  onAcceptBackupNotice: (...args: Array<any>) => any;
  onConfirmationValueChange: (...args: Array<any>) => any;
  handleSubmit: (...args: Array<any>) => any;
  inputLabel: string;
  checkboxLabel: string;
};

const DeleteWalletConfirmation = ({
  isBackupNoticeAccepted,
  confirmationValue,
  onAcceptBackupNotice,
  onConfirmationValueChange,
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
        onKeyPress={(event: KeyboardEvent) =>
          submitOnEnter(handleSubmit, event)
        }
        onChange={onConfirmationValueChange}
        skin={InputSkin}
      />
    )}
  </>
);

export default DeleteWalletConfirmation;
