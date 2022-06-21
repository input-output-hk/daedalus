import React from 'react';
import { observer } from 'mobx-react';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { doTermsNeedAcceptance } from './helpers';
import { PasswordInputProps as Props } from './types';
import HardwareWalletStatus from '../../../../components/hardware-wallet/HardwareWalletStatus';
import styles from './styles.scss';

function Component({
  isFlight,
  isTrezor,
  isHardwareWallet,
  areTermsAccepted,
  passphraseField,
  walletName,
  hwDeviceStatus,
  onExternalLinkClick,
  handleSubmitOnEnter,
}: Props) {
  if (doTermsNeedAcceptance({ isFlight, areTermsAccepted })) {
    return null;
  }

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
      {...passphraseField.bind()}
      error={passphraseField.error}
      skin={InputSkin}
      onKeyPress={handleSubmitOnEnter}
      autoFocus
    />
  );
}

export const PasswordInput = observer(Component);
