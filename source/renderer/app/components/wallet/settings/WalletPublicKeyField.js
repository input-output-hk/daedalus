// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { Input } from 'react-polymorph/lib/components/Input';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import InputSkin from './WalletPublicKeyFieldSkin';
import copyImage from '../../../assets/images/copy.inline.svg';
import qrCodeImage from '../../../assets/images/qr-code.inline.svg';
import globalMessages from '../../../i18n/global-messages';
import styles from './WalletPublicKeyField.scss';

export const messages = defineMessages({
  walletPublicKey: {
    id: 'wallet.settings.walletPublicKey',
    defaultMessage: '!!!Wallet public key',
    description: 'Wallet public key label.',
  },
  walletPublicKeyShowInstruction: {
    id: 'wallet.settings.walletPublicKeyShowInstruction',
    defaultMessage: '!!!To show wallet public key click "Reveal" button',
    description: 'Wallet public key show instruction.',
  },
  showQRCode: {
    id: 'wallet.settings.showQRCode',
    defaultMessage: '!!!Show QR code',
    description: 'Show QR code tooltip.',
  },
});

type Props = {
  walletPublicKey: string,
};

type State = {
  walletPublicKeyHidden: boolean,
};

@observer
export default class WalletPublicKeyField extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    walletPublicKeyHidden: true,
  };

  toggleWalletPublicKeyVisibility = () =>
    this.setState((prevState) => ({
      walletPublicKeyHidden: !prevState.walletPublicKeyHidden,
    }));

  render() {
    const { walletPublicKey } = this.props;
    const { walletPublicKeyHidden } = this.state;
    const { intl } = this.context;
    const label = intl.formatMessage(messages.walletPublicKey);
    const fieldStyles = classnames([
      styles.field,
      walletPublicKeyHidden ? styles.valueHidden : styles.valueShown,
    ]);
    const hiddenValuePlaceholder = intl.formatMessage(
      messages.walletPublicKeyShowInstruction
    );
    const toggleButtonLabel = intl.formatMessage(
      globalMessages[walletPublicKeyHidden ? 'reveal' : 'hide']
    );
    const copyButtonStyles = classnames([
      styles.imageButton,
      styles.copyButton,
      'flat',
    ]);
    const qrCodeButtonStyles = classnames([
      styles.imageButton,
      styles.qrCodeButton,
      'flat',
    ]);
    const buttonStyles = classnames([styles.button, 'flat']);

    return (
      <div className={styles.component}>
        <Input
          className={fieldStyles}
          type="text"
          label={label}
          value={
            walletPublicKeyHidden ? hiddenValuePlaceholder : walletPublicKey
          }
          disabled
          skin={InputSkin}
          tooltip={intl.formatMessage(globalMessages.copy)}
        />
        <div className={styles.addons}>
          {!walletPublicKeyHidden && (
            <>
              <div className={styles.imageButtonContainer}>
                <PopOver content={intl.formatMessage(globalMessages.copy)}>
                  <Button
                    className={copyButtonStyles}
                    onClick={() => null}
                    label={<SVGInline svg={copyImage} />}
                  />
                </PopOver>
              </div>
              <div className={styles.imageButtonContainer}>
                <PopOver content={intl.formatMessage(messages.showQRCode)}>
                  <Button
                    className={qrCodeButtonStyles}
                    onClick={() => null}
                    label={<SVGInline svg={qrCodeImage} />}
                  />
                </PopOver>
              </div>
            </>
          )}
          <Button
            className={buttonStyles}
            label={toggleButtonLabel}
            onClick={this.toggleWalletPublicKeyVisibility}
          />
        </div>
      </div>
    );
  }
}
