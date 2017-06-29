// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import CopyToClipboard from 'react-copy-to-clipboard';
import QRCode from 'qrcode.react';
import Button from 'react-toolbox/lib/button/Button';
import Input from 'react-toolbox/lib/input/Input';
import ReactToolboxMobxForm from '../../lib/ReactToolboxMobxForm';
import BorderedBox from '../widgets/BorderedBox';
import iconCopy from '../../assets/images/clipboard-ic.svg';
import iconProtected from '../../assets/images/protected-off.svg';
import WalletAddress from '../../domain/WalletAddress';
import globalMessages from '../../i18n/global-messages';
import LocalizableError from '../../i18n/LocalizableError';
import styles from './WalletReceive.scss';

const messages = defineMessages({
  walletAddressLabel: {
    id: 'wallet.receive.page.walletAddressLabel',
    defaultMessage: '!!!Your wallet address',
    description: 'Label for wallet address on the wallet "Receive page"',
  },
  walletReceiveInstructions: {
    id: 'wallet.receive.page.walletReceiveInstructions',
    defaultMessage: '!!!Share this wallet address to receive payments. To protect your privacy, new addresses are generated automatically once you use them.',
    description: 'Wallet receive payments instructions on the wallet "Receive page"',
  },
  generateNewAddressButtonLabel: {
    id: 'wallet.receive.page.generateNewAddressButtonLabel',
    defaultMessage: '!!!Generate new address',
    description: 'Label for "Generate new address" button on the wallet "Receive page"',
  },
  generatedAddressesSectionTitle: {
    id: 'wallet.receive.page.generatedAddressesSectionTitle',
    defaultMessage: '!!!Generated addresses',
    description: '"Generated addresses" section title on the wallet "Receive page"',
  },
  hideUsedLabel: {
    id: 'wallet.receive.page.hideUsedLabel',
    defaultMessage: '!!!hide used',
    description: 'Label for "hide used" wallet addresses link on the wallet "Receive page"',
  },
  showUsedLabel: {
    id: 'wallet.receive.page.showUsedLabel',
    defaultMessage: '!!!show used',
    description: 'Label for "show used" wallet addresses link on the wallet "Receive page"',
  },
  spendingPasswordPlaceholder: {
    id: 'wallet.receive.page.spendingPasswordPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for "spending password" on the wallet "Receive page"',
  },
  copyAddressLabel: {
    id: 'wallet.receive.page.copyAddressLabel',
    defaultMessage: '!!!Copy address',
    description: 'Label for "Copy address" link on the wallet "Receive page"',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

@observer
export default class WalletReceive extends Component {

  props: {
    walletAddress: string,
    walletAddresses: Array<WalletAddress>,
    onGenerateAddress: Function,
    onCopyAddress: Function,
    isSidebarExpanded: boolean,
    walletHasPassword: boolean,
    isSubmitting: boolean,
    error?: ?LocalizableError,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    showUsed: true,
  };

  toggleUsedAddresses = () => {
    this.setState({ showUsed: !this.state.showUsed });
  };

  form = new ReactToolboxMobxForm({
    fields: {
      spendingPassword: {
        type: 'password',
        label: ' ',
        placeholder: this.context.intl.formatMessage(messages.spendingPasswordPlaceholder),
        value: '',
        validators: [({ field }) => {
          if (this.props.walletHasPassword && field.value === '') {
            return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          }
          return [true];
        }],
        bindings: 'ReactToolbox',
      },
    },
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: 250,
    },
  });

  submit() {
    this.form.submit({
      onSuccess: (form) => {
        const { walletHasPassword } = this.props;
        const { spendingPassword } = form.values();
        const password = walletHasPassword ? spendingPassword : null;
        this.props.onGenerateAddress(password);

        // We need to disable on-change validation before reseting the form in order to
        // avoid debounced validation being called straight after the form is reset
        form.state.options.set({ validateOnChange: false });
        form.reset();
        form.showErrors(false);
        form.state.options.set({ validateOnChange: true });
      },
      onError: () => {}
    });
  }

  render() {
    const { form } = this;
    const {
      walletAddress, walletAddresses,
      onCopyAddress, isSidebarExpanded,
      walletHasPassword, isSubmitting,
      error,
    } = this.props;
    const { intl } = this.context;
    const { showUsed } = this.state;

    const generateAddressWrapperClasses = classnames([
      styles.generateAddressWrapper,
      isSidebarExpanded ? styles.fullWidthOnSmallScreen : null,
    ]);

    const generateAddressButtonClasses = classnames([
      'generateAddressButton',
      walletHasPassword ? styles.submitWithPasswordButton : styles.submitButton,
      isSubmitting ? styles.spinning : null,
    ]);

    const generateAddressForm = (
      <div className={generateAddressWrapperClasses}>
        {walletHasPassword &&
          <Input
            className={styles.spendingPassword}
            {...form.$('spendingPassword').bind()}
          />
        }

        <Button
          className={generateAddressButtonClasses}
          label={intl.formatMessage(messages.generateNewAddressButtonLabel)}
          onMouseUp={this.submit.bind(this)}
          primary
        />
      </div>
    );

    return (
      <div className={styles.component}>

        <BorderedBox>

          <div className={styles.qrCodeAndInstructions}>
            <div className={styles.qrCode}>
              <QRCode
                value={walletAddress}
                bgColor="transparent"
                size={160}
              />
            </div>

            <div className={styles.instructions}>
              <div className={styles.hash}>
                {walletAddress}
                <CopyToClipboard
                  text={walletAddress}
                  onCopy={onCopyAddress.bind(this, walletAddress)}
                >
                  <img className={styles.copyIconBig} src={iconCopy} role="presentation" />
                </CopyToClipboard>
              </div>

              <div className={styles.hashLabel}>
                {intl.formatMessage(messages.walletAddressLabel)}
              </div>

              <div className={styles.instructionsText}>
                {intl.formatMessage(messages.walletReceiveInstructions)}
              </div>

              {error ? <p className={styles.error}>{intl.formatMessage(error)}</p> : null}

              {generateAddressForm}

            </div>

            <img className={styles.protectedIcon} src={iconProtected} role="presentation" />
          </div>

          <div className={styles.generatedAddresses}>
            <h2>
              {intl.formatMessage(messages.generatedAddressesSectionTitle)}
              <button onClick={this.toggleUsedAddresses}>
                {intl.formatMessage(messages[showUsed ? 'hideUsedLabel' : 'showUsedLabel'])}
              </button>
            </h2>

            {walletAddresses.map((address, index) => {
              const isAddressVisible = !address.isUsed || showUsed;
              if (!isAddressVisible) return null;

              const addressClasses = classnames([
                'generatedAddress-' + (index + 1),
                styles.walletAddress,
                address.isUsed ? styles.usedWalletAddress : null,
              ]);
              return (
                <div key={index} className={addressClasses}>
                  <div className={styles.addressId}>{address.id}</div>
                  <div className={styles.addressActions}>
                    <CopyToClipboard
                      text={address.id}
                      onCopy={onCopyAddress.bind(this, address.id)}
                    >
                      <span className={styles.copyAddress}>
                        <img className={styles.copyIcon} src={iconCopy} role="presentation" />
                        <span>{intl.formatMessage(messages.copyAddressLabel)}</span>
                      </span>
                    </CopyToClipboard>
                  </div>
                </div>
              );
            })}
          </div>

        </BorderedBox>

      </div>
    );
  }

}
