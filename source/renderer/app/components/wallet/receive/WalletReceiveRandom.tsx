import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import CopyToClipboard from 'react-copy-to-clipboard';
import QRCode from 'qrcode.react';
import { Button } from 'react-polymorph/lib/components/Button';
import { Input } from 'react-polymorph/lib/components/Input';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { submitOnEnter } from '../../../utils/form';
import BorderedBox from '../../widgets/BorderedBox';
import TinySwitch from '../../widgets/forms/TinySwitch';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clipboa... Remove this comment to see the full error message
import iconCopy from '../../../assets/images/clipboard-ic.inline.svg';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { VirtualAddressesList } from './VirtualAddressesList';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletReceiveRandom.scss' or... Remove this comment to see the full error message
import styles from './WalletReceiveRandom.scss';
import AddressRandom from './AddressRandom';
import WalletAddress from '../../../domains/WalletAddress';

const messages = defineMessages({
  walletAddressLabel: {
    id: 'wallet.receive.page.walletAddressLabel',
    defaultMessage: '!!!Your wallet address',
    description: 'Label for wallet address on the wallet "Receive page"',
  },
  walletReceiveInstructions: {
    id: 'wallet.receive.page.walletReceiveInstructions',
    defaultMessage:
      '!!!Share this wallet address to receive payments. To protect your privacy, always use a new address when requesting funds. To generate a new address please enter your spending password and press the ‘Generate a new address’ button.',
    description:
      'Wallet receive payments instructions on the wallet "Receive page"',
  },
  generateNewAddressButtonLabel: {
    id: 'wallet.receive.page.generateNewAddressButtonLabel',
    defaultMessage: '!!!Generate a new address',
    description:
      'Label for "Generate new address" button on the wallet "Receive page"',
  },
  generatedAddressesSectionTitle: {
    id: 'wallet.receive.page.receivingAddressesSectionTitle',
    defaultMessage: '!!!Receiving addresses',
    description:
      '"Generated addresses" section title on the wallet "Receive page"',
  },
  showUsedLabel: {
    id: 'wallet.receive.page.showUsedLabel',
    defaultMessage: '!!!Show used',
    description:
      'Label for "show used" wallet addresses link on the wallet "Receive page"',
  },
  spendingPasswordPlaceholder: {
    id: 'wallet.receive.page.spendingPasswordPlaceholder',
    defaultMessage: '!!!Password',
    description:
      'Placeholder for "spending password" on the wallet "Receive page"',
  },
  copyAddressLabel: {
    id: 'wallet.receive.page.copyAddressLabel',
    defaultMessage: '!!!Copy address',
    description: 'Label for "Copy address" link on the wallet "Receive page"',
  },
});
messages.fieldIsRequired = globalMessages.fieldIsRequired;
type Props = {
  walletAddress: string;
  isWalletAddressUsed: boolean;
  walletAddresses: Array<WalletAddress>;
  onGenerateAddress: (...args: Array<any>) => any;
  onShareAddress: (...args: Array<any>) => any;
  onCopyAddress: (...args: Array<any>) => any;
  isSidebarExpanded: boolean;
  walletHasPassword: boolean;
  isSubmitting: boolean;
  error?: LocalizableError | null | undefined;
  showUsed: boolean;
  onToggleUsedAddresses: (...args: Array<any>) => any;
};

@observer
class WalletReceiveRandom extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  passwordField: Input;
  toggleUsedAddresses = () => {
    const { onToggleUsedAddresses } = this.props;
    onToggleUsedAddresses();
  };
  form = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: ' ',
          placeholder: this.context.intl.formatMessage(
            messages.spendingPasswordPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              if (this.props.walletHasPassword && field.value === '') {
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }

              return [true];
            },
          ],
        },
      },
    },
    {
      plugins: {
        vjf: vjf(),
      },
      options: {
        validationDebounceWait: 0,
        // Disable debounce to avoid error state after clearing
        validateOnChange: true,
        showErrorsOnBlur: false,
        showErrorsOnClear: false,
      },
    }
  );
  renderRow = (address: WalletAddress, index: number) => (
    <AddressRandom
      index={index}
      address={address}
      onCopyAddress={this.props.onCopyAddress}
      onShareAddress={this.props.onShareAddress}
    />
  );
  submit = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'submit' does not exist on type 'ReactToo... Remove this comment to see the full error message
    this.form.submit({
      onSuccess: (form) => {
        const { spendingPassword } = form.values();
        this.props.onGenerateAddress(spendingPassword || '');
        form.clear();
      },
      onError: () => {},
    });
    // eslint-disable-next-line no-unused-expressions
    this.passwordField && this.passwordField.focus();
  };
  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);
  getFilteredAddresses = (
    walletAddresses: Array<WalletAddress>
  ): Array<WalletAddress> =>
    walletAddresses.filter(
      (address: WalletAddress) => !address.used || this.props.showUsed
    );

  render() {
    const { form } = this;
    const {
      walletAddress,
      walletAddresses,
      onCopyAddress,
      isSidebarExpanded,
      walletHasPassword,
      isSubmitting,
      error,
      isWalletAddressUsed,
      showUsed,
    } = this.props;
    const { intl } = this.context;
    const walletAddressClasses = classnames([
      styles.hash,
      isWalletAddressUsed ? styles.usedHash : null,
    ]);
    const generateAddressWrapperClasses = classnames([
      styles.generateAddressWrapper,
      isSidebarExpanded ? styles.fullWidthOnSmallScreen : null,
    ]);
    const generateAddressButtonClasses = classnames([
      'primary',
      'generateAddressButton',
      walletHasPassword ? styles.submitWithPasswordButton : styles.submitButton,
      isSubmitting ? styles.spinning : null,
    ]);
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const passwordField = form.$('spendingPassword');
    const canSubmit = !isSubmitting && passwordField.value;
    const generateAddressForm = (
      <div className={generateAddressWrapperClasses}>
        {walletHasPassword && (
          <Input
            className={styles.spendingPassword}
            {...passwordField.bind()}
            ref={(input) => {
              this.passwordField = input;
            }}
            error={passwordField.error}
            skin={InputSkin}
            onKeyPress={this.handleSubmitOnEnter}
          />
        )}

        <Button
          className={generateAddressButtonClasses}
          disabled={!canSubmit}
          label={intl.formatMessage(messages.generateNewAddressButtonLabel)}
          skin={ButtonSkin}
          onClick={this.submit}
        />
      </div>
    );
    // Get QRCode color value from active theme's CSS variable
    const qrCodeBackgroundColor = document.documentElement
      ? document.documentElement.style.getPropertyValue(
          '--theme-receive-qr-code-background-color'
        )
      : 'transparent';
    const qrCodeForegroundColor = document.documentElement
      ? document.documentElement.style.getPropertyValue(
          '--theme-receive-qr-code-foreground-color'
        )
      : '#000';
    return (
      <div className={styles.component}>
        <BorderedBox fullHeight>
          <div className={styles.container}>
            <div className={styles.qrCodeAndInstructions}>
              {walletAddress && (
                <div className={styles.qrCode}>
                  <QRCode
                    value={walletAddress}
                    bgColor={qrCodeBackgroundColor}
                    fgColor={qrCodeForegroundColor}
                    size={152}
                  />
                </div>
              )}

              <div className={styles.instructions}>
                {walletAddress && (
                  <div className={walletAddressClasses}>
                    {walletAddress}
                    <CopyToClipboard
                      text={walletAddress} // eslint-disable-next-line react/jsx-no-bind
                      onCopy={onCopyAddress.bind(this, walletAddress)}
                    >
                      <SVGInline
                        svg={iconCopy}
                        className={styles.copyIconBig}
                      />
                    </CopyToClipboard>
                  </div>
                )}

                <div className={styles.hashLabel}>
                  {intl.formatMessage(messages.walletAddressLabel)}
                </div>

                <div className={styles.instructionsText}>
                  <FormattedHTMLMessage
                    {...messages.walletReceiveInstructions}
                  />
                </div>

                {error ? (
                  <p className={styles.error}>{intl.formatMessage(error)}</p>
                ) : null}

                {generateAddressForm}
              </div>
            </div>

            {walletAddresses.length ? (
              <>
                <div className={styles.generatedAddresses}>
                  <h2>
                    {intl.formatMessage(
                      messages.generatedAddressesSectionTitle
                    )}
                    <div className={styles.hideUsed}>
                      <TinySwitch
                        label={intl.formatMessage(messages.showUsedLabel)}
                        onChange={this.toggleUsedAddresses}
                        checked={showUsed}
                      />
                    </div>
                  </h2>

                  <VirtualAddressesList
                    rows={this.getFilteredAddresses(walletAddresses)}
                    renderRow={this.renderRow}
                  />
                </div>
              </>
            ) : null}
          </div>
        </BorderedBox>
      </div>
    );
  }
}

export default WalletReceiveRandom;
