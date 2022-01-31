import React, { Component } from 'react';
import { observer } from 'mobx-react';
import QRCode from 'qrcode.react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import CopyToClipboard from 'react-copy-to-clipboard';
import SVGInline from 'react-svg-inline';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import Dialog from '../../widgets/Dialog';
import { getNetworkExplorerUrl } from '../../../utils/network';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './CompletionDialog.scss' or it... Remove this comment to see the full error message
import styles from './CompletionDialog.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clipboa... Remove this comment to see the full error message
import iconCopy from '../../../assets/images/clipboard-ic.inline.svg';
import InlineNotification from '../../notifications/InlineNotification';
import { DEVELOPMENT } from '../../../../../common/types/environment.types';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.completion.dialog.headline',
    defaultMessage: '!!!Paper wallet certificate',
    description:
      'Headline for the "Paper wallet create certificate completion dialog" headline.',
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.completion.dialog.subtitle',
    defaultMessage:
      '!!!You may wish to fold your paper wallet certificate and glue together the edges to store it securely. Please keep your certificate safe.',
    description:
      'Headline for the "Paper wallet create certificate completion dialog" subtitle.',
  },
  linkInstructions: {
    id: 'paper.wallet.create.certificate.completion.dialog.linkInstructions',
    defaultMessage: `!!!When you wish to import your wallet back into Daedalus crop any glued edges of the certificate to open it.
      To check your balance on the paper wallet at any time, you may use the link below. Copy or save the URL to your browser bookmarks to do this easily`,
    description:
      'Headline for the "Paper wallet create certificate completion dialog" link instructions.',
  },
  addressInstructions: {
    id: 'paper.wallet.create.certificate.completion.dialog.addressInstructions',
    defaultMessage:
      '!!!To receive funds to your paper wallet simply share your wallet address with others.',
    description:
      'Headline for the "Paper wallet create certificate completion dialog" address instructions.',
  },
  cardanoLinkLabel: {
    id: 'paper.wallet.create.certificate.completion.dialog.cardanoLinkLabel',
    defaultMessage: '!!!Cardano explorer link',
    description:
      '"Paper wallet create certificate completion dialog" cardano link label.',
  },
  addressCopiedLabel: {
    id: 'paper.wallet.create.certificate.completion.dialog.addressCopiedLabel',
    defaultMessage: '!!!copied',
    description:
      '"Paper wallet create certificate completion dialog" address copied.',
  },
  addressLabel: {
    id: 'paper.wallet.create.certificate.completion.dialog.addressLabel',
    defaultMessage: '!!!Wallet address',
    description:
      '"Paper wallet create certificate completion dialog" wallet address label.',
  },
  finishButtonLabel: {
    id: 'paper.wallet.create.certificate.completion.dialog.finishButtonLabel',
    defaultMessage: '!!!Finish',
    description:
      '"Paper wallet create certificate completion dialog" finish button label.',
  },
});
type Props = {
  walletCertificateAddress: string;
  onClose: (...args: Array<any>) => any;
  onOpenExternalLink: (...args: Array<any>) => any;
  copyAddressNotificationDuration: number;
  network: string;
};
type State = {
  showCopyNotification: boolean;
};

@observer
class CompletionDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    network: DEVELOPMENT,
  };
  state = {
    showCopyNotification: false,
  };
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'TimeoutID'.
  copyNotificationTimeout: TimeoutID;
  onShowCopyNotification = () => {
    const { copyAddressNotificationDuration } = this.props;
    const timeInSeconds = copyAddressNotificationDuration * 1000;
    clearTimeout(this.copyNotificationTimeout);
    this.setState({
      showCopyNotification: true,
    });
    this.copyNotificationTimeout = setTimeout(
      () =>
        this.setState({
          showCopyNotification: false,
        }),
      timeInSeconds
    );
  };

  render() {
    const { intl } = this.context;
    const {
      onClose,
      walletCertificateAddress,
      onOpenExternalLink,
      network,
    } = this.props;
    const { showCopyNotification } = this.state;
    const dialogClasses = classnames([styles.component, 'completionDialog']);
    const actions = [
      {
        className: 'finishButton',
        label: intl.formatMessage(messages.finishButtonLabel),
        primary: true,
        onClick: onClose,
      },
    ];
    const cardanoExplorerLink = `${getNetworkExplorerUrl(
      network
    )}/address/${walletCertificateAddress}`;
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
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
      >
        <div className={styles.completionContentWrapper}>
          <p className={styles.subtitle}>
            {intl.formatMessage(messages.subtitle)}
          </p>

          <div className={styles.linkInstructionsWrapper}>
            <p>{intl.formatMessage(messages.linkInstructions)}</p>

            <p className={styles.infoBoxLabel}>
              {intl.formatMessage(messages.cardanoLinkLabel)}
            </p>

            <div className={styles.infoBox}>
              <Link
                className={styles.link}
                onClick={() => onOpenExternalLink(cardanoExplorerLink)}
                label={cardanoExplorerLink}
                skin={LinkSkin}
              />
            </div>
          </div>

          <div className={styles.addressInstructionsWrapper}>
            <p>{intl.formatMessage(messages.addressInstructions)}</p>

            <p className={styles.infoBoxLabel}>
              {intl.formatMessage(messages.addressLabel)}
            </p>

            <InlineNotification show={showCopyNotification}>
              {intl.formatMessage(messages.addressCopiedLabel)}
            </InlineNotification>

            <div className={styles.infoBox}>
              {walletCertificateAddress}
              <CopyToClipboard
                text={walletCertificateAddress}
                onCopy={this.onShowCopyNotification}
              >
                <SVGInline svg={iconCopy} className={styles.copyIconBig} />
              </CopyToClipboard>
            </div>
          </div>

          <div className={styles.qrCode}>
            <QRCode
              value={walletCertificateAddress}
              bgColor={qrCodeBackgroundColor}
              fgColor={qrCodeForegroundColor}
              size={152}
            />
          </div>
        </div>
      </Dialog>
    );
  }
}

export default CompletionDialog;
