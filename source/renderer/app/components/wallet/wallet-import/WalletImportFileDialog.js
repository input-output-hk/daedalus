// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import ReactModal from 'react-modal';
import styles from './WalletImportFileDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';

const messages = defineMessages({
  title: {
    id: 'wallet.import.file.dialog.title',
    defaultMessage: '!!!Import wallets',
    description: 'Import wallets dialog title',
  },
  versionName: {
    id: 'static.splash.network.itnVersionName',
    defaultMessage: '!!!INCENTIVIZED TESTNET v1',
    description: 'INCENTIVIZED TESTNET v1',
  },
  networkName: {
    id: 'static.splash.network.itnNetworkName',
    defaultMessage: '!!!Rewards',
    description: 'Rewards',
  },
  itnDescription: {
    id: 'static.splash.network.itnDescription',
    defaultMessage:
      '!!!This version of Daedalus has been created specifically for use with the Incentivized Testnet. It is not compatible with the Cardano mainnet. If you had ada in a mainnet Daedalus or Yoroi wallet at the time of the balance snapshot (12.00 UTC, November 29) you can use this version of Daedalus to restore those funds as testnet ada, for use exclusively on the Incentivized Testnet. The rewards earned for delegating stake and running stake pools on the Incentivized Testnet will be paid out in real ada at the end of the Incentivized Testnet program. Important: Please keep your Rewards wallet recovery phrase safe. You will need it to receive your ada rewards on the mainnet.',
    description:
      'This version of Daedalus has been created specifically for the balance check, the first stage in the roll-out of the Incentivized Testnet. It is not compatible with the Cardano mainnet. The balance check is a practice run for the official balance snapshot that is currently planned for later in November. This initial test will allow us to test core functionality, while enabling users to validate that the value of their mainnet ada balances is accurately captured ahead of the Incentivized Testnet.',
  },
  buttonLabel: {
    id: 'static.splash.network.buttonLabel',
    defaultMessage: '!!!I understand',
    description: 'I understand',
  },
  linkLabel: {
    id: 'static.splash.network.linkLabel',
    defaultMessage: '!!!Learn more',
    description: 'Learn more',
  },
  linkUrl: {
    id: 'static.splash.network.itnLinkUrl',
    defaultMessage: '!!!http://staking.cardano.org/',
    description: '"Learn more" link URL on the network splash screen',
  },
});

type Props = {
  onConfirm: Function,
  onClose: Function,
  openExternalLink: Function,
};

export default class WalletImportFileDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  confirm = () => {
    this.props.onConfirm();
  };

  render() {
    const { intl } = this.context;
    const { onConfirm, onClose, openExternalLink } = this.props;
    const title = intl.formatMessage(messages.title);
    const description = <FormattedHTMLMessage {...messages.itnDescription} />;
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    const linkLabel = intl.formatMessage(messages.linkLabel);
    const onLinkClick = () =>
      openExternalLink(intl.formatMessage(messages.linkUrl));

    return (
      <ReactModal
        isOpen
        onRequestClose={onClose}
        shouldCloseOnOverlayClick={false}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <div className={styles.component}>
          <DialogCloseButton
            className={styles.closeButton}
            icon={closeCrossThin}
            onClose={onClose}
          />
          <div className={styles.backgroundContainer} />
          <div className={styles.content}>
            <div className={styles.title}>{title}</div>
            <div className={styles.description}>{description}</div>
            <div className={styles.action}>
              <Button
                className={styles.actionButton}
                label={buttonLabel}
                onClick={onConfirm}
                skin={ButtonSkin}
              />
            </div>
            <Link
              className={styles.learnMoreLink}
              onClick={onLinkClick}
              label={linkLabel}
              skin={LinkSkin}
            />
          </div>
        </div>
      </ReactModal>
    );
  }
}
