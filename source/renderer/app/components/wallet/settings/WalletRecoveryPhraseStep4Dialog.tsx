import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletRecoveryPhraseStepDial... Remove this comment to see the full error message
import styles from './WalletRecoveryPhraseStepDialogs.scss';

export const messages = defineMessages({
  recoveryPhraseStep4Title: {
    id: 'wallet.settings.recoveryPhraseStep4Title',
    defaultMessage: '!!!verification failure',
    description: 'Label for the recoveryPhraseStep4Title on wallet settings.',
  },
  recoveryPhraseStep4Paragraph1: {
    id: 'wallet.settings.recoveryPhraseStep4Paragraph1',
    defaultMessage:
      '!!!The wallet recovery phrase you have entered does not match the recovery phrase of this wallet. Make sure you have entered the wallet recovery phrase which was written down during the wallet creation process for this wallet and make sure the words are in the correct order.',
    description:
      'Label for the recoveryPhraseStep4Paragraph1 on wallet settings.',
  },
  recoveryPhraseStep4Paragraph2: {
    id: 'wallet.settings.recoveryPhraseStep4Paragraph2',
    defaultMessage:
      '!!!If you are unable to verify your wallet recovery phrase, you should create a new wallet and move all of the funds from this wallet to the new wallet. If you do this, make sure you keep the wallet recovery phrase for the new wallet safe and secure.',
    description:
      'Label for the recoveryPhraseStep4Paragraph2 on wallet settings.',
  },
  recoveryPhraseStep4Button: {
    id: 'wallet.settings.recoveryPhraseStep4Button',
    defaultMessage: '!!!Verify recovery phrase again',
    description: 'Label for the recoveryPhraseStep4Button on wallet settings.',
  },
  recoveryPhraseStep4SupportTitle: {
    id: 'wallet.settings.recoveryPhraseStep4SupportTitle',
    defaultMessage: '!!!Read support portal article',
    description:
      'Label for the recoveryPhraseStep4SupportTitle on wallet settings.',
  },
  recoveryPhraseStep4SupportUrl: {
    id: 'wallet.settings.recoveryPhraseStep4SupportUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360035341914',
    description:
      'Label for the recoveryPhraseStep4SupportUrl on wallet settings.',
  },
});
type Props = {
  onClose: (...args: Array<any>) => any;
  onContinue: (...args: Array<any>) => any;
  openExternalLink: (...args: Array<any>) => any;
  walletName: string;
};

@observer
class WalletRecoveryPhraseStep4Dialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, onContinue, openExternalLink, walletName } = this.props;
    const actions = [
      {
        label: intl.formatMessage(messages.recoveryPhraseStep4Button),
        onClick: onContinue,
        className: 'attention',
      },
    ];
    const dialogStyles = classnames([
      styles.dialog,
      styles.dialog4,
      'verification-unsuccessful',
    ]);
    return (
      <Dialog
        className={dialogStyles}
        title={intl.formatMessage(messages.recoveryPhraseStep4Title)}
        subtitle={walletName}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <p>{intl.formatMessage(messages.recoveryPhraseStep4Paragraph1)}</p>
        <p>{intl.formatMessage(messages.recoveryPhraseStep4Paragraph2)}</p>
        <div className={styles.supportPortalContainer}>
          <Link
            onClick={(event: MouseEvent) =>
              openExternalLink(
                intl.formatMessage(messages.recoveryPhraseStep4SupportUrl),
                event
              )
            }
            label={intl.formatMessage(messages.recoveryPhraseStep4SupportTitle)}
            skin={LinkSkin}
          />
        </div>
      </Dialog>
    );
  }
}

export default WalletRecoveryPhraseStep4Dialog;
