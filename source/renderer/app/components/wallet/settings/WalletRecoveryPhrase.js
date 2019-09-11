// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import moment from 'moment';
import SVGInline from 'react-svg-inline';
import iconMnemonicsOk from '../../../assets/images/mnemonics-verification-ok.inline.svg';
import iconMnemonicsWarning from '../../../assets/images/mnemonics-verification-warning.inline.svg';
import iconMnemonicsNotification from '../../../assets/images/mnemonics-verification-notification.inline.svg';
import styles from './WalletRecoveryPhrase.scss';
import {
  MNEMONICS_CHECKING_WARNING,
  MNEMONICS_CHECKING_NOTIFICATION,
} from '../../../config/walletsConfig';
import WalletRecoveryPhraseStep1Dialog from './WalletRecoveryPhraseStep1Dialog';
import WalletRecoveryPhraseStep2Dialog from './WalletRecoveryPhraseStep2Dialog';

export const messages = defineMessages({
  mnemonicsValidationTitle: {
    id: 'wallet.settings.mnemonicsValidationTitle',
    defaultMessage: '!!!Do you have your wallet recovery phrase?',
    description: 'Label for the mnemonicsValidationTitle on wallet settings.',
  },
  mnemonicsValidationDescription: {
    id: 'wallet.settings.mnemonicsValidationDescription',
    defaultMessage:
      '!!!Funds in this wallet can only be recovered on another computer using the correct wallet recovery phrase. You can re-enter your wallet recovery phrase to verify that you have the correct recovery phrase for this wallet.',
    description:
      'Label for the mnemonicsValidationDescription on wallet settings.',
  },
  mnemonicsValidationConfirmed: {
    id: 'wallet.settings.mnemonicsValidationConfirmed',
    defaultMessage:
      '!!!You confirmed that you still have recovery phrase for this wallet <b>{timeAgo}</b>.',
    description:
      'Label for the mnemonicsValidationConfirmed on wallet settings.',
  },
  mnemonicsValidationNotConfirmed: {
    id: 'wallet.settings.mnemonicsValidationNotConfirmed',
    defaultMessage:
      '!!!You never confirmed that you still have recovery phrase for this wallet.',
    description:
      'Label for the mnemonicsValidationNotConfirmed on wallet settings.',
  },
  mnemonicsValidationNotification: {
    id: 'wallet.settings.mnemonicsValidationNotification',
    defaultMessage: '!!!We recommend that you check your recovery phrase.',
    description:
      'Label for the mnemonicsValidationNotConfirmed on wallet settings.',
  },
  mnemonicsValidationButton: {
    id: 'wallet.settings.mnemonicsValidationButton',
    defaultMessage: '!!!Confirm mnemonics.',
    description: 'Label for the mnemonicsValidationButton on wallet settings.',
  },
});

type Props = {
  mnemonicsConfirmationDate: Date,
  openDialogAction: Function,
  isDialogOpen: Function,
  walletRecoveryPhraseStep1Container: Node,
  walletRecoveryPhraseStep2Container: Node,
};

@observer
export default class WalletRecoveryPhrase extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  get statuses() {
    return {
      ok: {
        icon: iconMnemonicsOk,
        message: messages.mnemonicsValidationConfirmed,
      },
      warning: {
        icon: iconMnemonicsWarning,
        message: messages.mnemonicsValidationConfirmed,
      },
      notification: {
        icon: iconMnemonicsNotification,
        message: messages.mnemonicsValidationNotification,
      },
    };
  }

  get recoveryPhraseStatus() {
    const { mnemonicsConfirmationDate } = this.props;
    const daysSinceLastCheck = moment().diff(
      moment(mnemonicsConfirmationDate),
      'days'
    );
    let status = 'ok';
    if (daysSinceLastCheck > MNEMONICS_CHECKING_NOTIFICATION)
      status = 'notification';
    else if (daysSinceLastCheck > MNEMONICS_CHECKING_WARNING)
      status = 'warning';
    const { icon, message } = this.statuses[status];

    return {
      icon,
      message,
    };
  }

  render() {
    const { intl } = this.context;
    const {
      mnemonicsConfirmationDate,
      openDialogAction,
      isDialogOpen,
      walletRecoveryPhraseStep1Container,
      walletRecoveryPhraseStep2Container,
    } = this.props;
    const { icon, message } = this.recoveryPhraseStatus;

    return (
      <div className={styles.component}>
        <h2>
          {intl.formatMessage(messages.mnemonicsValidationTitle)}
          <SVGInline svg={icon} className={styles.mnemonicsValidationIcon} />
        </h2>
        <div className={styles.description}>
          {intl.formatMessage(messages.mnemonicsValidationDescription)}
        </div>
        <br />
        <FormattedHTMLMessage
          {...message}
          values={{
            timeAgo: moment(mnemonicsConfirmationDate).fromNow(),
          }}
        />
        <button
          className={styles.mnemonicsValidationButton}
          onClick={() => {
            openDialogAction({
              dialog: WalletRecoveryPhraseStep1Dialog,
            });
          }}
        >
          {intl.formatMessage(messages.mnemonicsValidationButton)}
        </button>

        {isDialogOpen(WalletRecoveryPhraseStep1Dialog)
          ? walletRecoveryPhraseStep1Container
          : false}

        {isDialogOpen(WalletRecoveryPhraseStep2Dialog)
          ? walletRecoveryPhraseStep2Container
          : false}
      </div>
    );
  }
}
