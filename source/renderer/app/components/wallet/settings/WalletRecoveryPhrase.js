// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { capitalize } from 'lodash';
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
  recoveryPhraseValidationTitle: {
    id: 'wallet.settings.recoveryPhraseValidationTitle',
    defaultMessage: '!!!Do you have your wallet recovery phrase?',
    description:
      'Label for the recoveryPhraseValidationTitle on wallet settings.',
  },
  recoveryPhraseValidationDescription: {
    id: 'wallet.settings.recoveryPhraseValidationDescription',
    defaultMessage:
      '!!!Funds in this wallet can only be recovered on another computer using the correct wallet recovery phrase. You can re-enter your wallet recovery phrase to verify that you have the correct recovery phrase for this wallet.',
    description:
      'Label for the recoveryPhraseValidationDescription on wallet settings.',
  },
  recoveryPhraseValidationNeverOk: {
    id: 'wallet.settings.recoveryPhraseValidationNeverOk',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in <b>{timeUntilWarning}</b>',
    description:
      'Label for the recoveryPhraseValidationNeverOk on wallet settings.',
  },
  recoveryPhraseValidationNeverWarning: {
    id: 'wallet.settings.recoveryPhraseValidationNeverWarning',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in <b>{timeUntilNotification}</b>.',
    description:
      'Label for the recoveryPhraseValidationNeverWarning on wallet settings.',
  },
  recoveryPhraseValidationNeverNotification: {
    id: 'wallet.settings.recoveryPhraseValidationNeverNotification',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase.',
    description:
      'Label for the recoveryPhraseValidationNeverNotification on wallet settings.',
  },
  recoveryPhraseValidationCheckedOk: {
    id: 'wallet.settings.recoveryPhraseValidationCheckedOk',
    defaultMessage:
      '!!!You verified the recovery phrase for this wallet <b>{timeAgo}</b>.',
    description:
      'Label for the recoveryPhraseValidationCheckedOk on wallet settings.',
  },
  recoveryPhraseValidationCheckedWarning: {
    id: 'wallet.settings.recoveryPhraseValidationCheckedWarning',
    defaultMessage:
      '!!!You verified the recovery phrase for this wallet <b>{timeAgo}</b>.',
    description:
      'Label for the recoveryPhraseValidationCheckedWarning on wallet settings.',
  },
  recoveryPhraseValidationCheckedNotification: {
    id: 'wallet.settings.recoveryPhraseValidationCheckedNotification',
    defaultMessage:
      '!!!You verified the recovery phrase for this wallet <b>{timeAgo}</b>. We recommend that you verify your wallet recovery phrase again.',
    description:
      'Label for the recoveryPhraseValidationCheckedNotification on wallet settings.',
  },
  recoveryPhraseValidationButton: {
    id: 'wallet.settings.recoveryPhraseValidationButton',
    defaultMessage: '!!!Confirm mnemonics.',
    description:
      'Label for the recoveryPhraseValidationButton on wallet settings.',
  },
});

type Props = {
  mnemonicsConfirmationDate: Date,
  walletCreationDate: Date,
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
      neverChecked: {
        ok: {
          icon: iconMnemonicsOk,
          message: messages.recoveryPhraseValidationNeverOk,
        },
        warning: {
          icon: iconMnemonicsWarning,
          message: messages.recoveryPhraseValidationNeverWarning,
        },
        notification: {
          icon: iconMnemonicsNotification,
          message: messages.recoveryPhraseValidationNeverNotification,
        },
      },
      alreadyChecked: {
        ok: {
          icon: iconMnemonicsOk,
          message: messages.recoveryPhraseValidationCheckedOk,
        },
        warning: {
          icon: iconMnemonicsWarning,
          message: messages.recoveryPhraseValidationCheckedWarning,
        },
        notification: {
          icon: iconMnemonicsNotification,
          message: messages.recoveryPhraseValidationCheckedNotification,
        },
      },
    };
  }

  get recoveryPhraseStatus() {
    const { mnemonicsConfirmationDate, walletCreationDate } = this.props;
    const dateToCheck = mnemonicsConfirmationDate || walletCreationDate;
    const daysSinceDate = moment().diff(moment(dateToCheck), 'days');
    let status = 'ok';
    if (daysSinceDate > MNEMONICS_CHECKING_NOTIFICATION)
      status = 'notification';
    else if (daysSinceDate > MNEMONICS_CHECKING_WARNING) status = 'warning';
    const type = mnemonicsConfirmationDate ? 'alreadyChecked' : 'neverChecked';
    const statuses = this.statuses[type];
    const { icon, message } = statuses[status];
    const timeAgo = moment(mnemonicsConfirmationDate).fromNow();
    const timeUntilWarning = 'few months, more or less';
    const timeUntilNotification = 'couple of days, more or less';
    return {
      icon,
      message,
      type,
      status,
      timeAgo,
      timeUntilWarning,
      timeUntilNotification,
    };
  }

  render() {
    const { intl } = this.context;
    const {
      openDialogAction,
      isDialogOpen,
      walletRecoveryPhraseStep1Container,
      walletRecoveryPhraseStep2Container,
    } = this.props;
    const {
      icon,
      message,
      status,
      timeAgo,
      timeUntilWarning,
      timeUntilNotification,
    } = this.recoveryPhraseStatus;

    const validationStatusStyles = classnames([
      styles.validationStatus,
      styles[`validationStatus${capitalize(status)}`],
    ]);

    return (
      <div className={styles.component}>
        <h2>{intl.formatMessage(messages.recoveryPhraseValidationTitle)}</h2>
        <div className={styles.description}>
          {intl.formatMessage(messages.recoveryPhraseValidationDescription)}
        </div>
        <br />
        <div className={validationStatusStyles}>
          <SVGInline svg={icon} className={styles.validationStatusIcon} />
          <FormattedHTMLMessage
            {...message}
            className={styles.validationStatusMessage}
            values={{
              timeAgo,
              timeUntilWarning,
              timeUntilNotification,
            }}
          />
          <button
            className={styles.validationStatusButton}
            onClick={() => {
              openDialogAction({
                dialog: WalletRecoveryPhraseStep1Dialog,
              });
            }}
          >
            {intl.formatMessage(messages.recoveryPhraseValidationButton)}
          </button>
        </div>

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
