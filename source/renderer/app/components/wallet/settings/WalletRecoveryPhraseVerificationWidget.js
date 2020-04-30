// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { camelCase } from 'lodash';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import moment from 'moment';
import SVGInline from 'react-svg-inline';
import iconOk from '../../../assets/images/recovery-phrase-verification-ok.inline.svg';
import iconWarning from '../../../assets/images/recovery-phrase-verification-warning.inline.svg';
import iconNotification from '../../../assets/images/recovery-phrase-verification-notification.inline.svg';
import styles from './WalletRecoveryPhraseVerificationWidget.scss';
import {
  RECOVERY_PHRASE_VERIFICATION_STATUSES as statuses,
  RECOVERY_PHRASE_VERIFICATION_TIMES as times,
} from '../../../config/walletRecoveryPhraseVerificationConfig';
import { getStatusFromWalletData } from '../../../utils/walletRecoveryPhraseVerificationUtils';

export const messages = defineMessages({
  title: {
    id: 'wallet.settings.recoveryPhraseVerification.title',
    defaultMessage: '!!!Do you have your wallet recovery phrase?',
    description:
      'Label for the recoveryPhraseVerificationTitle on wallet settings.',
  },
  description: {
    id: 'wallet.settings.recoveryPhraseVerification.description',
    defaultMessage:
      '!!!Funds in this wallet can only be recovered using the correct wallet recovery phrase, which is a unique {wordCount}-word string you were shown and asked to write down when creating this wallet. You can re-enter your wallet recovery phrase to verify that you have the correct recovery phrase for this wallet.',
    description:
      'Label for the recoveryPhraseVerificationDescription on wallet settings.',
  },
  neverOkTimeUntil: {
    id: 'wallet.settings.recoveryPhraseVerification.neverOkTimeUntil',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in <b>{timeUntilWarning}</b>.',
    description:
      'Label for the recoveryPhraseVerificationNeverOk on wallet settings.',
  },
  neverOkFewMonths: {
    id: 'wallet.settings.recoveryPhraseVerification.neverOkFewMonths',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in a few months.',
    description:
      'Label for the recoveryPhraseVerificationNeverOk on wallet settings.',
  },
  neverOkFewWeeks: {
    id: 'wallet.settings.recoveryPhraseVerification.neverOkFewWeeks',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in a few weeks.',
    description:
      'Label for the recoveryPhraseVerificationNeverOk on wallet settings.',
  },
  neverOkFewDays: {
    id: 'wallet.settings.recoveryPhraseVerification.neverOkFewDays',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in a few days.',
    description:
      'Label for the recoveryPhraseVerificationNeverOk on wallet settings.',
  },
  neverWarning: {
    id: 'wallet.settings.recoveryPhraseVerification.neverWarning',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase.',
    description:
      'Label for the recoveryPhraseVerificationNeverWarning on wallet settings.',
  },
  neverNotification: {
    id: 'wallet.settings.recoveryPhraseVerification.neverNotification',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase.',
    description:
      'Label for the recoveryPhraseVerificationNeverNotification on wallet settings.',
  },
  checkedOk: {
    id: 'wallet.settings.recoveryPhraseVerification.checkedOk',
    defaultMessage:
      '!!!You verified the recovery phrase for this wallet <b>{timeAgo}</b>.',
    description:
      'Label for the recoveryPhraseVerificationCheckedOk on wallet settings.',
  },
  checkedWarning: {
    id: 'wallet.settings.recoveryPhraseVerification.checkedWarning',
    defaultMessage:
      '!!!You verified the recovery phrase for this wallet <b>{timeAgo}</b>.',
    description:
      'Label for the recoveryPhraseVerificationCheckedWarning on wallet settings.',
  },
  checkedNotification: {
    id: 'wallet.settings.recoveryPhraseVerification.checkedNotification',
    defaultMessage:
      '!!!You verified the recovery phrase for this wallet <b>{timeAgo}</b>. We recommend that you verify your wallet recovery phrase again.',
    description:
      'Label for the recoveryPhraseVerificationCheckedNotification on wallet settings.',
  },
  button: {
    id: 'wallet.settings.recoveryPhraseVerification.button',
    defaultMessage: '!!!Verify wallet recovery phrase',
    description:
      'Label for the recoveryPhraseVerificationButton on wallet settings.',
  },
});

export type Props = {
  creationDate: Date,
  recoveryPhraseVerificationDate: ?Date,
  onVerify: Function,
  wordCount: number,
  locale: string,
};

@observer
export default class WalletRecoveryPhraseVerificationWidget extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  get statuses() {
    return {
      neverVerified_okTimeUntil: {
        icon: iconOk,
        message: messages.neverOkTimeUntil,
      },
      neverVerified_okFewMonths: {
        icon: iconOk,
        message: messages.neverOkFewMonths,
      },
      neverVerified_okFewWeeks: {
        icon: iconOk,
        message: messages.neverOkFewWeeks,
      },
      neverVerified_okFewDays: {
        icon: iconOk,
        message: messages.neverOkFewDays,
      },
      neverVerified_warning: {
        icon: iconWarning,
        message: messages.neverWarning,
      },
      neverVerified_notification: {
        icon: iconNotification,
        message: messages.neverNotification,
      },
      alreadyVerified_ok: {
        icon: iconOk,
        message: messages.checkedOk,
      },
      alreadyVerified_warning: {
        icon: iconWarning,
        message: messages.checkedWarning,
      },
      alreadyVerified_notification: {
        icon: iconNotification,
        message: messages.checkedNotification,
      },
    };
  }

  get recoveryPhraseStatus() {
    const { creationDate, recoveryPhraseVerificationDate, locale } = this.props;
    const {
      recoveryPhraseVerificationStatus: status,
      recoveryPhraseVerificationStatusType: type,
    } = getStatusFromWalletData({
      creationDate,
      recoveryPhraseVerificationDate,
    });
    const { icon, message } = this.statuses[`${type}_${status}`];
    const timeAgo = moment(recoveryPhraseVerificationDate).fromNow();
    const timeFromCreationToWarning = moment(new Date(creationDate)).add(
      times.warning,
      'days'
    );
    const timeUntilWarning = moment()
      .locale(locale)
      .to(timeFromCreationToWarning, true);
    return {
      icon,
      message,
      timeAgo,
      timeUntilWarning,
    };
  }

  render() {
    const { intl } = this.context;
    const {
      onVerify,
      wordCount,
      creationDate,
      recoveryPhraseVerificationDate,
    } = this.props;
    const {
      icon,
      message,
      timeAgo,
      timeUntilWarning,
    } = this.recoveryPhraseStatus;
    const { recoveryPhraseVerificationStatus } = getStatusFromWalletData({
      creationDate,
      recoveryPhraseVerificationDate,
    });

    const statusStyle = camelCase(`status ${recoveryPhraseVerificationStatus}`);
    const statusStyles = classnames([styles.status, styles[statusStyle]]);

    let statusButtonType = 'flat';
    if (recoveryPhraseVerificationStatus === statuses.WARNING)
      statusButtonType = 'primary';
    else if (recoveryPhraseVerificationStatus === statuses.NOTIFICATION)
      statusButtonType = 'attention';

    const statusButtonStyles = classnames([
      styles.statusButton,
      statusButtonType,
    ]);

    return (
      <div className={styles.component}>
        <h2>{intl.formatMessage(messages.title)}</h2>
        <div className={styles.description}>
          {intl.formatMessage(messages.description, {
            wordCount,
          })}
        </div>
        <br />
        <div className={statusStyles}>
          <SVGInline svg={icon} className={styles.statusIcon} />
          <FormattedHTMLMessage
            {...message}
            className={styles.statusMessage}
            values={{
              timeAgo,
              timeUntilWarning,
            }}
          />
          <Button
            className={statusButtonStyles}
            themeOverrides={styles}
            label={intl.formatMessage(messages.button)}
            onClick={onVerify}
            skin={ButtonSkin}
          />
        </div>
      </div>
    );
  }
}
