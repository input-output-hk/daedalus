// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { capitalize } from 'lodash';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import moment from 'moment';
import SVGInline from 'react-svg-inline';
import iconRecoveryPhraseOk from '../../../assets/images/recovery-phrase-verification-ok.inline.svg';
import iconRecoveryPhraseWarning from '../../../assets/images/recovery-phrase-verification-warning.inline.svg';
import iconRecoveryPhraseNotification from '../../../assets/images/recovery-phrase-verification-notification.inline.svg';
import styles from './WalletRecoveryPhraseVerificationWidget.scss';
import {
  RECOVERY_PHRASE_VERIFICATION_STATUSES,
  RECOVERY_PHRASE_VERIFICATION_WARNING,
} from '../../../config/walletRecoveryPhraseVerificationConfig';
import { getStatusFromWalletData } from '../../../utils/walletRecoveryPhraseVerificationUtils';

export const messages = defineMessages({
  recoveryPhraseVerificationTitle: {
    id: 'wallet.settings.recoveryPhraseVerificationTitle',
    defaultMessage: '!!!Do you have your wallet recovery phrase?',
    description:
      'Label for the recoveryPhraseVerificationTitle on wallet settings.',
  },
  recoveryPhraseVerificationDescription: {
    id: 'wallet.settings.recoveryPhraseVerificationDescription',
    defaultMessage:
      '!!!Funds in this wallet can only be recovered using the correct wallet recovery phrase, which is a unique {wordCount}-word string you were shown and asked to write down when creating this wallet. You can re-enter your wallet recovery phrase to verify that you have the correct recovery phrase for this wallet.',
    description:
      'Label for the recoveryPhraseVerificationDescription on wallet settings.',
  },

  recoveryPhraseVerificationNeverOkTimeUntil: {
    id: 'wallet.settings.recoveryPhraseVerificationNeverOkTimeUntil',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in <b>{timeUntilWarning}</b>.',
    description:
      'Label for the recoveryPhraseVerificationNeverOk on wallet settings.',
  },
  recoveryPhraseVerificationNeverOkFewMonths: {
    id: 'wallet.settings.recoveryPhraseVerificationNeverOkFewMonths',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in a few months.',
    description:
      'Label for the recoveryPhraseVerificationNeverOk on wallet settings.',
  },
  recoveryPhraseVerificationNeverOkFewWeeks: {
    id: 'wallet.settings.recoveryPhraseVerificationNeverOkFewWeeks',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in a few weeks.',
    description:
      'Label for the recoveryPhraseVerificationNeverOk on wallet settings.',
  },
  recoveryPhraseVerificationNeverOkFewDays: {
    id: 'wallet.settings.recoveryPhraseVerificationNeverOkFewDays',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in in a few days.',
    description:
      'Label for the recoveryPhraseVerificationNeverOk on wallet settings.',
  },
  recoveryPhraseVerificationNeverWarning: {
    id: 'wallet.settings.recoveryPhraseVerificationNeverWarning',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase.',
    description:
      'Label for the recoveryPhraseVerificationNeverWarning on wallet settings.',
  },
  recoveryPhraseVerificationNeverNotification: {
    id: 'wallet.settings.recoveryPhraseVerificationNeverNotification',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase.',
    description:
      'Label for the recoveryPhraseVerificationNeverNotification on wallet settings.',
  },
  recoveryPhraseVerificationCheckedOk: {
    id: 'wallet.settings.recoveryPhraseVerificationCheckedOk',
    defaultMessage:
      '!!!You verified the recovery phrase for this wallet <b>{timeAgo}</b>.',
    description:
      'Label for the recoveryPhraseVerificationCheckedOk on wallet settings.',
  },
  recoveryPhraseVerificationCheckedWarning: {
    id: 'wallet.settings.recoveryPhraseVerificationCheckedWarning',
    defaultMessage:
      '!!!You verified the recovery phrase for this wallet <b>{timeAgo}</b>.',
    description:
      'Label for the recoveryPhraseVerificationCheckedWarning on wallet settings.',
  },
  recoveryPhraseVerificationCheckedNotification: {
    id: 'wallet.settings.recoveryPhraseVerificationCheckedNotification',
    defaultMessage:
      '!!!You verified the recovery phrase for this wallet <b>{timeAgo}</b>. We recommend that you verify your wallet recovery phrase again.',
    description:
      'Label for the recoveryPhraseVerificationCheckedNotification on wallet settings.',
  },
  recoveryPhraseVerificationButton: {
    id: 'wallet.settings.recoveryPhraseVerificationButton',
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
};

@observer
export default class WalletRecoveryPhraseVerificationWidget extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  test = (file: string): Date => {
    if (file === 'blah') return this.props.creationDate;
    return new Date();
  };

  get statuses() {
    return {
      neverChecked: {
        okTimeUntil: {
          icon: iconRecoveryPhraseOk,
          message: messages.recoveryPhraseVerificationNeverOkTimeUntil,
        },
        okFewMonths: {
          icon: iconRecoveryPhraseOk,
          message: messages.recoveryPhraseVerificationNeverOkFewMonths,
        },
        okFewWeeks: {
          icon: iconRecoveryPhraseOk,
          message: messages.recoveryPhraseVerificationNeverOkFewWeeks,
        },
        okFewDays: {
          icon: iconRecoveryPhraseOk,
          message: messages.recoveryPhraseVerificationNeverOkFewDays,
        },
        warning: {
          icon: iconRecoveryPhraseWarning,
          message: messages.recoveryPhraseVerificationNeverWarning,
        },
        notification: {
          icon: iconRecoveryPhraseNotification,
          message: messages.recoveryPhraseVerificationNeverNotification,
        },
      },
      alreadyChecked: {
        ok: {
          icon: iconRecoveryPhraseOk,
          message: messages.recoveryPhraseVerificationCheckedOk,
        },
        warning: {
          icon: iconRecoveryPhraseWarning,
          message: messages.recoveryPhraseVerificationCheckedWarning,
        },
        notification: {
          icon: iconRecoveryPhraseNotification,
          message: messages.recoveryPhraseVerificationCheckedNotification,
        },
      },
    };
  }

  get recoveryPhraseStatus() {
    const { locale } = this.context.intl;
    const { creationDate, recoveryPhraseVerificationDate } = this.props;
    const {
      recoveryPhraseVerificationStatus,
      recoveryPhraseVerificationStatusType,
    } = getStatusFromWalletData({
      creationDate,
      recoveryPhraseVerificationDate,
    });

    const statuses = this.statuses[recoveryPhraseVerificationStatusType];
    const { icon, message } = statuses[recoveryPhraseVerificationStatus];
    const timeAgo = moment(recoveryPhraseVerificationDate).fromNow();
    const timeFromCreationToWarning = moment(new Date(creationDate)).add(
      RECOVERY_PHRASE_VERIFICATION_WARNING,
      'days'
    );
    // console.log('timeFromCreationToWarning', timeFromCreationToWarning);
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

    const validationStatusStyles = classnames([
      styles.validationStatus,
      styles[`validationStatus${capitalize(recoveryPhraseVerificationStatus)}`],
    ]);

    let validationStatusButtonType = 'flat';
    if (
      recoveryPhraseVerificationStatus ===
      RECOVERY_PHRASE_VERIFICATION_STATUSES.WARNING
    )
      validationStatusButtonType = 'primary';
    else if (
      recoveryPhraseVerificationStatus ===
      RECOVERY_PHRASE_VERIFICATION_STATUSES.NOTIFICATION
    )
      validationStatusButtonType = 'attention';

    const validationStatusButtonStyles = classnames([
      styles.validationStatusButton,
      validationStatusButtonType,
    ]);

    return (
      <div className={styles.component}>
        <h2>{intl.formatMessage(messages.recoveryPhraseVerificationTitle)}</h2>
        <div className={styles.description}>
          {intl.formatMessage(messages.recoveryPhraseVerificationDescription, {
            wordCount,
          })}
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
            }}
          />
          <Button
            className={validationStatusButtonStyles}
            themeOverrides={styles}
            label={intl.formatMessage(
              messages.recoveryPhraseVerificationButton
            )}
            onClick={onVerify}
            skin={ButtonSkin}
          />
        </div>
      </div>
    );
  }
}
