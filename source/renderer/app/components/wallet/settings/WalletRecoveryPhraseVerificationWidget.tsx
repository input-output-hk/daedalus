import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { camelCase } from 'lodash';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import moment from 'moment';
import SVGInline from 'react-svg-inline';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/recover... Remove this comment to see the full error message
import iconOk from '../../../assets/images/recovery-phrase-verification-ok.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/recover... Remove this comment to see the full error message
import iconWarning from '../../../assets/images/recovery-phrase-verification-warning.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/recover... Remove this comment to see the full error message
import iconNotification from '../../../assets/images/recovery-phrase-verification-notification.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletRecoveryPhraseVerifica... Remove this comment to see the full error message
import styles from './WalletRecoveryPhraseVerificationWidget.scss';
import {
  RECOVERY_PHRASE_VERIFICATION_STATUSES as statuses,
  RECOVERY_PHRASE_VERIFICATION_TIMES as times,
} from '../../../config/walletRecoveryPhraseVerificationConfig';
import { getStatusFromWalletData } from '../../../utils/walletRecoveryPhraseVerificationUtils';
import { LOCALES } from '../../../../../common/types/locales.types';

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
  paperWalletDescription: {
    id: 'wallet.settings.recoveryPhraseVerification.paperWalletDescription',
    defaultMessage:
      '!!!If this wallet was restored from a paper wallet certificate, you cannot use this feature to verify your wallet recovery phrase. Paper wallet recovery phrase to regular wallet recovery phrase conversion will be available in Daedalus soon.',
    description:
      'Description for the paperWallet instructions on wallet settings.',
  },
  paperWalletTitle: {
    id: 'wallet.settings.recoveryPhraseVerification.paperWalletTitle',
    defaultMessage: '!!!Paper wallet',
    description: 'Title for the paperWallet instructions on wallet settings.',
  },
  button: {
    id: 'wallet.settings.recoveryPhraseVerification.button',
    defaultMessage: '!!!Verify wallet recovery phrase',
    description:
      'Label for the recoveryPhraseVerificationButton on wallet settings.',
  },
  timeUntilWarningReplacement: {
    id:
      'wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement',
    defaultMessage: '!!!ヶ月,か月',
    description:
      'Label for the recoveryPhraseVerificationButton on wallet settings.',
  },
});
export type Props = {
  creationDate: Date;
  recoveryPhraseVerificationDate: Date | null | undefined;
  onVerify: (...args: Array<any>) => any;
  wordCount: number;
  locale: string;
  isLegacy: boolean;
};

@observer
class WalletRecoveryPhraseVerificationWidget extends Component<Props> {
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
    let timeUntilWarning = moment()
      .locale(locale)
      .to(timeFromCreationToWarning, true);

    // The content is generated by `moment`, but we need to replace `ヶ月` by `か月`
    if (locale === LOCALES.japanese) {
      const replacement = this.context.intl
        .formatMessage(messages.timeUntilWarningReplacement)
        .split(',');
      // @ts-ignore ts-migrate(2556) FIXME: Expected 2 arguments, but got 0 or more.
      timeUntilWarning = timeUntilWarning.replace(...replacement);
    }

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
      isLegacy,
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
          {isLegacy && (
            <>
              &nbsp;
              <PopOver
                maxWidth={700}
                content={
                  <div className={styles.paperWalletTooltip}>
                    {intl.formatMessage(messages.paperWalletDescription)}
                  </div>
                }
              >
                <div className={styles.paperWallet}>
                  {intl.formatMessage(messages.paperWalletTitle)}
                </div>
              </PopOver>
            </>
          )}
        </div>
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

export default WalletRecoveryPhraseVerificationWidget;
