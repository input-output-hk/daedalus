// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
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
import styles from './WalletRecoveryPhrase.scss';
import { WalletStatuses } from '../../../domains/Wallet';
import { RECOVERY_PHRASE_VERIFICATION_WARNING } from '../../../config/walletsConfig';
import WalletRecoveryPhraseStep1Dialog from './WalletRecoveryPhraseStep1Dialog';
import WalletRecoveryPhraseStep2Dialog from './WalletRecoveryPhraseStep2Dialog';
import WalletRecoveryPhraseStep3Dialog from './WalletRecoveryPhraseStep3Dialog';
import WalletRecoveryPhraseStep4Dialog from './WalletRecoveryPhraseStep4Dialog';

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
      '!!!We recommend that you verify your wallet recovery phrase in <b>{timeUntilWarning}</b>.',
    description:
      'Label for the recoveryPhraseValidationNeverOk on wallet settings.',
  },
  recoveryPhraseValidationNeverWarning: {
    id: 'wallet.settings.recoveryPhraseValidationNeverWarning',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase.',
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
    defaultMessage: '!!!Verify wallet recovery phrase',
    description:
      'Label for the recoveryPhraseValidationButton on wallet settings.',
  },
});

type Props = {
  walletCreationDate: Date,
  openDialogAction: Function,
  isDialogOpen: Function,
  walletRecoveryPhraseStep1Container: Node,
  walletRecoveryPhraseStep2Container: Node,
  walletRecoveryPhraseStep3Container: Node,
  walletRecoveryPhraseStep4Container: Node,
  mnemonicsConfirmationDate: ?Date,
  mnemonicsConfirmationStatus: string,
  mnemonicsConfirmationStatusType: string,
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
          icon: iconRecoveryPhraseOk,
          message: messages.recoveryPhraseValidationNeverOk,
        },
        warning: {
          icon: iconRecoveryPhraseWarning,
          message: messages.recoveryPhraseValidationNeverWarning,
        },
        notification: {
          icon: iconRecoveryPhraseNotification,
          message: messages.recoveryPhraseValidationNeverNotification,
        },
      },
      alreadyChecked: {
        ok: {
          icon: iconRecoveryPhraseOk,
          message: messages.recoveryPhraseValidationCheckedOk,
        },
        warning: {
          icon: iconRecoveryPhraseWarning,
          message: messages.recoveryPhraseValidationCheckedWarning,
        },
        notification: {
          icon: iconRecoveryPhraseNotification,
          message: messages.recoveryPhraseValidationCheckedNotification,
        },
      },
    };
  }

  get recoveryPhraseStatus() {
    const {
      walletCreationDate,
      mnemonicsConfirmationDate,
      mnemonicsConfirmationStatus,
      mnemonicsConfirmationStatusType,
    } = this.props;

    const statuses = this.statuses[mnemonicsConfirmationStatusType];
    const { icon, message } = statuses[mnemonicsConfirmationStatus];
    const timeAgo = moment(mnemonicsConfirmationDate).fromNow();
    const timeFromCreationToWarning = moment(new Date(walletCreationDate)).add(
      RECOVERY_PHRASE_VERIFICATION_WARNING,
      'days'
    );
    const timeUntilWarning = moment().to(timeFromCreationToWarning, true);
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
      openDialogAction,
      isDialogOpen,
      walletRecoveryPhraseStep1Container,
      walletRecoveryPhraseStep2Container,
      walletRecoveryPhraseStep3Container,
      walletRecoveryPhraseStep4Container,
      mnemonicsConfirmationStatus,
    } = this.props;
    const {
      icon,
      message,
      timeAgo,
      timeUntilWarning,
    } = this.recoveryPhraseStatus;

    const validationStatusStyles = classnames([
      styles.validationStatus,
      styles[`validationStatus${capitalize(mnemonicsConfirmationStatus)}`],
    ]);

    let validationStatusButtonType = 'flat';
    if (mnemonicsConfirmationStatus === WalletStatuses.WARNING)
      validationStatusButtonType = 'primary';
    else if (mnemonicsConfirmationStatus === WalletStatuses.NOTIFICATION)
      validationStatusButtonType = 'attention';

    const validationStatusButtonStyles = classnames([
      styles.validationStatusButton,
      validationStatusButtonType,
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
            }}
          />
          <Button
            className={validationStatusButtonStyles}
            themeOverrides={styles}
            label={intl.formatMessage(messages.recoveryPhraseValidationButton)}
            onClick={() => {
              openDialogAction({
                dialog: WalletRecoveryPhraseStep1Dialog,
              });
            }}
            skin={ButtonSkin}
          />
        </div>

        {isDialogOpen(WalletRecoveryPhraseStep1Dialog)
          ? walletRecoveryPhraseStep1Container
          : false}

        {isDialogOpen(WalletRecoveryPhraseStep2Dialog)
          ? walletRecoveryPhraseStep2Container
          : false}

        {isDialogOpen(WalletRecoveryPhraseStep3Dialog)
          ? walletRecoveryPhraseStep3Container
          : false}

        {isDialogOpen(WalletRecoveryPhraseStep4Dialog)
          ? walletRecoveryPhraseStep4Container
          : false}
      </div>
    );
  }
}
