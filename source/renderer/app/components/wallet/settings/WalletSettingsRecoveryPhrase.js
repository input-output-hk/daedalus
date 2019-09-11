// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import moment from 'moment';
import SVGInline from 'react-svg-inline';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import iconMnemonicsOk from '../../../assets/images/mnemonics-verification-ok.inline.svg';
import iconMnemonicsWarning from '../../../assets/images/mnemonics-verification-warning.inline.svg';
import iconMnemonicsNotification from '../../../assets/images/mnemonics-verification-notification.inline.svg';
import styles from './WalletSettingsRecoveryPhrase.scss';
import {
  MNEMONICS_CHECKING_WARNING,
  MNEMONICS_CHECKING_NOTIFICATION,
} from '../../../config/walletsConfig';

export const messages = defineMessages({
  mnemonicsValidationTitle: {
    id: 'wallet.settings.mnemonicsValidationTitle',
    defaultMessage: '!!!Do you have your wallet recovery phrase?',
    description: 'Label for the mnemonicsValidationTitle on wallet settings.',
  },
  mnemonicsValidationDescription: {
    id: 'wallet.settings.mnemonicsValidationDescription',
    defaultMessage:
      '!!!Funds in this wallet can only be recovered on another computer using a wallet recovery phrase. You can re-enter your wallet recovery phrase to confirm that you have the correct recovery phrase for this wallet.',
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
};

@observer
export default class WalletSettingsRecoveryPhrase extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  get statuses() {
    return {
      ok: {
        icon: iconMnemonicsOk,
        message: messages.mnemonicsValidationConfirmed,
        tooltip: messages.mnemonicsValidationConfirmed,
        tooltipClassname: styles.tooltipOk,
      },
      warning: {
        icon: iconMnemonicsWarning,
        message: messages.mnemonicsValidationConfirmed,
        tooltip: messages.mnemonicsValidationConfirmed,
        tooltipClassname: styles.tooltipWarning,
      },
      notification: {
        icon: iconMnemonicsNotification,
        message: messages.mnemonicsValidationNotification,
        tooltip: messages.mnemonicsValidationNotification,
        tooltipClassname: styles.tooltipNotification,
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
    const { icon, message, tooltip, tooltipClassname } = this.statuses[status];

    const tooltipClassnameStyles = classnames([
      tooltipClassname,
      styles.tooltipClassname,
    ]);

    return {
      icon,
      message,
      tooltip,
      tooltipClassname: tooltipClassnameStyles,
    };
  }

  render() {
    const { intl } = this.context;
    const { mnemonicsConfirmationDate } = this.props;
    const {
      icon,
      message,
      tooltip,
      tooltipClassname,
    } = this.recoveryPhraseStatus;

    return (
      <div className={styles.component}>
        <h2>
          {intl.formatMessage(messages.mnemonicsValidationTitle)}
          <Tooltip
            skin={TooltipSkin}
            themeOverrides1="{tooltipStyles}"
            tip={intl.formatMessage(tooltip)}
            className={tooltipClassname}
          >
            <SVGInline svg={icon} className={styles.mnemonicsValidationIcon} />
          </Tooltip>
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
        <button className={styles.mnemonicsValidationButton} onClick={() => {}}>
          {intl.formatMessage(messages.mnemonicsValidationButton)}
        </button>
      </div>
    );
  }
}
