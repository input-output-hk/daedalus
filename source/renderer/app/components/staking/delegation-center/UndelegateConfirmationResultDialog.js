// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import humanizeDuration from 'humanize-duration';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './UndelegateConfirmationResultDialog.scss';
import globalMessages from '../../../i18n/global-messages';
import sadLogo from '../../../assets/images/untada.inline.svg';

const messages = defineMessages({
  dialogTitle: {
    id: 'staking.delegationCenter.undelegate.result.dialog.title',
    defaultMessage: '!!!Wallet Undelegated',
    description: 'Title for the "Undelegate Result" dialog.',
  },
  description: {
    id: 'staking.delegationCenter.undelegate.result.dialog.description',
    defaultMessage:
      '!!!<p>The stake from your wallet <strong>{walletName}</strong> is no longer delegated and you will soon stop earning rewards for this wallet.</p><p>Your new delegation preferences are now posted on the blockchain <strong>and will take effect at the start of the next Cardano epoch in {timeUntilNextEpochStart}</strong>. For the rest of the current epoch, your previous delegation preferences are still active.</p>',
    description: 'Description for the "Undelegate Result" dialog.',
  },
});

type Props = {
  walletName: string,
  nextEpochStartTime: string,
  currentLocale: string,
  onClose: Function,
};

@observer
export default class UndelegateConfirmationResultDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { walletName, onClose, nextEpochStartTime, currentLocale } = this.props;
    const actions = [
      {
        label: intl.formatMessage(globalMessages.close),
        onClick: onClose,
        primary: true,
      },
    ];

    const timeLeft = Math.max(
      0,
      new Date(nextEpochStartTime).getTime() - new Date().getTime()
    );

    let humanizedDurationLanguage;
    switch (currentLocale) {
      case 'ja-JP':
        humanizedDurationLanguage = 'ja';
        break;
      case 'zh-CN':
        humanizedDurationLanguage = 'zh_CN';
        break;
      case 'ko-KR':
        humanizedDurationLanguage = 'ko';
        break;
      case 'de-DE':
        humanizedDurationLanguage = 'de';
        break;
      default:
        humanizedDurationLanguage = 'en';
    }

    const timeUntilNextEpochStart = humanizeDuration(timeLeft || 0, {
      round: true, // round seconds to prevent e.g. 1 day 3 hours *11,56 seconds*
      language: humanizedDurationLanguage,
      conjunction: ' and ',
      units: ['d', 'h', 'm'],
      serialComma: false,
    });

    return (
      <Dialog
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={styles.dialog}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <div className={styles.sadLogoContainer}>
          <SVGInline svg={sadLogo} className={styles.sadLogoIcon} />
        </div>
        <div className={styles.description}>
          <FormattedHTMLMessage
            {...messages.description}
            values={{
              walletName,
              timeUntilNextEpochStart,
            }}
          />
        </div>
      </Dialog>
    );
  }
}
