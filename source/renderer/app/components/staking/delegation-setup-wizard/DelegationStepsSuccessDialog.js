// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classNames from 'classnames';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
import humanizeDuration from 'humanize-duration';
import commonStyles from './DelegationSteps.scss';
import styles from './DelegationStepsSuccessDialog.scss';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import tadaImage from '../../../assets/images/tada-ic.inline.svg';
import Wallet from '../../../domains/Wallet';
import type { StakePool } from '../../../api/staking/types';

const messages = defineMessages({
  title: {
    id: 'staking.delegationSetup.success.step.dialog.title',
    defaultMessage: '!!!Wallet Delegated',
    description:
      'Title "Wallet Delegated" on the delegation setup "success" step dialog.',
  },
  descriptionLine1: {
    id: 'staking.delegationSetup.success.step.dialog.description.line1',
    defaultMessage: '!!!The stake from your wallet <span>{delegatedWalletName}</span> is now delegated to the <span>[{delegatedStakePoolTicker}]</span> stake pool.',
    description:
      'Description "line 1" on the delegation setup "success" step dialog.',
  },
  descriptionLine2: {
    id: 'staking.delegationSetup.success.step.dialog.description.line2',
    defaultMessage: '!!!Your new delegation preferences are now posted on the blockchain <span>and will take effect at the start of the next Cardano epoch in {timeUntilNextEpochStart}</span>. For the rest of the current epoch, your previous delegation preferences are still active.',
    description:
      'Description "line 2" on the delegation setup "success" step dialog.',
  },
  closeButtonLabel: {
    id: 'staking.delegationSetup.success.step.dialog.closeButtonLabel',
    defaultMessage: '!!!Close',
    description:
      'Label for Close button on the delegation setup "success" step dialog.',
  },
});

type Props = {
  delegatedWallet: Wallet,
  delegatedStakePool: StakePool,
  nextEpochStartTime: string,
  onClose: Function,
  currentLocale: string,
};

@observer
export default class DelegationStepsSuccessDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      delegatedWallet,
      delegatedStakePool,
      nextEpochStartTime,
      currentLocale,
      onClose,
    } = this.props;

    const actions = [
      {
        className: 'closeButton',
        label: intl.formatMessage(messages.closeButtonLabel),
        onClick: onClose,
        primary: true,
      },
    ];

    const dialogClassName = classNames([
      commonStyles.delegationSteps,
      styles.delegationStepsSuccessDialogWrapper,
    ]);
    const contentClasses = classNames([commonStyles.content, styles.content]);

    const delegatedWalletName = get(delegatedWallet, 'name');
    const delegatedStakePoolTicker = get(delegatedStakePool, 'slug');

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

    const timeUntilNextEpochStart = humanizeDuration((timeLeft || 0), {
      round: true, // round seconds to prevent e.g. 1 day 3 hours *11,56 seconds*
      language: humanizedDurationLanguage,
      conjunction: ' and ',
      units: ['d', 'h', 'm'],
      serialComma: false,
    });

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={dialogClassName}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <div className={contentClasses}>
          <SVGInline svg={tadaImage} className={styles.tadaImage} />
          <div className={styles.description1}>
            <FormattedHTMLMessage
              {...messages.descriptionLine1}
              values={{ delegatedWalletName, delegatedStakePoolTicker }}
            />
          </div>
          <div className={styles.description2}>
            <FormattedHTMLMessage
              {...messages.descriptionLine2}
              values={{ timeUntilNextEpochStart }}
            />
          </div>
        </div>
      </Dialog>
    );
  }
}
