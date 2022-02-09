import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classNames from 'classnames';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegationSteps.scss' or its... Remove this comment to see the full error message
import commonStyles from './DelegationSteps.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegationStepsSuccessDialog... Remove this comment to see the full error message
import styles from './DelegationStepsSuccessDialog.scss';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/tada-ic... Remove this comment to see the full error message
import tadaImage from '../../../assets/images/tada-ic.inline.svg';
import Wallet from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';
import humanizeDurationByLocale from '../../../utils/humanizeDurationByLocale';
import { EPOCH_COUNTDOWN_INTERVAL } from '../../../config/stakingConfig';

const messages = defineMessages({
  title: {
    id: 'staking.delegationSetup.success.step.dialog.title',
    defaultMessage: '!!!Wallet Delegated',
    description:
      'Title "Wallet Delegated" on the delegation setup "success" step dialog.',
  },
  descriptionLine1: {
    id: 'staking.delegationSetup.success.step.dialog.description.line1',
    defaultMessage:
      '!!!The stake from your wallet <span>{delegatedWalletName}</span> is now delegated to the <span>[{delegatedStakePoolTicker}] {delegatedStakePoolName}</span> stake pool.',
    description:
      'Description "line 1" on the delegation setup "success" step dialog.',
  },
  descriptionLine2: {
    id: 'staking.delegationSetup.success.step.dialog.description.line2',
    defaultMessage:
      '!!!Your new delegation preferences are now posted on the Cardano blockchain. <strong>These preferences will take effect after both the current and the next Cardano epochs have completed in {timeUntilNextEpochStart}.</strong> During this time, your previous delegation preferences remain active.',
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
  delegatedWallet: Wallet | null | undefined;
  delegatedStakePool: StakePool | null | undefined;
  futureEpochStartTime: string;
  onClose: (...args: Array<any>) => any;
  currentLocale: string;
};
type State = {
  timeUntilNextEpochStart: number;
};

@observer
class DelegationStepsSuccessDialog extends Component<Props, State> {
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  intervalHandler: IntervalID | null | undefined = null;
  state = {
    timeUntilNextEpochStart: 0,
  };
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentDidMount() {
    this.configureUpdateTimer();
  }

  configureUpdateTimer = () => {
    this.updateTimeUntilNextEpochStart();
    this.intervalHandler = setInterval(
      () => this.updateTimeUntilNextEpochStart(),
      EPOCH_COUNTDOWN_INTERVAL
    );
  };
  updateTimeUntilNextEpochStart = () => {
    const { futureEpochStartTime } = this.props;
    const timeUntilNextEpochStart = Math.max(
      0,
      new Date(futureEpochStartTime).getTime() - new Date().getTime()
    );
    this.setState({
      timeUntilNextEpochStart,
    });
  };

  componentWillUnmount() {
    if (this.intervalHandler) {
      clearInterval(this.intervalHandler);
    }
  }

  render() {
    const { intl } = this.context;
    const {
      delegatedWallet,
      delegatedStakePool,
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
    const delegatedStakePoolName = get(delegatedStakePool, 'name');
    const delegatedStakePoolTicker = get(delegatedStakePool, 'ticker');
    const timeUntilNextEpochStart = humanizeDurationByLocale(
      this.state.timeUntilNextEpochStart,
      currentLocale
    );
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
              values={{
                delegatedWalletName,
                delegatedStakePoolTicker,
                delegatedStakePoolName,
              }}
            />
          </div>
          <div className={styles.description2}>
            <FormattedHTMLMessage
              {...messages.descriptionLine2}
              values={{
                timeUntilNextEpochStart,
              }}
            />
          </div>
        </div>
      </Dialog>
    );
  }
}

export default DelegationStepsSuccessDialog;
