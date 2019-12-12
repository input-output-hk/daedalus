// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './UndelegateConfirmationResultDialog.scss';
import globalMessages from '../../../i18n/global-messages';
import sadLogo from '../../../assets/images/untada.inline.svg';
import humanizeDurationByLocale from '../../../utils/humanizeDurationByLocale';
import { EPOCH_COUNTDOWN_INTERVAL } from '../../../config/epochsConfig';

const messages = defineMessages({
  dialogTitle: {
    id: 'staking.delegationCenter.undelegate.result.dialog.title',
    defaultMessage: '!!!Wallet undelegated',
    description: 'Title for the "Undelegate Result" dialog.',
  },
  description1: {
    id: 'staking.delegationCenter.undelegate.result.dialog.description1',
    defaultMessage:
      '!!!The stake from your wallet <strong>{walletName}</strong> is no longer delegated and you will soon stop earning rewards for this wallet.',
    description: 'Description 1 for the "Undelegate Result" dialog.',
  },
  description2: {
    id: 'staking.delegationCenter.undelegate.result.dialog.description2',
    defaultMessage:
      '!!!Your new delegation preferences are now posted on the blockchain <strong>and will take effect after the next two Cardano epochs have completed in {timeUntilNextEpochStart}</strong>. During this time, your previous delegation preferences are still active.',
    description: 'Description 2 for the "Undelegate Result" dialog.',
  },
});

type Props = {
  walletName: string,
  nextEpochStartTime: string,
  currentLocale: string,
  onClose: Function,
};
type State = { timeUntilNextEpochStart: number };

@observer
export default class UndelegateConfirmationResultDialog extends Component<
  Props,
  State
> {
  intervalHandler: ?IntervalID = null;
  state = { timeUntilNextEpochStart: 0 };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentDidMount() {
    this.updateTimeUntilNextEpochStart();
    this.intervalHandler = setInterval(
      () => this.updateTimeUntilNextEpochStart(),
      EPOCH_COUNTDOWN_INTERVAL
    );
  }

  updateTimeUntilNextEpochStart = () => {
    const { nextEpochStartTime } = this.props;
    const timeUntilNextEpochStart = Math.max(
      0,
      new Date(nextEpochStartTime).getTime() - new Date().getTime()
    );
    this.setState({ timeUntilNextEpochStart });
  };

  componentWillUnmount() {
    if (this.intervalHandler) {
      clearInterval(this.intervalHandler);
    }
  }

  render() {
    const { intl } = this.context;
    const { walletName, onClose, currentLocale } = this.props;
    const actions = [
      {
        label: intl.formatMessage(globalMessages.close),
        onClick: onClose,
        primary: true,
      },
    ];

    const timeUntilNextEpochStart = humanizeDurationByLocale(
      this.state.timeUntilNextEpochStart,
      currentLocale
    );

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
          <p>
            <FormattedHTMLMessage
              {...messages.description1}
              values={{
                walletName,
              }}
            />
          </p>
          <p>
            <FormattedHTMLMessage
              {...messages.description2}
              values={{
                timeUntilNextEpochStart,
              }}
            />
          </p>
        </div>
      </Dialog>
    );
  }
}
