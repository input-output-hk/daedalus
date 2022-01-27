import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './UndelegateWalletSuccessDialo... Remove this comment to see the full error message
import styles from './UndelegateWalletSuccessDialog.scss';
import globalMessages from '../../../i18n/global-messages';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/untada.... Remove this comment to see the full error message
import sadLogo from '../../../assets/images/untada.inline.svg';
import humanizeDurationByLocale from '../../../utils/humanizeDurationByLocale';
import { EPOCH_COUNTDOWN_INTERVAL } from '../../../config/stakingConfig';

const messages = defineMessages({
  title: {
    id: 'wallet.settings.undelegate.result.dialog.title',
    defaultMessage: '!!!Wallet undelegated',
    description: 'Title for the "Undelegate Result" dialog.',
  },
  description1: {
    id: 'wallet.settings.undelegate.result.dialog.description1',
    defaultMessage:
      '!!!The stake from your wallet <strong>{walletName}</strong> is no longer delegated and you will soon stop earning rewards for this wallet.',
    description: 'Description 1 for the "Undelegate Result" dialog.',
  },
  description2: {
    id: 'wallet.settings.undelegate.result.dialog.description2',
    defaultMessage:
      '!!!Your new delegation preferences are now posted on the blockchain <strong>and will take effect after both the current and next Cardano epochs have completed in {timeUntilNextEpochStart}</strong>. During this time, your previous delegation preferences are still active.',
    description: 'Description 2 for the "Undelegate Result" dialog.',
  },
});
type Props = {
  walletName: string;
  futureEpochStartTime: string;
  currentLocale: string;
  onClose: (...args: Array<any>) => any;
};
type State = {
  timeUntilNextEpochStart: number;
};

@observer
class UndelegateWalletSuccessDialog extends Component<Props, State> {
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  intervalHandler: IntervalID | null | undefined = null;
  state = {
    timeUntilNextEpochStart: 0,
  };
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
        title={intl.formatMessage(messages.title)}
        subtitle={walletName}
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

export default UndelegateWalletSuccessDialog;
