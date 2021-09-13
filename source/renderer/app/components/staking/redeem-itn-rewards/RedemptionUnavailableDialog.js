// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import BigNumber from 'bignumber.js';
import globalMessages from '../../../i18n/global-messages';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import styles from './RedemptionUnavailableDialog.scss';

const messages = defineMessages({
  title: {
    id: 'staking.redeemItnRewards.redemptionUnavailable.title',
    defaultMessage: '!!!Redeem Incentivized Testnet rewards',
    description:
      'Title for Redeem Incentivized Testnet - redemptionUnavailable',
  },
  closeButtonLabel: {
    id: 'staking.redeemItnRewards.redemptionUnavailable.closeButton.label',
    defaultMessage: '!!!Close',
    description:
      'closeButtonLabel for Redeem Incentivized Testnet - redemptionUnavailable',
  },
});

type Props = {
  onClose: Function,
  syncPercentage: number,
};

@observer
export default class RedemptionUnavailableDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { onClose, syncPercentage } = this.props;

    const closeButton = <DialogCloseButton onClose={onClose} />;

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={[
          {
            className: 'primary',
            primary: true,
            label: intl.formatMessage(messages.closeButtonLabel),
            onClick: onClose,
          },
        ]}
        closeButton={closeButton}
        onClose={onClose}
        closeOnOverlayClick={false}
        fullSize
      >
        <div className={styles.component}>
          <LoadingSpinner big />
          <div className={styles.description}>
            <FormattedHTMLMessage
              {...globalMessages.featureUnavailableWhileSyncing}
              values={{
                syncPercentage: new BigNumber(syncPercentage).toFormat(2),
              }}
            />
          </div>
        </div>
      </Dialog>
    );
  }
}
