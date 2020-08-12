// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './RedemptionUnavailableDialog.scss';

const messages = defineMessages({
  title: {
    id: 'staking.redeemItnRewards.redemptionUnavailable.title',
    defaultMessage: '!!!Redeem Incentivized Testnet rewards',
    description:
      'Title for Redeem Incentivized Testnet - redemptionUnavailable',
  },
  description: {
    id: 'staking.redeemItnRewards.redemptionUnavailable.description',
    defaultMessage:
      '!!!Before you can redeem your Incentivized Testnet rewards, Daedalus first needs to synchronize with the blockchain. The synchronization process is now underway and is currently {syncPercentage}% complete. As soon as this process is fully complete, you’ll be able to redeem your Incentivized Testnet rewards. Please wait for this process to complete before returning here to redeem your rewards.',
    description:
      'description for Redeem Incentivized Testnet - redemptionUnavailable',
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
          <FormattedHTMLMessage
            {...messages.description}
            values={{
              syncPercentage: parseFloat(syncPercentage).toFixed(2),
            }}
          />
        </div>
      </Dialog>
    );
  }
}
