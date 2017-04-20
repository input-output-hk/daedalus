// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import styles from './AdaRedemptionSuccessOverlay.scss';
import successIcon from '../../../assets/images/success-big.svg';
import closeCrossWhite from '../../../assets/images/close-cross-white.svg';

const messages = defineMessages({
  headline: {
    id: 'wallet.redeem.success.overlay.headline',
    defaultMessage: '!!!You have successfully redeemed',
    description: 'Headline for the ada redemption success overlay.'
  },
});

@observer
export default class AdaRedemptionSuccessOverlay extends Component {

  props: {
    amount: number,
    onClose: Function,
  };

  static contextTypes = {
    intl: intlShape.isRequired
  };

  render() {
    const { intl } = this.context;
    const { amount, onClose } = this.props;
    return (
      <div className={styles.component}>
        <img className={styles.icon} src={successIcon} role="presentation" />
        <div className={styles.text}>
          <h1 className={styles.headline}>{intl.formatMessage(messages.headline)}</h1>
          <div className={styles.amount}>{amount} ADA</div>
        </div>
        <DialogCloseButton onClose={onClose} icon={closeCrossWhite} />
      </div>
    );
  }

}
