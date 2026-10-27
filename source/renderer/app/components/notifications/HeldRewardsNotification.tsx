import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import type BigNumber from 'bignumber.js';
import styles from './HeldRewardsNotification.scss';
import { DiscreetWalletAmount } from '../../features/discreet-mode';

const messages = defineMessages({
  title: {
    id: 'wallet.heldRewards.notification.title',
    defaultMessage: '!!!Staking rewards on hold',
    description:
      'Title of the notification on a wallet whose staking rewards cannot be withdrawn because its voting power is not delegated',
  },
  description: {
    id: 'wallet.heldRewards.notification.description',
    defaultMessage:
      '!!!{rewardsAmount} of staking rewards cannot be withdrawn from this wallet, because its voting power is not delegated, for example because its DRep retired. Payments still work and leave the rewards in place. Delegate the voting power to any DRep, to Abstain or to No Confidence to release them; they can be withdrawn once that delegation is confirmed.',
    description:
      'Body of that notification. {rewardsAmount} is the reward balance with its unit, hidden in discreet mode',
  },
  delegateButton: {
    id: 'wallet.heldRewards.notification.delegateButton',
    defaultMessage: '!!!Delegate voting power',
    description:
      'Button on that notification; opens the voting power delegation form with this wallet selected',
  },
});

type Props = {
  heldReward: BigNumber;
  onDelegate: (...args: Array<any>) => any;
};

@observer
class HeldRewardsNotification extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { heldReward, onDelegate } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.title}>{intl.formatMessage(messages.title)}</div>
        <div className={styles.description}>
          <FormattedMessage
            {...messages.description}
            values={{
              rewardsAmount: <DiscreetWalletAmount amount={heldReward} />,
            }}
          />
        </div>
        <div className={styles.actions}>
          <Button
            className={styles.actionDelegate}
            label={intl.formatMessage(messages.delegateButton)}
            onClick={onDelegate}
            skin={ButtonSkin}
          />
        </div>
      </div>
    );
  }
}

export default HeldRewardsNotification;
