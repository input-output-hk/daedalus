// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import type { RewardForIncentivizedTestnet } from '../../../api/staking/types';
import styles from './StakingRewardsDialog.scss';
import globalMessages from '../../../i18n/global-messages';

const messages = defineMessages({
  title: {
    id: 'staking.rewards.dialog.title',
    defaultMessage: '!!!Rewards details',
    description:
      'Title "Earned delegation rewards" label on the staking rewards page.',
  },
  exportCsvLabel: {
    id: 'staking.rewards.dialog.csv.label',
    defaultMessage: '!!!Export CSV',
    description:
      'Title "Earned delegation rewards" label on the staking rewards page.',
  },
});

type Props = {
  reward: RewardForIncentivizedTestnet,
  onClose: Function,
};

@observer
export default class StakingRewardsDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { reward, onClose } = this.props;
    const { walletName, rewardsAddress } = reward || {};
    const actions = [
      {
        label: intl.formatMessage(globalMessages.cancel),
        onClick: onClose,
      },
      {
        label: 'Export CSV',
        primary: true,
        onClick: () => {},
      },
    ];

    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.title)}
        subtitle={walletName}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.label}>Rewards address</div>
        <p>{rewardsAddress}</p>
        <div className={styles.label}>Date range</div>
        <input value="..." />
      </Dialog>
    );
  }
}
