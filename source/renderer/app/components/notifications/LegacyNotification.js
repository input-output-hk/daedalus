// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './LegacyNotification.scss';

const messages = defineMessages({
  title: {
    id: 'wallet.legacy.notification.title',
    defaultMessage: '!!!Move funds from the legacy wallet',
    description:
      'Title "Move funds from the legacy wallet" on the legacy notification.',
  },
  description: {
    id: 'wallet.legacy.notification.description',
    defaultMessage:
      '!!!This is a legacy wallet which uses legacy addresses and does not support new features. Please move all of the ada from this legacy wallet to one of the wallets where new features are available. You can also create a brand new wallet for your ada in case you don’t want to move ada to one of your existing wallets. A sequence of screens will guide you through the process.',
    description: 'Legacy notification description.',
  },
  actionLearnMore: {
    id: 'wallet.legacy.notification.actionLearnMore',
    defaultMessage: '!!!Learn more',
    description: 'Learn more action of legacy notification.',
  },
  actionMove: {
    id: 'wallet.legacy.notification.actionMove',
    defaultMessage: '!!!Move all of the ada from this wallet',
    description: 'Move all ada action of legacy notification.',
  },
});

type Props = {
  onLearnMore: Function,
  onTransferFunds: Function,
};

@observer
export default class LegacyNotification extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onLearnMore, onTransferFunds } = this.props;
    const title = intl.formatMessage(messages.title);
    const description = intl.formatMessage(messages.description);
    const actionLearnMore = intl.formatMessage(messages.actionLearnMore);
    const actionMove = intl.formatMessage(messages.actionMove);

    return (
      <div className={styles.component}>
        <div className={styles.title}>{title}</div>
        <div className={styles.description}>{description}</div>
        <div className={styles.actions}>
          <Button
            className={styles.actionLearnMore}
            label={actionLearnMore}
            onClick={onLearnMore}
            skin={ButtonSkin}
          />
          <Button
            className={styles.actionMove}
            label={actionMove}
            onClick={onTransferFunds}
            skin={ButtonSkin}
          />
        </div>
      </div>
    );
  }
}
