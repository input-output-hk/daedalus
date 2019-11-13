// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import externalLinkIcon from '../../assets/images/link-ic.inline.svg';
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
  addWallet: {
    id: 'wallet.legacy.notification.addWallet',
    defaultMessage: '!!!Move all of the ada from this wallet',
    description: 'Add wallet action of legacy notification.',
  },
  learnMoreLinkUrl: {
    id: 'wallet.legacy.notification.learnMore.url',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360038726373',
    description: '"Learn more" link URL',
  },
});

type Props = {
  onLearnMore: Function,
  onTransferFunds: Function,
  hasAnyWallets?: boolean,
  onWalletAdd?: boolean,
};

@observer
export default class LegacyNotification extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  onLearnMore = () => {
    const { intl } = this.context;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);
    this.props.onLearnMore(learnMoreLinkUrl);
  };

  render() {
    const { intl } = this.context;
    const { onTransferFunds, hasAnyWallets, onWalletAdd } = this.props;
    const title = intl.formatMessage(messages.title);
    const description = intl.formatMessage(messages.description);

    const buttonLabel = hasAnyWallets
      ? intl.formatMessage(messages.actionMove)
      : intl.formatMessage(messages.addWallet);

    const buttonAction = hasAnyWallets ? onTransferFunds : onWalletAdd;

    return (
      <div className={styles.component}>
        <div className={styles.title}>{title}</div>
        <div className={styles.description}>{description}</div>
        <div className={styles.actions}>
          <Button
            className={styles.actionLearnMore}
            label={
              <p>
                {intl.formatMessage(messages.actionLearnMore)}
                <SVGInline
                  svg={externalLinkIcon}
                  className={styles.externalLinkIcon}
                />
              </p>
            }
            onClick={this.onLearnMore}
            skin={ButtonSkin}
          />
          {
            <Button
              className={styles.actionMove}
              label={buttonLabel}
              onClick={buttonAction}
              skin={ButtonSkin}
            />
          }
        </div>
      </div>
    );
  }
}
