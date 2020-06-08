// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import ButtonLink from '../widgets/ButtonLink';
import styles from './LegacyNotification.scss';
import Wallet from '../../domains/Wallet';

const messages = defineMessages({
  moveFundsTitle: {
    id: 'wallet.legacy.notification.moveFundsTitle',
    defaultMessage: '!!!Move funds from {activeWalletName}',
    description:
      'Title "Move funds from the legacy wallet" on the legacy notification.',
  },
  addWalletTitle: {
    id: 'wallet.legacy.notification.addWalletTitle',
    defaultMessage: '!!!Create a new wallet',
    description: 'Title "Create a new wallet" on the legacy notification.',
  },
  descriptionWithNoFunds: {
    id: 'wallet.legacy.notification.descriptionWithNoFunds',
    defaultMessage:
      '!!!"{transferWalletName}"" is a legacy wallet. It does not support Shelley delegation features. Move testnet ada to a Rewards wallet to delegate your testnet ada stake and earn rewards.',
    description: 'Legacy notification description WithNoFunds.',
  },
  descriptionWithFunds: {
    id: 'wallet.legacy.notification.descriptionWithFunds',
    defaultMessage:
      '!!!"{transferWalletName}"" is a legacy wallet. It does not support Shelley delegation features. To earn ada from delegating your stake, please move all funds from this wallet to a new, Shelley-compatible wallet. You can create a brand new wallet or move funds to one of the existing wallets.',
    description: 'Legacy notification description WithFunds.',
  },
  actionLearnMore: {
    id: 'wallet.legacy.notification.actionLearnMore',
    defaultMessage: '!!!Learn more',
    description: 'Learn more action of legacy notification.',
  },
  actionMove: {
    id: 'wallet.legacy.notification.actionMove',
    defaultMessage: '!!!Move ada from this wallet',
    description: 'Move Move ada from this wallet of legacy notification.',
  },
  addWallet: {
    id: 'wallet.legacy.notification.addWallet',
    defaultMessage: '!!!Create a new Rewards wallet',
    description: 'Create a new Rewards wallet action of legacy notification.',
  },
  learnMoreLinkUrl: {
    id: 'wallet.legacy.notification.learnMore.url',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360038726373',
    description: '"Learn more" link URL',
  },
});

type Props = {
  activeWallet: Wallet,
  onLearnMore: Function,
  onTransferFunds: Function,
  hasRewardsWallets?: boolean,
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
    const {
      onTransferFunds,
      hasRewardsWallets,
      onWalletAdd,
      activeWallet,
    } = this.props;
    const buttonAction = hasRewardsWallets ? onTransferFunds : onWalletAdd;

    const buttonLabel = hasRewardsWallets
      ? intl.formatMessage(messages.actionMove)
      : intl.formatMessage(messages.addWallet);

    return (
      <div className={styles.component}>
        <div className={styles.title}>
          {hasRewardsWallets ? (
            <FormattedHTMLMessage
              {...messages.moveFundsTitle}
              values={{
                activeWalletName: activeWallet.name,
              }}
            />
          ) : (
            <FormattedHTMLMessage
              {...messages.addWalletTitle}
              values={{
                activeWalletName: activeWallet.name,
              }}
            />
          )}
        </div>
        <div className={styles.description}>
          <p>
            {hasRewardsWallets ? (
              <FormattedHTMLMessage
                {...messages.descriptionWithNoFunds}
                values={{
                  activeWalletName: activeWallet.name,
                }}
              />
            ) : (
              <FormattedHTMLMessage
                {...messages.descriptionWithFunds}
                values={{
                  activeWalletName: activeWallet.name,
                }}
              />
            )}
          </p>
        </div>
        <div className={styles.actions}>
          <ButtonLink
            className={styles.actionLearnMore}
            onClick={this.onLearnMore}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.actionLearnMore)}
            linkProps={{
              className: styles.externalLink,
              hasIconBefore: false,
              hasIconAfter: true,
            }}
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
