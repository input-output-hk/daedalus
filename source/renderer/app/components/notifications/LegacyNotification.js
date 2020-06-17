// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  defineMessages,
  intlShape,
  FormattedHTMLMessage,
  FormattedMessage,
} from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import ButtonLink from '../widgets/ButtonLink';
import styles from './LegacyNotification.scss';
import Wallet from '../../domains/Wallet';

const messages = defineMessages({
  moveFundsTitle: {
    id: 'wallet.byron.notification.moveFundsTitle',
    defaultMessage: '!!!Move funds from {activeWalletName}',
    description:
      'Title "Move funds from the legacy wallet" on the legacy notification.',
  },
  addWalletTitle: {
    id: 'wallet.byron.notification.addWalletTitle',
    defaultMessage: '!!!Create a Shelley wallet',
    description: 'Title "Create a Shelley wallet" on the legacy notification.',
  },
  moveFundsDescriptionLine1: {
    id: 'wallet.byron.notification.moveFundsDescription.line1',
    defaultMessage:
      '!!!"{transferWalletName}"" is a Byron legacy wallet that does not support Shelley delegation features. To earn ada from delegating your stake, please move all funds from this wallet to a new wallet that is Shelley-compatible.',
    description: 'Legacy notification description.',
  },
  moveFundsDescriptionLine2: {
    id: 'wallet.byron.notification.moveFundsDescription.line2',
    defaultMessage:
      '!!!You can create a brand new wallet or move funds to one of your existing wallets.',
    description: 'Legacy notification description.',
  },
  moveFundsDescriptionLine2LinkLabel: {
    id: 'wallet.byron.notification.moveFundsDescription.line2.link.label',
    defaultMessage: '!!!brand new wallet',
    description: 'Legacy notification link label.',
  },
  descriptionWithFunds: {
    id: 'wallet.legacy.notification.descriptionWithFunds',
    defaultMessage:
      '!!!"{transferWalletName}"" is a legacy wallet. It does not support Shelley delegation features. To earn ada from delegating your stake, please move all funds from this wallet to a new, Shelley-compatible wallet. You can create a brand new wallet or move funds to one of the existing wallets.',
    description: 'Legacy notification description WithFunds.',
  },
  addWalletDescriptionLine1: {
    id: 'wallet.byron.notification.addWalletDescription.line1',
    defaultMessage:
      '!!!"{activeWalletName}"" is a Byron legacy wallet that does not support Shelley delegation features. To earn ada from delegating your stake, please move all funds from this wallet to a new wallet that is Shelley-compatible.',
    description: 'Legacy notification description.',
  },
  addWalletDescriptionLine2: {
    id: 'wallet.byron.notification.addWalletDescription.line2',
    defaultMessage:
      '!!!Since all of your wallets are Byron legacy wallets you will first need to create a new Shelley wallet.',
    description: 'Legacy notification description.',
  },
  actionLearnMore: {
    id: 'wallet.byron.notification.actionLearnMore',
    defaultMessage: '!!!Learn more',
    description: 'Learn more action of legacy notification.',
  },
  actionMove: {
    id: 'wallet.byron.notification.actionMove',
    defaultMessage: '!!!Move ada from this wallet',
    description: 'Move Move ada from this wallet of legacy notification.',
  },
  addWallet: {
    id: 'wallet.byron.notification.addWallet',
    defaultMessage: '!!!Create a new wallet',
    description: 'Create a new wallet action of legacy notification.',
  },
  learnMoreLinkUrl: {
    id: 'wallet.byron.notification.learnMore.url',
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

    const moveFundsDescriptionLine2Link = (
      <Link
        className={styles.descriptionLink}
        onClick={onWalletAdd}
        label={intl.formatMessage(messages.moveFundsDescriptionLine2LinkLabel)}
        skin={LinkSkin}
      />
    );

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
                {...messages.moveFundsDescriptionLine1}
                values={{
                  activeWalletName: activeWallet.name,
                }}
              />
            ) : (
              <FormattedHTMLMessage
                {...messages.addWalletDescriptionLine1}
                values={{
                  activeWalletName: activeWallet.name,
                }}
              />
            )}
          </p>
          <p>
            {hasRewardsWallets ? (
              <FormattedMessage
                {...messages.moveFundsDescriptionLine2}
                values={{
                  link: moveFundsDescriptionLine2Link,
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
