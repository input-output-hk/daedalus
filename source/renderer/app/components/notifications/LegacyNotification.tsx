import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import ButtonLink from '../widgets/ButtonLink';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './LegacyNotification.scss' or ... Remove this comment to see the full error message
import styles from './LegacyNotification.scss';

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
      '!!!"{activeWalletName}"" is a Byron legacy wallet that does not support Shelley delegation features. To earn ada from delegating your stake, please move all funds from this wallet to a new wallet that is Shelley-compatible.',
    description: 'Legacy notification description.',
  },
  moveFundsDescriptionLine2: {
    id: 'wallet.byron.notification.moveFundsDescription.line2',
    defaultMessage:
      '!!!You can create a {moveFundsLink} or move funds to one of your existing wallets.',
    description: 'Legacy notification description.',
  },
  moveFundsLinkLabel: {
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
    defaultMessage: '!!!Move ada to an existing wallet',
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
  activeWalletName: string;
  onLearnMore: (...args: Array<any>) => any;
  onTransferFunds: (...args: Array<any>) => any;
  hasRewardsWallets?: boolean;
  onWalletAdd?: (...args: Array<any>) => any;
};

@observer
class LegacyNotification extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  onLearnMore = () => {
    const { intl } = this.context;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);
    this.props.onLearnMore(learnMoreLinkUrl);
  };
  getValue = (
    messageHasRewardsWallets: string,
    messageNoRewardsWallets: string,
    _values?: Record<string, any>
  ) => {
    const { hasRewardsWallets, activeWalletName } = this.props;
    const message = hasRewardsWallets
      ? messageHasRewardsWallets
      : messageNoRewardsWallets;
    const values = {
      activeWalletName,
      ..._values,
    };
    return <FormattedMessage {...message} values={values} />;
  };

  render() {
    const { intl } = this.context;
    const { onTransferFunds, hasRewardsWallets, onWalletAdd } = this.props;
    const { getValue, onLearnMore } = this;
    const showLearnMoreButton = false;
    const moveFundsLink = (
      <Link
        className={styles.descriptionLink}
        onClick={onWalletAdd}
        label={intl.formatMessage(messages.moveFundsLinkLabel)}
        skin={LinkSkin}
        hasIconAfter={false}
      />
    );
    const title = getValue(messages.moveFundsTitle, messages.addWalletTitle);
    const description1 = getValue(
      messages.moveFundsDescriptionLine1,
      messages.addWalletDescriptionLine1
    );
    const description2 = getValue(
      messages.moveFundsDescriptionLine2,
      messages.addWalletDescriptionLine2,
      {
        moveFundsLink,
      }
    );
    const buttonLabel = getValue(messages.actionMove, messages.addWallet);
    const buttonAction = hasRewardsWallets ? onTransferFunds : onWalletAdd;
    return (
      <div className={styles.component}>
        <div className={styles.title}>{title}</div>
        <div className={styles.description}>
          <p>{description1}</p>
          <p>{description2}</p>
        </div>
        <div className={styles.actions}>
          {showLearnMoreButton && (
            <ButtonLink
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              className={styles.actionLearnMore}
              onClick={onLearnMore}
              skin={ButtonSkin}
              label={intl.formatMessage(messages.actionLearnMore)}
              linkProps={{
                className: styles.externalLink,
                hasIconBefore: false,
                hasIconAfter: true,
              }}
            />
          )}
          <Button
            className={styles.actionMove}
            label={buttonLabel}
            onClick={buttonAction}
            skin={ButtonSkin}
          />
        </div>
      </div>
    );
  }
}

export default LegacyNotification;
