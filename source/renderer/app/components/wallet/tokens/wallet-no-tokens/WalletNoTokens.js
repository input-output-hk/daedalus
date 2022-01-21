// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import BorderedBox from '../../../widgets/BorderedBox';
import styles from './WalletNoTokens.scss';
import { ExternalLinkButton } from '../../../widgets/ExternalLinkButton';

const messages = defineMessages({
  tokensTitle: {
    id: 'wallet.summary.assets.tokensTitle',
    defaultMessage: '!!!Tokens',
    description: 'Number of tokens title on Wallet summary assets page',
  },
  learnMoreTextTop: {
    id: 'wallet.summary.noTokens.learnMore.textTop',
    defaultMessage: '!!!Want to find out more about native tokens?',
    description: '"Learn more" text in the Wallets Summary No Tokens component',
  },
  learnMoreTextBottom: {
    id: 'wallet.summary.noTokens.learnMore.textBottom',
    defaultMessage: '!!!Start by visiting the IOHK blog for a useful primer.',
    description: '"Learn more" text in the Wallets Summary No Tokens component',
  },
  learnMoreLinkLabel: {
    id: 'wallet.summary.noTokens.learnMore.linkLabel',
    defaultMessage: '!!!Learn more',
    description:
      '"Learn more" label or button in the Wallets Summary No Tokens component',
  },
  learnMoreLinkUrl: {
    id: 'wallet.summary.noTokens.learnMore.linkUrl',
    defaultMessage:
      '!!!https://iohk.io/en/blog/posts/2021/02/04/native-tokens-to-bring-new-utility-to-life-on-cardano/',
    description:
      '"Learn more" link URL in the Wallets Summary No Tokens component',
  },
});

type Props = {
  isLoadingAssets: boolean,
  onExternalLinkClick: Function,
  numberOfAssets: number,
};

@observer
export default class WalletSummaryNoTokens extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { isLoadingAssets, onExternalLinkClick, numberOfAssets } = this.props;
    const { intl } = this.context;

    return (
      <>
        {!isLoadingAssets && (
          <div className={styles.numberOfAssets}>
            {intl.formatMessage(messages.tokensTitle)} ({numberOfAssets})
          </div>
        )}
        <div className={styles.component}>
          <BorderedBox>
            <div className={styles.noTokensContainer}>
              <div className={styles.noTokensLeftContainer}>
                <p>{intl.formatMessage(messages.learnMoreTextTop)}</p>
                <p>{intl.formatMessage(messages.learnMoreTextBottom)}</p>
              </div>
              <ExternalLinkButton
                label={intl.formatMessage(messages.learnMoreLinkLabel)}
                onClick={() =>
                  onExternalLinkClick(
                    intl.formatMessage(messages.learnMoreLinkUrl)
                  )
                }
              />
            </div>
          </BorderedBox>
        </div>
      </>
    );
  }
}
