// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import Button from 'react-polymorph/lib/components/Button';
import SimpleButtonSkin from 'react-polymorph/lib/skins/simple/raw/ButtonSkin';

import styles from './AdaRedemptionNoWallets.scss';
import icon from '../../../assets/images/attention-big-thin.inline.svg';

const messages = defineMessages({
  headLine: {
    id: 'wallet.redeem.noWallets.headLine',
    defaultMessage: '!!!Ada redemption is not available because you don\'t have any wallets.',
    description: '"No wallets" headLine on the Ada Redemption Page.'
  },
  instructions: {
    id: 'wallet.redeem.noWallets.instructions',
    defaultMessage: '!!!Create a new wallet (or restore an existing one), come back here and choose it for Ada redemption.',
    description: '"No wallets" instructions on the Ada Redemption Page.'
  },
  createWalletLink: {
    id: 'wallet.redeem.noWallets.createWalletLink',
    defaultMessage: '!!!Create your first wallet',
    description: 'URL for the "FAQ on Daedalus website" link in the FAQ section on the support settings page',
  },
});

type Props = {
  onGoToCreateWalletClick: Function
};

export default class AdaRedemptionNoWallets extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;

    return (
      <div className={styles.component}>
        <SVGInline svg={icon} className={styles.icon} />
        <h1>{intl.formatMessage(messages.headLine)}</h1>
        <p>{intl.formatMessage(messages.instructions)}</p>
        <p>
          <Button
            className="primary"
            onClick={this.props.onGoToCreateWalletClick}
            label={intl.formatMessage(messages.createWalletLink)}
            skin={<SimpleButtonSkin />}
          />
        </p>
      </div>
    );
  }

}
