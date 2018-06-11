// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';

import styles from './AdaRedemptionNoWallets.scss';
import cross from '../../../assets/images/close-cross.inline.svg'

const messages = defineMessages({
  warning: {
    id: 'wallet.redeem.noWallets.warning',
    defaultMessage: '!!!Redeeming Ada is not possible because you don\'t have any wallet.',
    description: '"No wallets" warning on the Ada Redemption Page.'
  },
  createWalletLink: {
    id: 'wallet.redeem.noWallets.createWalletLink',
    defaultMessage: '!!!Click here to create one',
    description: 'URL for the "FAQ on Daedalus website" link in the FAQ section on the support settings page',
  },
});

export default class AdaRedemptionNoWallets extends Component {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;

    return (
      <div className={styles.component}>
        <SVGInline svg={cross} className={styles.icon} />
        <div>
          <p>{intl.formatMessage(messages.warning)}</p>
          <p>
            <button
              onClick={this.props.onGoToCreateWalletClick}
            >
              {intl.formatMessage(messages.createWalletLink)}
            </button>
          </p>
        </div>
      </div>
    );
  }

}
