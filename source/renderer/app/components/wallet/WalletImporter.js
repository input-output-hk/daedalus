// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import BorderedBox from '../widgets/BorderedBox';
import styles from './WalletImporter.scss';

export const messages = defineMessages({
  headline: {
    id: 'wallet.importer.headline',
    defaultMessage: '!!!Wallet importer',
    description: 'Headline of the wallet importer page.',
  },
  instructions: {
    id: 'wallet.importer.instructions',
    defaultMessage: '!!!In order to import wallets select a folder which contains wallet key files.',
    description: 'Detailed instructions for importing wallets.',
  },
});

type Props = {};

@observer
export default class WalletImporter extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;

    return (
      <div className={styles.component}>

        <BorderedBox>

          <h1 className={styles.headline}>{intl.formatMessage(messages.headline)}</h1>

          <div className={styles.instructions}>
            <p>{intl.formatMessage(messages.instructions)}</p>
          </div>

        </BorderedBox>

      </div>
    );
  }

}
