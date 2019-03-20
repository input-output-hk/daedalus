// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletUtxoSettings.scss';

export const messages = defineMessages({});

type Props = {
  allStakes: number,
  histogram: Object,
  boundType: string,
};

@observer
export default class WalletUtxoSettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { allStakes, histogram, boundType } = this.props;
    return (
      <div className={styles.component}>
        <pre>
          {JSON.stringify({ allStakes, histogram, boundType }, null, 2)}
        </pre>
      </div>
    );
  }
}
