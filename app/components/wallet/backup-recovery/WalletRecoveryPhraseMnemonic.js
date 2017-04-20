// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletRecoveryPhraseMnemonic.scss';

@observer
export default class WalletRecoveryPhraseMnemonic extends Component {

  props: {
    phrase: string,
  };

  render() {
    const { phrase } = this.props;
    return (
      <div className={styles.component}>{phrase}</div>
    );
  }

}
