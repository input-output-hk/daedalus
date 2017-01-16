// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletRecoveryPhraseMnemonic.scss';

@observer
export default class WalletRecoveryPhraseShowDialog extends Component {

  static propTypes = {
    phrase: PropTypes.string.isRequired
  };

  render() {
    const { phrase } = this.props;
    return (
      <div className={styles.component}>{phrase}</div>
    );
  }

}
