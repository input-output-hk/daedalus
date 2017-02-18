// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletRecoveryInstructions.scss';

@observer
export default class WalletRecoveryInstructions extends Component {

  static propTypes = {
    instructionsText: PropTypes.string.isRequired
  };

  render() {
    const { instructionsText } = this.props;
    return (
      <div className={styles.component}>{instructionsText}</div>
    );
  }

}
