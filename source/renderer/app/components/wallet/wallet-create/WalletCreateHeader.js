// @flow
import React, { Component } from 'react';
import { CREATE_WALLET_STEPS } from '../../../containers/wallet/dialogs/WalletCreateDialogContainer';

type Props = {
  stepNumber: number,
};

class WalletCreateHeader extends Component<Props> {
  render() {
    const { stepNumber } = this.props;

    return (
      <div>
        <h1>CREATE PERSONAL WALLET</h1>
        <p>Step: {CREATE_WALLET_STEPS[stepNumber]}</p>
      </div>
    );
  }
}

export default WalletCreateHeader;
