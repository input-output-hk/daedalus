// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import WalletHome from '../../components/wallet/WalletHome';

@observer(['store'])
export default class WalletHomePage extends Component {

  static propTypes = {
    store: PropTypes.shape({
      selectedWallet: MobxPropTypes.observableObject.isRequired
    })
  };

  render() {
    const { selectedWallet } = this.props.store;
    return (
      <WalletHome transactions={selectedWallet.transactions} />
    );
  }

}
