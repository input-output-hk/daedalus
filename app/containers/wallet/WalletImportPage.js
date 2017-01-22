// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import WalletImportDialog from '../../components/wallet/WalletImportDialog';

@inject('stores', 'actions') @observer
export default class WalletImportPage extends Component {

  static propTypes = {
  };

  render() {
    return (
    <WalletImportDialog

    />
    );
  }

}
