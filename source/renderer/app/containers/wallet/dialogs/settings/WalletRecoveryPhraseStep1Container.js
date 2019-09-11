// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import WalletRecoveryPhraseStep1Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep1Dialog';

@observer
export default class WalletRecoveryPhraseStep1Container extends Component {
  handleContinue = () => {};

  render() {
    return <div>KJHFKDFHKHDK</div>;
    return <WalletRecoveryPhraseStep1Dialog onContinue={this.handleContinue} />;
  }
}
