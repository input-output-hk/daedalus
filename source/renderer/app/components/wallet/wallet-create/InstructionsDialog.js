// @flow
import React, { Component } from 'react';
import WalletCreateHeader from './WalletCreateHeader';
import WalletCreateFooter from './WalletCreateFooter';

type Props = {
  onAcceptTermsAndConditions: Function,
};

export default class InstructionsDialog extends Component<Props> {
  render() {
    const { onAcceptTermsAndConditions } = this.props;

    return (
      <div>
        <WalletCreateHeader stepNumber={0} />
        <button onClick={onAcceptTermsAndConditions}>
          onAcceptTermsAndConditions
        </button>
        <WalletCreateFooter
          actions={[
            {
              primary: true,
              label: 'Skip video and continue',
            },
          ]}
        />
      </div>
    );
  }
}
