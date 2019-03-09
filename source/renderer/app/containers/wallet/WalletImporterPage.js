// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import Layout from '../MainLayout';
import WalletImporter from '../../components/wallet/WalletImporter';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('actions', 'stores') @observer
export default class WalletImporterPage extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  constructor(props: Props) {
    super(props);
    props.stores.sidebar._resetActivateSidebarCategory();
  }

  handleSecretKeyFileSelection = (secretKeyFilePath: string) => {
    this.props.actions.walletImporter.extractWallets.trigger({ secretKeyFilePath });
  };

  handleMatchPasswords = (passwords: Array<string>) => {
    this.props.actions.walletImporter.matchPasswords.trigger({ passwords });
  };

  render() {
    const { stores } = this.props;
    const {
      isMatchingPasswords,
      isExtractingWallets,
      hasExtractedWallets,
      extractedWallets,
    } = stores.walletImporter;

    return (
      <Layout>
        <WalletImporter
          isMatchingPasswords={isMatchingPasswords}
          isExtractingWallets={isExtractingWallets}
          hasExtractedWallets={hasExtractedWallets}
          extractedWallets={extractedWallets}
          onSecretKeyFileSelect={this.handleSecretKeyFileSelection}
          onMatchPasswords={this.handleMatchPasswords}
        />
      </Layout>
    );
  }

}
