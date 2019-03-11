// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import Layout from '../MainLayout';
import WalletImporter from '../../components/wallet/WalletImporter';
import type { InjectedProps } from '../../types/injectedPropsType';
import type { ExtractedWallet } from '../../../../common/types/wallet-importer.types';

type Props = InjectedProps;

@inject('actions', 'stores') @observer
export default class WalletImporterPage extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  constructor(props: Props) {
    super(props);
    props.stores.sidebar._resetActivateSidebarCategory();
  }

  handleSecretKeyFileSelection = (keyFile: File) => {
    this.props.actions.walletImporter.extractWallets.trigger({ keyFile });
  };

  handleMatchPasswords = (passwords: Array<string>) => {
    this.props.actions.walletImporter.matchPasswords.trigger({ passwords });
  };

  handleDownloadKeyFile = (wallet: ExtractedWallet, filePath: string) => {
    this.props.actions.walletImporter.downloadKeyFile.trigger({ wallet, filePath });
  };

  handleImportKeyFile = (wallet: ExtractedWallet) => {
    this.props.actions.walletImporter.importKeyFile.trigger({ wallet });
  };

  handleOpenWallet = (walletId: string) => {
    this.props.stores.wallets.goToWalletRoute(walletId);
  };

  render() {
    const { stores } = this.props;
    const {
      keyFile,
      isMatchingPasswords,
      isExtractingWallets,
      hasExtractedWallets,
      extractedWallets,
    } = stores.walletImporter;

    return (
      <Layout>
        <WalletImporter
          keyFile={keyFile}
          isMatchingPasswords={isMatchingPasswords}
          isExtractingWallets={isExtractingWallets}
          hasExtractedWallets={hasExtractedWallets}
          extractedWallets={extractedWallets}
          onSecretKeyFileSelect={this.handleSecretKeyFileSelection}
          onDownloadKeyFile={this.handleDownloadKeyFile}
          onImportKeyFile={this.handleImportKeyFile}
          onMatchPasswords={this.handleMatchPasswords}
          onOpenWallet={this.handleOpenWallet}
        />
      </Layout>
    );
  }

}
