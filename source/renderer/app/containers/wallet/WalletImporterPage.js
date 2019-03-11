// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { get } from 'lodash';
import Layout from '../MainLayout';
import WalletImporter from '../../components/wallet/WalletImporter';
import RestoreNotification from '../../components/notifications/RestoreNotification';
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
    const { wallets, walletImporter, profile } = stores;
    const {
      keyFile,
      isMatchingPasswords,
      isExtractingWallets,
      hasExtractedWallets,
      extractedWallets,
    } = walletImporter;
    const {
      isRestoreActive,
      restoringWalletId,
      getWalletById,
    } = wallets;
    const { currentLocale } = profile;

    const restoringWallet = restoringWalletId ? getWalletById(restoringWalletId) : null;
    const restoreProgress = get(restoringWallet, 'syncState.data.percentage.quantity', 0);
    const restoreETA = get(restoringWallet, 'syncState.data.estimatedCompletionTime.quantity', 0);

    return (
      <Layout>
        {isRestoreActive && restoringWallet ? (
          <RestoreNotification
            currentLocale={currentLocale}
            restoreProgress={restoreProgress}
            restoreETA={restoreETA}
          />
        ) : null}

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
          isRestoreActive={isRestoreActive}
          restoringWalletId={restoringWalletId}
        />
      </Layout>
    );
  }

}
