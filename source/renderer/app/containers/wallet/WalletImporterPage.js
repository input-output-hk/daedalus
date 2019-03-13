// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { get } from 'lodash';
import Layout from '../MainLayout';
import WalletImporter from '../../components/wallet/WalletImporter';
import RestoreNotification from '../../components/notifications/RestoreNotification';
import GenericNotification from '../../components/notifications/GenericNotification';
import { WALLET_IMPORTER_PASSWORD_MATCHING_IS_DONE } from '../../config/timingConfig';
import type { InjectedProps } from '../../types/injectedPropsType';
import type { ExtractedWallet } from '../../../../common/types/wallet-importer.types';

const messages = defineMessages({
  passwordMatchingIsDone: {
    id: 'wallet.importer.passwordMatchingIsDone',
    defaultMessage: '!!!The password matching is done',
    description: '"The password matching is done" notification on the wallet importer page.',
  },
});

const WALLET_IMPORTER_PASSWORD_MATCHING_IS_DONE_ID = 'wallet-importer-password-matching-is-done';

type Props = InjectedProps;

@inject('actions', 'stores') @observer
export default class WalletImporterPage extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

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

  handleImportKeyFile = (wallet: ExtractedWallet) => {
    this.props.actions.walletImporter.importKeyFile.trigger({ wallet });
  };

  handleOpenWallet = (walletId: string) => {
    this.props.stores.wallets.goToWalletRoute(walletId);
  };

  render() {
    const { stores, actions } = this.props;
    const { intl } = this.context;
    const { wallets, walletImporter, profile, uiNotifications } = stores;
    const { notifications, walletImporter: walletImporterActions } = actions;

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

        <GenericNotification
          id={WALLET_IMPORTER_PASSWORD_MATCHING_IS_DONE_ID}
          message={intl.formatMessage(messages.passwordMatchingIsDone)}
          duration={WALLET_IMPORTER_PASSWORD_MATCHING_IS_DONE}
          show={uiNotifications.isOpen(WALLET_IMPORTER_PASSWORD_MATCHING_IS_DONE_ID)}
          actionToListen={walletImporterActions.matchPasswordsEnd}
          openNotification={notifications.open}
          closeNotification={notifications.closeActiveNotification}
        />

        <WalletImporter
          keyFile={keyFile}
          isMatchingPasswords={isMatchingPasswords}
          isExtractingWallets={isExtractingWallets}
          hasExtractedWallets={hasExtractedWallets}
          extractedWallets={extractedWallets}
          onSecretKeyFileSelect={this.handleSecretKeyFileSelection}
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
