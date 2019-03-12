// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { get } from 'lodash';
import Layout from '../MainLayout';
import WalletImporter from '../../components/wallet/WalletImporter';
import RestoreNotification from '../../components/notifications/RestoreNotification';
import NotificationMessage from '../../components/widgets/NotificationMessage';
import successIcon from '../../assets/images/success-small.inline.svg';
import { WALLET_IMPORTER_PASSWORD_ANALYS_IS_DONE } from '../../config/timingConfig';
import type { InjectedProps } from '../../types/injectedPropsType';
import type { ExtractedWallet } from '../../../../common/types/wallet-importer.types';

const messages = defineMessages({
  passwordAnalysisDone: {
    id: 'wallet.importer.passwordAnalysisDone',
    defaultMessage: '!!!The password analysis is done',
    description: '"The password analysis is done" notification on the wallet importer page.',
  },
});

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
    this.registerPasswordAnalysisDoneNotification();
  }

  componentWillUnmount() {
    this.props.actions.walletImporter.matchPasswordsEnd.remove(this.openNotification);
    this.closeNotification();
  }

  registerPasswordAnalysisDoneNotification = () => {
    this.props.actions.walletImporter.matchPasswordsEnd.listen(this.openNotification);
  };

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

  openNotification = () => {
    const { notifications } = this.props.actions;
    const { id, duration } = this.passwordAnalysisDoneNotification;
    notifications.open.trigger({ id, duration });
  };

  closeNotification = () => {
    const { id } = this.passwordAnalysisDoneNotification;
    this.props.actions.notifications.closeActiveNotification.trigger({ id });
  }

  get passwordAnalysisDoneNotification() {
    const { intl } = this.context;
    return {
      id: 'wallet-importer-passwordAnalysisDone',
      duration: WALLET_IMPORTER_PASSWORD_ANALYS_IS_DONE,
      message: intl.formatMessage(messages.passwordAnalysisDone),
    };
  }

  render() {
    const { stores } = this.props;
    const { wallets, walletImporter, profile, uiNotifications } = stores;
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

    const { id, message } = this.passwordAnalysisDoneNotification;

    return (
      <Layout>
        {isRestoreActive && restoringWallet ? (
          <RestoreNotification
            currentLocale={currentLocale}
            restoreProgress={restoreProgress}
            restoreETA={restoreETA}
          />
        ) : null}

        <NotificationMessage
          icon={successIcon}
          show={uiNotifications.isOpen(id)}
          onClose={this.closeNotification}
          clickToClose
          hasCloseButton
        >
          {message}
        </NotificationMessage>

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
