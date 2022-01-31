import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import type { InjectedProps } from '../../types/injectedPropsType';
import Notification from '../../components/notifications/Notification';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/success-sm... Remove this comment to see the full error message
import successIcon from '../../assets/images/success-small.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/spinner-da... Remove this comment to see the full error message
import spinnerIcon from '../../assets/images/spinner-dark.inline.svg';
import type {
  NotificationConfig,
  NotificationId,
} from '../../types/notificationTypes';
import type { NotificationDataProps } from '../../components/notifications/Notification';

const ICONS = {
  successIcon,
  spinnerIcon,
};
const messages = defineMessages({
  downloadLogsProgress: {
    id: 'notification.downloadLogsProgress',
    defaultMessage: '!!!Preparing logs for download',
    description:
      'Notification for download logs in progress in the Loading and Settings pages.',
  },
  downloadLogsSuccess: {
    id: 'notification.downloadLogsSuccess',
    defaultMessage: '!!!Logs successfully downloaded',
    description:
      'Notification for download logs in the Loading and Settings pages.',
  },
  downloadRewardsCSVSuccess: {
    id: 'notification.downloadRewardsCSVSuccess',
    defaultMessage: '!!!CSV file successfully downloaded',
    description: 'Notification for download Rewards CSV file.',
  },
  downloadTransactionsCSVSuccess: {
    id: 'notification.downloadTransactionsCSVSuccess',
    defaultMessage: '!!!CSV file successfully downloaded',
    description: 'Notification for download Transactions CSV file.',
  },
  copyWalletPublicKey: {
    id: 'notification.copyWalletPublicKey',
    defaultMessage:
      '!!!Public key: <strong>{publicKey}</strong> copied to clipboard',
    description:
      'Notification for the wallet public key copy success in the Wallet Settings page.',
  },
  copyICOPublicKey: {
    id: 'notification.copyICOPublicKey',
    defaultMessage:
      '!!!ICO Public key: <strong>{publicKey}</strong> copied to clipboard',
    description:
      'Notification for the ICO public key copy success in the Wallet Settings page.',
  },
  copyAddress: {
    id: 'notification.copyAddress',
    defaultMessage:
      '!!!Address: <strong>{address}</strong> copied to clipboard',
    description:
      'Notification for the wallet address copy success in the Wallet Receive page.',
  },
  copyAssetParam: {
    id: 'notification.copyAssetParam',
    defaultMessage:
      '!!!{param}: <strong>{shortValue}</strong> copied to clipboard',
    description:
      'Notification for the wallet assetItem copy success in the Wallet Receive page.',
  },
  downloadAddressPDFSuccess: {
    id: 'notification.downloadAddressPDFSuccess',
    defaultMessage:
      '!!!Address: <strong>{walletAddress}</strong> PDF successfully downloaded',
    description:
      'Notification for the wallet address PDF download success in the Wallet Receive page.',
  },
  downloadVotingPDFSuccess: {
    id: 'notification.downloadVotingPDFSuccess',
    defaultMessage: '!!!PDF successfully downloaded',
    description:
      'Notification for the wallet voting PDF download success in the Voting Registration dialog.',
  },
  downloadQRCodeImageSuccess: {
    id: 'notification.downloadQRCodeImageSuccess',
    defaultMessage:
      '!!!Address: <strong>{walletAddress}</strong> QR code image successfully downloaded',
    description:
      'Notification for the wallet address PDF download success in the Wallet Receive page.',
  },
  copyStateDirectoryPath: {
    id: 'notification.copyStateDirectoryPath',
    defaultMessage: '!!!Daedalus state directory copied to clipboard',
    description:
      'Notification for the state directory copy success in the Diagnostics page.',
  },
});

@inject('stores', 'actions')
@observer
class NotificationsContainer extends Component<InjectedProps> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  constructor(props: InjectedProps) {
    super(props);
    this.registerNotifications();
  }

  notificationsConfig: Array<NotificationConfig> = [
    {
      id: 'downloadLogsProgress',
      actionToListenAndOpen: this.props.actions.profile.downloadLogs,
      actionToListenAndClose: this.props.actions.profile.downloadLogsSuccess,
    },
    {
      id: 'downloadLogsSuccess',
      actionToListenAndOpen: this.props.actions.profile.downloadLogsSuccess,
      actionToListenAndClose: this.props.actions.profile.downloadLogs,
    },
    {
      id: 'downloadRewardsCSVSuccess',
      actionToListenAndOpen: this.props.actions.staking.requestCSVFileSuccess,
      actionToListenAndClose: this.props.actions.staking.requestCSVFile,
    },
    {
      id: 'downloadTransactionsCSVSuccess',
      actionToListenAndOpen: this.props.actions.transactions
        .requestCSVFileSuccess,
      actionToListenAndClose: this.props.actions.transactions.requestCSVFile,
    },
    {
      id: 'copyWalletPublicKey',
      actionToListenAndOpen: this.props.actions.wallets.copyWalletPublicKey,
    },
    {
      id: 'copyICOPublicKey',
      actionToListenAndOpen: this.props.actions.wallets.copyICOPublicKey,
    },
    {
      id: 'copyAddress',
      actionToListenAndOpen: this.props.actions.wallets.copyAddress,
    },
    {
      id: 'downloadAddressPDFSuccess',
      actionToListenAndOpen: this.props.actions.wallets
        .generateAddressPDFSuccess,
      actionToListenAndClose: this.props.actions.wallets.generateAddressPDF,
    },
    {
      id: 'downloadVotingPDFSuccess',
      actionToListenAndOpen: this.props.actions.voting.saveAsPDFSuccess,
      actionToListenAndClose: this.props.actions.voting.saveAsPDF,
    },
    {
      id: 'downloadQRCodeImageSuccess',
      actionToListenAndOpen: this.props.actions.wallets.saveQRCodeImageSuccess,
      actionToListenAndClose: this.props.actions.wallets.saveQRCodeImage,
    },
    {
      id: 'copyStateDirectoryPath',
      actionToListenAndOpen: this.props.actions.networkStatus
        .copyStateDirectoryPath,
    },
    {
      id: 'copyAssetParam',
      actionToListenAndOpen: this.props.actions.assets
        .copyAssetParamNotification,
    },
  ];
  // @ts-ignore ts-migrate(2740) FIXME: Type '{ downloadLogsProgress: { icon: string; hasE... Remove this comment to see the full error message
  notificationsData: Record<NotificationId, NotificationDataProps> = {
    downloadLogsProgress: {
      icon: 'spinner',
      hasEllipsis: true,
      clickToClose: false,
    },
  };
  registerNotifications = () => {
    const { registerNotification } = this.props.actions.notifications;
    this.notificationsConfig.forEach((notificationConfig: NotificationConfig) =>
      registerNotification.trigger(notificationConfig)
    );
  };
  getIcon = (icon = 'success') => (icon ? ICONS[`${icon}Icon`] : icon);
  getLabel = (
    id: string,
    labelValues?: Record<string, any> | null | undefined
  ) => {
    const values = typeof labelValues === 'object' ? labelValues : {};
    return <FormattedHTMLMessage {...messages[id]} values={values} />;
  };

  render() {
    const { stores, actions } = this.props;
    const { closeNotification } = actions.notifications;
    const { activeNotifications } = stores.uiNotifications;
    return (
      <div>
        {this.notificationsConfig.map(({ id }: NotificationConfig) => {
          const isVisible = id in activeNotifications;
          const data = this.notificationsData[id] || {};
          // @ts-ignore ts-migrate(2525) FIXME: Initializer provides no value for this binding ele... Remove this comment to see the full error message
          const { labelValues, index } = isVisible
            ? activeNotifications[id]
            : {};
          const { icon } = data || {};
          const hasSpinner = icon === 'spinner';
          return (
            <Notification
              key={id}
              {...data}
              onClose={() =>
                closeNotification.trigger({
                  id,
                })
              }
              icon={this.getIcon(icon)}
              isVisible={isVisible}
              hasSpinner={hasSpinner}
              index={index || 0}
            >
              {isVisible ? this.getLabel(id, labelValues) : null}
            </Notification>
          );
        })}
      </div>
    );
  }
}

export default NotificationsContainer;
