// @flow
import React, { Component, Fragment } from 'react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import { observer, inject } from 'mobx-react';
import { ellipsis } from '../../utils/strings';
import WalletReceive from '../../components/wallet/receive/WalletReceive';
import WalletReceiveDialog from '../../components/wallet/receive/WalletReceiveDialog';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import NotificationMessage from '../../components/widgets/NotificationMessage';
import successIcon from '../../assets/images/success-small.inline.svg';
import type { InjectedProps } from '../../types/injectedPropsType';
import WalletAddress from '../../domains/WalletAddress';
import Wallet from '../../domains/Wallet';
import { ADDRESS_COPY_NOTIFICATION_DURATION } from '../../config/timingConfig';
import { ADDRESS_COPY_NOTIFICATION_ELLIPSIS } from '../../config/formattingConfig';
import { generateFileNameWithTimestamp } from '../../../../common/utils/files';

export const messages = defineMessages({
  message: {
    id: 'wallet.receive.page.addressCopyNotificationMessage',
    defaultMessage: '!!!You have successfully copied wallet address',
    description: 'Message for the wallet address copy success notification.',
  },
});

type Props = InjectedProps;

type State = {
  copiedAddress: string,
  addressToShare?: ?WalletAddress,
  activeWallet: ?Wallet,
};

@inject('stores', 'actions')
@observer
export default class WalletReceivePage extends Component<Props, State> {
  static defaultProps = { actions: null, stores: null };

  state = {
    copiedAddress: '',
    addressToShare: null,
    activeWallet: this.props.stores.wallets.active,
  };

  componentWillUnmount() {
    this.closeNotification();
  }

  get notification() {
    const { copiedAddress, activeWallet } = this.state;

    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletReceivePage.');

    return {
      id: `${activeWallet.id}-copyNotification`,
      duration: ADDRESS_COPY_NOTIFICATION_DURATION,
      message: (
        <FormattedHTMLMessage
          {...messages.message}
          values={{
            walletAddress: ellipsis(
              copiedAddress,
              ADDRESS_COPY_NOTIFICATION_ELLIPSIS,
              ADDRESS_COPY_NOTIFICATION_ELLIPSIS
            ),
          }}
        />
      ),
    };
  }

  closeNotification = () => {
    const { id } = this.notification;
    if (id) {
      this.props.actions.notifications.closeActiveNotification.trigger({ id });
    }
  };

  handleIsAddressValid = (index: number) => index < 3 || index > 7;

  handleCopyAddress = (address: string) => {
    this.setState({ copiedAddress: address });
    this.props.actions.notifications.open.trigger({
      id: this.notification.id,
      duration: this.notification.duration,
    });
  };

  handleShareAddress = (addressToShare: WalletAddress) => {
    this.setState({
      addressToShare,
    });
    const dialog = WalletReceiveDialog;
    this.props.actions.dialogs.open.trigger({ dialog });
  };

  handleCloseShareAddress = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  handleDownloadPDF = (note: string) => {
    const { addressToShare } = this.state;

    const name = generateFileNameWithTimestamp({
      prefix: 'daedalus-cardano-ada-address',
      extension: '',
      isUTC: false,
    });

    // TODO: refactor this direct access to the dialog api
    const filePath = global.dialog.showSaveDialog({
      defaultPath: `${name}.pdf`,
      filters: [
        {
          name,
          extensions: ['pdf'],
        },
      ],
    });

    // if cancel button is clicked or path is empty
    if (!filePath || !addressToShare) return;

    const { id: address } = addressToShare;

    this.props.actions.wallets.generateAddressPDF.trigger({
      address,
      note,
      filePath,
    });
  };

  render() {
    const { actions, stores } = this.props;
    const { uiNotifications, uiDialogs, addresses, networkStatus } = stores;
    const { isIncentivizedTestnet } = networkStatus.environment;
    const { addressToShare, activeWallet } = this.state;
    const { toggleSubMenus } = actions.sidebar;

    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletReceivePage.');

    const walletAddresses = addresses.all.slice().reverse();

    return (
      <Fragment>
        <VerticalFlexContainer>
          <WalletReceive
            walletAddresses={walletAddresses}
            isAddressValid={this.handleIsAddressValid}
            onShareAddress={this.handleShareAddress}
            onCopyAddress={this.handleCopyAddress}
            isIncentivizedTestnet={isIncentivizedTestnet}
            onToggleSubMenus={toggleSubMenus}
          />
        </VerticalFlexContainer>

        <NotificationMessage
          icon={successIcon}
          show={uiNotifications.isOpen(this.notification.id)}
          onClose={this.closeNotification}
          clickToClose
          hasCloseButton
        >
          {this.notification.message}
        </NotificationMessage>
        {uiDialogs.isOpen(WalletReceiveDialog) && addressToShare && (
          <WalletReceiveDialog
            address={addressToShare}
            onCopyAddress={this.handleCopyAddress}
            onDownloadPDF={this.handleDownloadPDF}
            onClose={this.handleCloseShareAddress}
          />
        )}
      </Fragment>
    );
  }
}
