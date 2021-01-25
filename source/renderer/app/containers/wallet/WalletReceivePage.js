// @flow
import React, { Component, Fragment } from 'react';
import path from 'path';
import { defineMessages, intlShape } from 'react-intl';
import { observer, inject } from 'mobx-react';
import { showSaveDialogChannel } from '../../ipc/show-file-dialog-channels';
import WalletReceiveRandom from '../../components/wallet/receive/WalletReceiveRandom';
import WalletReceiveSequential from '../../components/wallet/receive/WalletReceiveSequential';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import WalletReceiveDialog from '../../components/wallet/receive/WalletReceiveDialog';
import type { InjectedProps } from '../../types/injectedPropsType';
import WalletAddress from '../../domains/WalletAddress';
import { generateFileNameWithTimestamp } from '../../../../common/utils/files';
import { ellipsis } from '../../utils/strings';

const messages = defineMessages({
  address: {
    id: 'wallet.receive.pdf.filenamePrefix',
    defaultMessage: '!!!Address',
    description: '"Address" word in the Address PDF export',
  },
});

type Props = InjectedProps;

type State = {
  addressToShare?: ?WalletAddress,
};

@inject('stores', 'actions')
@observer
export default class WalletReceivePage extends Component<Props, State> {
  static defaultProps = { actions: null, stores: null };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    addressToShare: null,
  };

  get activeWallet() {
    return this.props.stores.wallets.active;
  }

  componentWillUnmount() {
    const { actions } = this.props;
    actions.addresses.resetErrors.trigger();
    actions.notifications.closeNotification.trigger({
      id: 'copyAddress',
    });
  }

  handleIsAddressValid = (index: number) => index < 3 || index > 7;

  handleCopyAddress = (copiedAddress: string) => {
    const address = ellipsis(copiedAddress, 15, 15);
    this.props.actions.wallets.copyAddress.trigger({ address });
  };

  handleShareAddress = (addressToShare: WalletAddress) => {
    const { activeWallet } = this;
    const { actions, stores } = this.props;
    const { dialogs } = actions;
    const { hardwareWallets } = stores;

    this.setState({
      addressToShare,
    });
    const dialog = WalletReceiveDialog;
    if (activeWallet) {
      const showAddressVerification = hardwareWallets.isAddressVerificationEnabled(
        activeWallet.id
      );
      if (showAddressVerification) {
        hardwareWallets.initiateAddressVerification(addressToShare);
      }
    }
    dialogs.open.trigger({ dialog });
  };

  handleCloseShareAddress = () => {
    const { actions, stores } = this.props;
    const { dialogs } = actions;
    const { hardwareWallets } = stores;

    dialogs.closeActiveDialog.trigger();
    hardwareWallets.resetInitializedAddressVerification();
  };

  getAddressAndFilepath = async (fileExtension?: string = 'pdf') => {
    const { addressToShare } = this.state;
    const { activeWallet } = this;
    const { intl } = this.context;
    if (!activeWallet) return {};

    const prefix = `${intl.formatMessage(messages.address)}-${
      activeWallet.name
    }`;

    const name = generateFileNameWithTimestamp({
      prefix,
      extension: '',
      isUTC: false,
    });
    const { desktopDirectoryPath } = this.props.stores.profile;
    const defaultPath = path.join(
      desktopDirectoryPath,
      `${name}.${fileExtension}`
    );
    const params = {
      defaultPath,
      filters: [
        {
          name,
          extensions: [fileExtension],
        },
      ],
    };
    const { filePath } = await showSaveDialogChannel.send(params);

    if (!filePath || !addressToShare) return {};

    const { id: address } = addressToShare;

    return {
      filePath,
      address,
    };
  };

  handleDownloadPDF = async (note: string) => {
    const { address, filePath } = await this.getAddressAndFilepath();

    // if cancel button is clicked or path is empty
    if (!filePath || !address) return;

    this.handleCloseShareAddress();
    this.props.actions.wallets.generateAddressPDF.trigger({
      note,
      address,
      filePath,
    });
  };

  handleSaveQRCodeImage = async () => {
    const { address, filePath } = await this.getAddressAndFilepath('png');

    // if cancel button is clicked or path is empty
    if (!filePath || !address) return;

    this.handleCloseShareAddress();
    this.props.actions.wallets.saveQRCodeImage.trigger({
      address,
      filePath,
    });
  };

  handleGenerateAddress = (passphrase: string) => {
    const { activeWallet } = this;
    if (activeWallet) {
      this.props.actions.addresses.createByronWalletAddress.trigger({
        walletId: activeWallet.id,
        passphrase,
      });
    }
  };

  render() {
    const { actions, stores } = this.props;
    const { uiDialogs, addresses, sidebar, hardwareWallets } = stores;
    const { activeWallet } = this;
    const { addressToShare } = this.state;
    const { toggleSubMenus } = actions.sidebar;
    const {
      isAddressVerificationEnabled,
      hwDeviceStatus,
      transportDevice,
      isAddressDerived,
      isAddressChecked,
      setAddressVerificationCheckStatus,
    } = hardwareWallets;

    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletReceivePage.');

    const { hasPassword, isRandom } = activeWallet;
    const walletAddresses = addresses.all.slice().reverse();
    const byronWalletAddress = addresses.active ? addresses.active.id : '';
    const isByronWalletAddressUsed = addresses.active
      ? addresses.active.used
      : false;

    return (
      <Fragment>
        <VerticalFlexContainer>
          {isRandom ? (
            <WalletReceiveRandom
              walletAddress={byronWalletAddress}
              isWalletAddressUsed={isByronWalletAddressUsed}
              walletAddresses={walletAddresses}
              onGenerateAddress={this.handleGenerateAddress}
              onShareAddress={this.handleShareAddress}
              onCopyAddress={this.handleCopyAddress}
              isSidebarExpanded={sidebar.isShowingSubMenus}
              walletHasPassword={hasPassword}
              isSubmitting={
                addresses.createByronWalletAddressRequest.isExecuting
              }
              error={addresses.error}
            />
          ) : (
            <WalletReceiveSequential
              walletAddresses={walletAddresses}
              isAddressValid={this.handleIsAddressValid}
              onShareAddress={this.handleShareAddress}
              onCopyAddress={this.handleCopyAddress}
              onToggleSubMenus={toggleSubMenus}
            />
          )}
        </VerticalFlexContainer>

        {uiDialogs.isOpen(WalletReceiveDialog) && addressToShare && (
          <WalletReceiveDialog
            address={addressToShare}
            onCopyAddress={this.handleCopyAddress}
            onDownloadPDF={this.handleDownloadPDF}
            onSaveQRCodeImage={this.handleSaveQRCodeImage}
            onClose={this.handleCloseShareAddress}
            isAddressVerificationEnabled={isAddressVerificationEnabled(
              activeWallet.id
            )}
            walletName={activeWallet.name}
            hwDeviceStatus={hwDeviceStatus}
            transportDevice={transportDevice}
            isAddressDerived={isAddressDerived}
            isAddressChecked={isAddressChecked}
            onChangeVerificationStatus={setAddressVerificationCheckStatus}
          />
        )}
      </Fragment>
    );
  }
}
