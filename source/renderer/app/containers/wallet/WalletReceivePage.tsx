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
import { generateSupportRequestLink } from '../../../../common/utils/reporting';
import type { WalletLocalData } from '../../api/utils/localStorage';

const messages = defineMessages({
  address: {
    id: 'wallet.receive.pdf.filenamePrefix',
    defaultMessage: '!!!Address',
    description: '"Address" word in the Address PDF export',
  },
});
type Props = InjectedProps;
type State = {
  addressToShare?: WalletAddress | null | undefined;
};

@inject('stores', 'actions')
@observer
class WalletReceivePage extends Component<Props, State> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
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
    this.props.actions.wallets.copyAddress.trigger({
      address,
    });
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

    if (activeWallet && activeWallet.isHardwareWallet) {
      hardwareWallets.initiateAddressVerification(addressToShare);
    }

    dialogs.open.trigger({
      dialog,
    });
  };
  handleCloseShareAddress = () => {
    const { activeWallet } = this;
    const { actions, stores } = this.props;
    const { dialogs } = actions;
    const { hardwareWallets } = stores;
    dialogs.closeActiveDialog.trigger();

    if (activeWallet && activeWallet.isHardwareWallet) {
      hardwareWallets.resetInitializedAddressVerification({
        cancelDeviceAction: true,
      });
    }
  };
  handleToggleUsedAddresses = () => {
    this.props.actions.walletSettings.toggleShowUsedAddresses.trigger();
  };
  getAddressAndFilepath = async (fileExtension = 'pdf') => {
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
  handleSupportRequestClick = async (supportRequestLinkUrl: string) => {
    const { profile, app } = this.props.stores;
    const { environment, openExternalLink } = app;
    const supportUrl = generateSupportRequestLink(
      supportRequestLinkUrl,
      environment,
      profile.currentLocale
    );
    openExternalLink(supportUrl);
    this.handleCloseShareAddress();
  };

  render() {
    const { actions, stores } = this.props;
    const {
      uiDialogs,
      addresses,
      sidebar,
      hardwareWallets,
      walletSettings,
    } = stores;
    const { activeWallet } = this;
    const { addressToShare } = this.state;
    const { toggleSubMenus } = actions.sidebar;
    const {
      hwDeviceStatus,
      transportDevice,
      isAddressDerived,
      isAddressChecked,
      setAddressVerificationCheckStatus,
      checkIsTrezorByWalletId,
    } = hardwareWallets;
    const { getLocalWalletDataById } = walletSettings;
    const localWalletData:
      | WalletLocalData
      | null
      | undefined = getLocalWalletDataById(activeWallet ? activeWallet.id : '');
    const { showUsedAddresses } = localWalletData || {};
    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletReceivePage.');
    const { hasPassword, isRandom } = activeWallet;
    const walletAddresses = addresses.all.slice().reverse();
    const byronWalletAddress = addresses.active ? addresses.active.id : '';
    const isByronWalletAddressUsed = addresses.active
      ? addresses.active.used
      : false;
    const isTrezor = checkIsTrezorByWalletId(activeWallet.id);
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
              showUsed={showUsedAddresses}
              onToggleUsedAddresses={this.handleToggleUsedAddresses}
            />
          ) : (
            <WalletReceiveSequential
              walletAddresses={walletAddresses}
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              isAddressValid={this.handleIsAddressValid}
              onShareAddress={this.handleShareAddress}
              onCopyAddress={this.handleCopyAddress}
              onToggleSubMenus={toggleSubMenus}
              showUsed={showUsedAddresses}
              onToggleUsedAddresses={this.handleToggleUsedAddresses}
            />
          )}
        </VerticalFlexContainer>

        {uiDialogs.isOpen(WalletReceiveDialog) && addressToShare && (
          <WalletReceiveDialog
            address={addressToShare}
            walletName={activeWallet.name}
            onCopyAddress={this.handleCopyAddress}
            onDownloadPDF={this.handleDownloadPDF}
            onSaveQRCodeImage={this.handleSaveQRCodeImage}
            onClose={this.handleCloseShareAddress}
            isHardwareWallet={activeWallet.isHardwareWallet}
            hwDeviceStatus={hwDeviceStatus}
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            transportDevice={transportDevice}
            isAddressDerived={isAddressDerived}
            isAddressChecked={isAddressChecked}
            onChangeVerificationStatus={setAddressVerificationCheckStatus}
            onSupportRequestClick={this.handleSupportRequestClick}
            isTrezor={isTrezor}
          />
        )}
      </Fragment>
    );
  }
}

export default WalletReceivePage;
