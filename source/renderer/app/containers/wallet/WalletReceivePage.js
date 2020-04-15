// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import WalletReceiveRandom from '../../components/wallet/receive/WalletReceiveRandom';
import WalletReceiveSequential from '../../components/wallet/receive/WalletReceiveSequential';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import WalletReceiveDialog from '../../components/wallet/receive/WalletReceiveDialog';
import type { InjectedProps } from '../../types/injectedPropsType';
import WalletAddress from '../../domains/WalletAddress';
import { generateFileNameWithTimestamp } from '../../../../common/utils/files';
import { ellipsis } from '../../utils/strings';

type Props = InjectedProps;

type State = {
  addressToShare?: ?WalletAddress,
};

@inject('stores', 'actions')
@observer
export default class WalletReceivePage extends Component<Props, State> {
  static defaultProps = { actions: null, stores: null };

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
    const { uiDialogs, addresses, sidebar } = stores;
    const { activeWallet } = this;
    const { addressToShare } = this.state;
    const { toggleSubMenus } = actions.sidebar;
    const { isIncentivizedTestnet } = global;
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
              isIncentivizedTestnet={isIncentivizedTestnet}
              onToggleSubMenus={toggleSubMenus}
            />
          )}
        </VerticalFlexContainer>

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
