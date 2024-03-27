'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const path_1 = __importDefault(require('path'));
const react_intl_1 = require('react-intl');
const mobx_react_1 = require('mobx-react');
const show_file_dialog_channels_1 = require('../../ipc/show-file-dialog-channels');
const WalletReceiveRandom_1 = __importDefault(
  require('../../components/wallet/receive/WalletReceiveRandom')
);
const WalletReceiveSequential_1 = __importDefault(
  require('../../components/wallet/receive/WalletReceiveSequential')
);
const VerticalFlexContainer_1 = __importDefault(
  require('../../components/layout/VerticalFlexContainer')
);
const WalletReceiveDialog_1 = __importDefault(
  require('../../components/wallet/receive/WalletReceiveDialog')
);
const files_1 = require('../../../../common/utils/files');
const strings_1 = require('../../utils/strings');
const reporting_1 = require('../../../../common/utils/reporting');
const messages = (0, react_intl_1.defineMessages)({
  address: {
    id: 'wallet.receive.pdf.filenamePrefix',
    defaultMessage: '!!!Address',
    description: '"Address" word in the Address PDF export',
  },
});
let WalletReceivePage = class WalletReceivePage extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
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
  handleIsAddressValid = (index) => index < 3 || index > 7;
  handleCopyAddress = (copiedAddress) => {
    const address = (0, strings_1.ellipsis)(copiedAddress, 15, 15);
    this.props.actions.wallets.copyAddress.trigger({
      address,
    });
  };
  handleShareAddress = (addressToShare) => {
    const { activeWallet } = this;
    const { actions, stores } = this.props;
    const { dialogs } = actions;
    const { hardwareWallets } = stores;
    this.setState({
      addressToShare,
    });
    const dialog = WalletReceiveDialog_1.default;
    if (activeWallet && activeWallet.isHardwareWallet) {
      try {
        hardwareWallets.initiateAddressVerification(addressToShare);
      } catch (err) {
        hardwareWallets.resetInitializedAddressVerification({
          cancelDeviceAction: true,
        });
      }
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
    const name = (0, files_1.generateFileNameWithTimestamp)({
      prefix,
      extension: '',
      isUTC: false,
    });
    const { desktopDirectoryPath } = this.props.stores.profile;
    const defaultPath = path_1.default.join(
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
    const {
      filePath,
    } = await show_file_dialog_channels_1.showSaveDialogChannel.send(params);
    if (!filePath || !addressToShare) return {};
    const { id: address } = addressToShare;
    return {
      filePath,
      address,
    };
  };
  handleDownloadPDF = async (note) => {
    const { address, filePath } = await this.getAddressAndFilepath();
    // if cancel button is clicked or path is empty
    if (!filePath || !address) return;
    this.props.actions.wallets.generateAddressPDF.trigger({
      note,
      address,
      filePath,
      wallet: this.activeWallet,
    });
  };
  handleSaveQRCodeImage = async () => {
    const { address, filePath } = await this.getAddressAndFilepath('png');
    // if cancel button is clicked or path is empty
    if (!filePath || !address) return;
    this.props.actions.wallets.saveQRCodeImage.trigger({
      address,
      filePath,
      wallet: this.activeWallet,
    });
  };
  handleGenerateAddress = (passphrase) => {
    const { activeWallet } = this;
    if (activeWallet) {
      this.props.actions.addresses.createByronWalletAddress.trigger({
        walletId: activeWallet.id,
        passphrase,
      });
    }
  };
  handleSupportRequestClick = async (supportRequestLinkUrl) => {
    const { profile, app } = this.props.stores;
    const { environment, openExternalLink } = app;
    const supportUrl = (0, reporting_1.generateSupportRequestLink)(
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
    const localWalletData = getLocalWalletDataById(
      activeWallet ? activeWallet.id : ''
    );
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
    return react_1.default.createElement(
      react_1.Fragment,
      null,
      react_1.default.createElement(
        VerticalFlexContainer_1.default,
        null,
        isRandom
          ? react_1.default.createElement(WalletReceiveRandom_1.default, {
              walletAddress: byronWalletAddress,
              isWalletAddressUsed: isByronWalletAddressUsed,
              walletAddresses: walletAddresses,
              onGenerateAddress: this.handleGenerateAddress,
              onShareAddress: this.handleShareAddress,
              onCopyAddress: this.handleCopyAddress,
              isSidebarExpanded: sidebar.isShowingSubMenus,
              walletHasPassword: hasPassword,
              isSubmitting:
                addresses.createByronWalletAddressRequest.isExecuting,
              error: addresses.error,
              showUsed: showUsedAddresses,
              onToggleUsedAddresses: this.handleToggleUsedAddresses,
            })
          : react_1.default.createElement(WalletReceiveSequential_1.default, {
              walletAddresses: walletAddresses,
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              isAddressValid: this.handleIsAddressValid,
              onShareAddress: this.handleShareAddress,
              onCopyAddress: this.handleCopyAddress,
              onToggleSubMenus: toggleSubMenus,
              showUsed: showUsedAddresses,
              onToggleUsedAddresses: this.handleToggleUsedAddresses,
            })
      ),
      uiDialogs.isOpen(WalletReceiveDialog_1.default) &&
        addressToShare &&
        react_1.default.createElement(WalletReceiveDialog_1.default, {
          address: addressToShare,
          walletName: activeWallet.name,
          onCopyAddress: this.handleCopyAddress,
          onDownloadPDF: this.handleDownloadPDF,
          onSaveQRCodeImage: this.handleSaveQRCodeImage,
          onClose: this.handleCloseShareAddress,
          isHardwareWallet: activeWallet.isHardwareWallet,
          hwDeviceStatus: hwDeviceStatus,
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          transportDevice: transportDevice,
          isAddressDerived: isAddressDerived,
          isAddressChecked: isAddressChecked,
          onChangeVerificationStatus: setAddressVerificationCheckStatus,
          onSupportRequestClick: this.handleSupportRequestClick,
          isTrezor: isTrezor,
        })
    );
  }
};
WalletReceivePage = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  WalletReceivePage
);
exports.default = WalletReceivePage;
//# sourceMappingURL=WalletReceivePage.js.map
