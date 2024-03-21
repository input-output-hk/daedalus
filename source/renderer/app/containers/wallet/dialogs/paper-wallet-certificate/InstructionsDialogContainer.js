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
const mobx_react_1 = require('mobx-react');
const moment_1 = __importDefault(require('moment'));
const show_file_dialog_channels_1 = require('../../../../ipc/show-file-dialog-channels');
const InstructionsDialog_1 = __importDefault(
  require('../../../../components/wallet/paper-wallet-certificate/InstructionsDialog')
);
const files_1 = require('../../../../../../common/utils/files');
let InstructionsDialogContainer = class InstructionsDialogContainer extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  onPrint = async () => {
    const {
      currentDateFormat,
      currentTimeFormatShort,
    } = this.props.stores.profile;
    const date = (0, moment_1.default)();
    const formattedDate = date.format(currentDateFormat);
    const formattedTime = date.format(currentTimeFormatShort);
    const timestamp = `${formattedDate} - ${formattedTime}`;
    const name = (0, files_1.generateFileNameWithTimestamp)({
      prefix: 'paper-wallet-certificate',
      date,
      extension: '',
      isUTC: false,
    });
    const { desktopDirectoryPath } = this.props.stores.profile;
    const defaultPath = path_1.default.join(
      desktopDirectoryPath,
      `${name}.pdf`
    );
    const params = {
      defaultPath,
      filters: [
        {
          name,
          extensions: ['pdf'],
        },
      ],
    };
    const {
      filePath,
    } = await show_file_dialog_channels_1.showSaveDialogChannel.send(params);
    // if cancel button is clicked or path is empty
    if (!filePath) return;
    this.props.actions.wallets.generateCertificate.trigger({
      filePath,
      timestamp,
    });
  };
  handleOpenExternalLink = (url) => {
    const { openExternalLink } = this.props.stores.app;
    openExternalLink(url);
  };
  render() {
    const { wallets, app } = this.props.stores;
    const {
      environment: { network },
    } = app;
    return react_1.default.createElement(InstructionsDialog_1.default, {
      inProgress: wallets.generatingCertificateInProgress,
      error: wallets.generatingCertificateError,
      network: network,
      onPrint: this.onPrint,
      onClose: this.props.onClose,
      onOpenExternalLink: this.handleOpenExternalLink,
    });
  }
};
InstructionsDialogContainer = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  InstructionsDialogContainer
);
exports.default = InstructionsDialogContainer;
//# sourceMappingURL=InstructionsDialogContainer.js.map
