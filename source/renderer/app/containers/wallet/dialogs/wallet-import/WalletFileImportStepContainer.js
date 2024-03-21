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
const mobx_react_1 = require('mobx-react');
const injectedPropsType_1 = require('../../../../types/injectedPropsType');
const WalletImportFileDialog_1 = __importDefault(
  require('../../../../components/wallet/wallet-import/WalletImportFileDialog')
);
const DefaultProps =
  injectedPropsType_1.InjectedDialogContainerStepDefaultProps;
let WalletFileImportStepContainer = class WalletFileImportStepContainer extends react_1.Component {
  static defaultProps = DefaultProps;
  onSelectExportSourcePath = (params) => {
    this.props.actions.walletMigration.selectExportSourcePath.trigger(params);
  };
  onResetExportSourcePath = () => {
    this.props.actions.walletMigration.resetExportSourcePath.trigger();
  };
  onOpen = () => {
    this.props.actions.walletMigration.resetMigration.trigger();
    this.props.actions.walletMigration.initiateMigration.trigger();
  };
  render() {
    const { onClose, onContinue, stores } = this.props;
    const { walletMigration, app } = stores;
    const {
      exportErrors,
      exportSourcePath,
      defaultExportSourcePath,
      pendingImportWalletsCount,
      isExportRunning,
    } = walletMigration;
    const { openExternalLink } = app;
    return react_1.default.createElement(WalletImportFileDialog_1.default, {
      isSubmitting: isExportRunning,
      exportSourcePath: exportSourcePath,
      defaultExportSourcePath: defaultExportSourcePath,
      exportErrors: exportErrors,
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      pendingImportWalletsCount: pendingImportWalletsCount,
      onOpen: this.onOpen,
      onOpenExternalLink: openExternalLink,
      onSelectExportSourcePath: this.onSelectExportSourcePath,
      onResetExportSourcePath: this.onResetExportSourcePath,
      onClose: onClose,
      onContinue: onContinue,
    });
  }
};
WalletFileImportStepContainer = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  WalletFileImportStepContainer
);
exports.default = WalletFileImportStepContainer;
//# sourceMappingURL=WalletFileImportStepContainer.js.map
