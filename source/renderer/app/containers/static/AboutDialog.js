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
const react_modal_1 = __importDefault(require('react-modal'));
const About_1 = __importDefault(require('../../components/static/About'));
const AboutDialog_scss_1 = __importDefault(require('./AboutDialog.scss'));
let AboutDialog = class AboutDialog extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  render() {
    const { app } = this.props.stores;
    const { openExternalLink, environment } = app;
    const { actions } = this.props;
    const { closeAboutDialog } = actions.app;
    const { apiVersion, nodeVersion, build, os, version } = environment;
    return react_1.default.createElement(
      react_modal_1.default,
      {
        isOpen: true,
        onRequestClose: closeAboutDialog.trigger,
        shouldCloseOnOverlayClick: false,
        className: AboutDialog_scss_1.default.dialog,
        overlayClassName: AboutDialog_scss_1.default.overlay,
        ariaHideApp: false,
      },
      react_1.default.createElement(About_1.default, {
        apiVersion: apiVersion,
        nodeVersion: nodeVersion,
        build: build,
        onOpenExternalLink: openExternalLink,
        os: os,
        version: version,
        onClose: closeAboutDialog.trigger,
      })
    );
  }
};
AboutDialog = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  AboutDialog
);
exports.default = AboutDialog;
//# sourceMappingURL=AboutDialog.js.map
