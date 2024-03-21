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
const TopBar_1 = __importDefault(require('../../components/layout/TopBar'));
const TopBarLayout_1 = __importDefault(
  require('../../components/layout/TopBarLayout')
);
const TermsOfUseForm_1 = __importDefault(
  require('../../components/profile/terms-of-use/TermsOfUseForm')
);
let TermsOfUsePage = class TermsOfUsePage extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  onSubmit = () => {
    this.props.actions.profile.acceptTermsOfUse.trigger();
  };
  render() {
    const { app, networkStatus, profile } = this.props.stores;
    const { setTermsOfUseAcceptanceRequest, termsOfUse } = profile;
    const { currentRoute, openExternalLink } = app;
    const { isShelleyActivated } = networkStatus;
    const isSubmitting = setTermsOfUseAcceptanceRequest.isExecuting;
    const topbar = react_1.default.createElement(
      TopBar_1.default,
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      {
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        currentRoute: currentRoute,
        showSubMenuToggle: false,
        isShelleyActivated: isShelleyActivated,
      }
    );
    return react_1.default.createElement(
      TopBarLayout_1.default,
      { topbar: topbar },
      react_1.default.createElement(TermsOfUseForm_1.default, {
        localizedTermsOfUse: termsOfUse,
        onSubmit: this.onSubmit,
        isSubmitting: isSubmitting,
        error: setTermsOfUseAcceptanceRequest.error,
        onOpenExternalLink: openExternalLink,
      })
    );
  }
};
TermsOfUsePage = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  TermsOfUsePage
);
exports.default = TermsOfUsePage;
//# sourceMappingURL=TermsOfUsePage.js.map
