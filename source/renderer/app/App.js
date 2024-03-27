'use strict';
// @ts-nocheck
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
const ThemeProvider_1 = require('@react-polymorph/components/ThemeProvider');
const simple_1 = require('@react-polymorph/skins/simple');
const simple_2 = require('@react-polymorph/themes/simple');
const react_router_dom_1 = require('react-router-dom');
const react_intl_1 = require('react-intl');
const Routes_1 = require('./Routes');
const daedalus_1 = require('./themes/daedalus');
const overrides_1 = require('./themes/overrides');
const translations_1 = __importDefault(require('./i18n/translations'));
const ThemeManager_1 = __importDefault(require('./ThemeManager'));
const AboutDialog_1 = __importDefault(
  require('./containers/static/AboutDialog')
);
const DaedalusDiagnosticsDialog_1 = __importDefault(
  require('./containers/status/DaedalusDiagnosticsDialog')
);
const NotificationsContainer_1 = __importDefault(
  require('./containers/notifications/NotificationsContainer')
);
const NewsOverlayContainer_1 = __importDefault(
  require('./containers/news/NewsOverlayContainer')
);
const constants_1 = require('../../common/ipc/constants');
const NewsFeedContainer_1 = __importDefault(
  require('./containers/news/NewsFeedContainer')
);
const ToggleRTSFlagsDialogContainer_1 = __importDefault(
  require('./containers/knownIssues/ToggleRTSFlagsDialogContainer')
);
const RTSFlagsRecommendationOverlayContainer_1 = __importDefault(
  require('./containers/knownIssues/RTSFlagsRecommendationOverlayContainer')
);
const MenuUpdater_1 = require('./containers/MenuUpdater');
let App = class App extends react_1.Component {
  componentDidMount() {
    // Loads app's global environment variables into AppStore via ipc
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    this.props.actions.app.initAppEnvironment.trigger();
  }
  render() {
    const { stores, actions, history } = this.props;
    const { app, networkStatus } = stores;
    const { isActiveDialog, isSetupPage } = app;
    const { isNodeStopping, isNodeStopped } = networkStatus;
    const locale = stores.profile.currentLocale;
    const { currentTheme } = stores.profile;
    const themeVars = require(`./themes/daedalus/${currentTheme}.ts`).default;
    const {
      ABOUT,
      DAEDALUS_DIAGNOSTICS,
      TOGGLE_RTS_FLAGS_MODE,
    } = constants_1.DIALOGS;
    const canShowNews =
      !isSetupPage && // Active page is not "Language Selection" or "Terms of Use"
      !isNodeStopping && // Daedalus is not shutting down
      !isNodeStopped;
    // Daedalus is not shutting down
    if (document.documentElement) {
      document.documentElement.lang = locale;
    }
    return react_1.default.createElement(
      react_1.Fragment,
      null,
      react_1.default.createElement(ThemeManager_1.default, {
        variables: themeVars,
      }),
      react_1.default.createElement(
        mobx_react_1.Provider,
        { stores: stores, actions: actions },
        react_1.default.createElement(MenuUpdater_1.MenuUpdater, {
          stores: stores,
        }),
        react_1.default.createElement(
          ThemeProvider_1.ThemeProvider,
          {
            theme: daedalus_1.daedalusTheme,
            skins: simple_1.SimpleSkins,
            variables: simple_2.SimpleDefaults,
            themeOverrides: overrides_1.themeOverrides,
          },
          react_1.default.createElement(
            react_intl_1.IntlProvider,
            {
              ...{
                locale,
                key: locale,
                messages: translations_1.default[locale],
              },
            },
            react_1.default.createElement(
              react_1.Fragment,
              null,
              react_1.default.createElement(
                react_router_dom_1.Router,
                { history: history },
                react_1.default.createElement(Routes_1.Routes, null)
              ),
              [
                // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
                isActiveDialog(ABOUT) &&
                  react_1.default.createElement(AboutDialog_1.default, {
                    key: 'aboutDialog',
                  }),
                // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
                isActiveDialog(DAEDALUS_DIAGNOSTICS) &&
                  react_1.default.createElement(
                    DaedalusDiagnosticsDialog_1.default,
                    { key: 'daedalusDiagnosticsDialog' }
                  ),
                // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string' is not assignable to par... Remove this comment to see the full error message
                isActiveDialog(TOGGLE_RTS_FLAGS_MODE) &&
                  react_1.default.createElement(
                    ToggleRTSFlagsDialogContainer_1.default,
                    { key: 'toggleRTSFlagsDialog' }
                  ),
              ],
              react_1.default.createElement(
                RTSFlagsRecommendationOverlayContainer_1.default,
                null
              ),
              react_1.default.createElement(
                NotificationsContainer_1.default,
                null
              ),
              canShowNews && [
                react_1.default.createElement(NewsFeedContainer_1.default, {
                  key: 'newsFeedList',
                }),
                react_1.default.createElement(NewsOverlayContainer_1.default, {
                  key: 'newsFeedOverlay',
                }),
              ]
            )
          )
        )
      )
    );
  }
};
App = __decorate([mobx_react_1.observer], App);
exports.default = App;
//# sourceMappingURL=App.js.map
