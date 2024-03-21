'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const react_intl_1 = require('react-intl');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const qrcode_react_1 = __importDefault(require('qrcode.react'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/voting/... Remove this comment to see the full error message
const download_app_store_icon_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/voting/download-app-store-icon-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/voting/... Remove this comment to see the full error message
const download_play_store_icon_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/voting/download-play-store-icon-ic.inline.svg')
);
const AppStore_scss_1 = __importDefault(require('./AppStore.scss'));
const AppStore_messages_1 = require('./AppStore.messages');
function AppStore({ onAppleStoreLinkClick, onAndroidStoreLinkClick, intl }) {
  const appleAppButtonUrl = intl.formatMessage(
    AppStore_messages_1.messages.appleAppButtonUrl
  );
  const androidAppButtonUrl = intl.formatMessage(
    AppStore_messages_1.messages.androidAppButtonUrl
  );
  return react_1.default.createElement(
    'div',
    { className: AppStore_scss_1.default.component },
    react_1.default.createElement(
      'div',
      { className: AppStore_scss_1.default.appStoreItem },
      react_1.default.createElement(
        'button',
        {
          className: AppStore_scss_1.default.appStoreButton,
          onClick: () => {
            onAppleStoreLinkClick(appleAppButtonUrl);
          },
        },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: download_app_store_icon_ic_inline_svg_1.default,
          className: AppStore_scss_1.default.appleStoreIcon,
        })
      ),
      react_1.default.createElement(
        'div',
        { className: AppStore_scss_1.default.qrCode },
        react_1.default.createElement(qrcode_react_1.default, {
          value: appleAppButtonUrl,
          size: 75,
          renderAs: 'svg',
        })
      )
    ),
    react_1.default.createElement(
      'div',
      { className: AppStore_scss_1.default.appStoreItem },
      react_1.default.createElement(
        'button',
        {
          className: AppStore_scss_1.default.appStoreButton,
          onClick: () => {
            onAndroidStoreLinkClick(androidAppButtonUrl);
          },
        },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: download_play_store_icon_ic_inline_svg_1.default,
          className: AppStore_scss_1.default.playStoreIcon,
        })
      ),
      react_1.default.createElement(
        'div',
        { className: AppStore_scss_1.default.qrCode },
        react_1.default.createElement(qrcode_react_1.default, {
          value: androidAppButtonUrl,
          size: 75,
          renderAs: 'svg',
        })
      )
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(AppStore);
//# sourceMappingURL=AppStore.js.map
