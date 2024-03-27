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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const Button_1 = require('@react-polymorph/components/Button');
const Input_1 = require('@react-polymorph/components/Input');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_intl_1 = require('react-intl');
const PublicKeyFieldSkin_1 = __importDefault(require('./PublicKeyFieldSkin'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/qr-code... Remove this comment to see the full error message
const qr_code_inline_svg_1 = __importDefault(
  require('../../../assets/images/qr-code.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/reveal-... Remove this comment to see the full error message
const reveal_key_inline_svg_1 = __importDefault(
  require('../../../assets/images/reveal-key.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/hide-ke... Remove this comment to see the full error message
const hide_key_inline_svg_1 = __importDefault(
  require('../../../assets/images/hide-key.inline.svg')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const locales_types_1 = require('../../../../../common/types/locales.types');
const PublicKeyField_scss_1 = __importDefault(require('./PublicKeyField.scss'));
function PublicKeyField(props) {
  const {
    publicKey,
    onOpenWalletKeyDialog,
    onShowQRCode,
    locale,
    intl,
    messages,
    description,
    onCopyPublicKey,
  } = props;
  const [publicKeyHidden, setPublicKeyHidden] = (0, react_1.useState)(
    !publicKey
  );
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  const togglePublicKeyVisibility = (0, react_1.useCallback)(() => {
    if (!publicKey) {
      onOpenWalletKeyDialog();
    } else {
      setPublicKeyHidden((prevCheck) => !prevCheck);
    }
  });
  // This is called when the publicKey is set
  (0, react_1.useEffect)(() => {
    setPublicKeyHidden(!publicKey);
  }, [publicKey]);
  // This is called when the component is mounted the first time
  (0, react_1.useEffect)(() => {
    setPublicKeyHidden(true);
  }, []);
  const handleCopyPublicKey = (0, react_1.useCallback)(
    () => onCopyPublicKey(publicKey),
    [publicKey]
  );
  const fieldStyles = (0, classnames_1.default)([
    PublicKeyField_scss_1.default.field,
    publicKeyHidden || !publicKey
      ? PublicKeyField_scss_1.default.valueHidden
      : PublicKeyField_scss_1.default.valueShown,
    locale === locales_types_1.LOCALES.japanese
      ? PublicKeyField_scss_1.default.withBigToggleButton
      : null,
  ]);
  const hiddenValuePlaceholder = intl.formatMessage(
    messages.publicKeyShowInstruction
  );
  const toggleButtonTooltip = intl.formatMessage(
    global_messages_1.default[publicKeyHidden ? 'reveal' : 'hide']
  );
  const qrCodeButtonStyles = (0, classnames_1.default)([
    PublicKeyField_scss_1.default.imageButton,
    PublicKeyField_scss_1.default.qrCodeButton,
    'flat',
  ]);
  const revealHideButtonStyles = (0, classnames_1.default)([
    PublicKeyField_scss_1.default.imageButton,
    publicKeyHidden
      ? PublicKeyField_scss_1.default.revealButton
      : PublicKeyField_scss_1.default.hideButton,
    'flat',
  ]);
  return react_1.default.createElement(
    'div',
    { className: PublicKeyField_scss_1.default.component },
    react_1.default.createElement(
      'div',
      { className: PublicKeyField_scss_1.default.title },
      intl.formatMessage(messages.publicKey)
    ),
    !!description &&
      react_1.default.createElement(
        'div',
        { className: PublicKeyField_scss_1.default.contentBox },
        description
      ),
    react_1.default.createElement(
      'div',
      { className: PublicKeyField_scss_1.default.inputBox },
      react_1.default.createElement(Input_1.Input, {
        className: fieldStyles,
        type: 'text',
        value: publicKeyHidden ? hiddenValuePlaceholder : publicKey,
        readOnly: true,
        skin: PublicKeyFieldSkin_1.default,
        tooltip: intl.formatMessage(global_messages_1.default.copy),
        valueVisible: !publicKeyHidden,
        onCopyValue: handleCopyPublicKey,
      }),
      react_1.default.createElement(
        'div',
        { className: PublicKeyField_scss_1.default.addons },
        !publicKeyHidden &&
          react_1.default.createElement(
            'div',
            { className: PublicKeyField_scss_1.default.imageButtonContainer },
            react_1.default.createElement(
              PopOver_1.PopOver,
              { content: intl.formatMessage(messages.showQRCode) },
              react_1.default.createElement(Button_1.Button, {
                className: qrCodeButtonStyles,
                onClick: onShowQRCode,
                label: react_1.default.createElement(
                  react_svg_inline_1.default,
                  { svg: qr_code_inline_svg_1.default }
                ),
              })
            )
          ),
        react_1.default.createElement(
          PopOver_1.PopOver,
          { content: toggleButtonTooltip },
          react_1.default.createElement(Button_1.Button, {
            className: revealHideButtonStyles,
            label: publicKeyHidden
              ? react_1.default.createElement(react_svg_inline_1.default, {
                  svg: reveal_key_inline_svg_1.default,
                })
              : react_1.default.createElement(react_svg_inline_1.default, {
                  svg: hide_key_inline_svg_1.default,
                }),
            onClick: togglePublicKeyVisibility,
          })
        )
      )
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(PublicKeyField);
//# sourceMappingURL=PublicKeyField.js.map
