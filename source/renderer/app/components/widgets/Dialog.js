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
// @ts-nocheck
const react_1 = __importStar(require('react'));
const lodash_1 = require('lodash');
const classnames_1 = __importDefault(require('classnames'));
const Modal_1 = require('@react-polymorph/components/Modal');
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const ModalSkin_1 = require('@react-polymorph/skins/simple/ModalSkin');
const Dialog_scss_1 = __importDefault(require('./Dialog.scss'));
const DialogOverride_scss_1 = __importDefault(require('./DialogOverride.scss'));
const DialogFullSizeOverride_scss_1 = __importDefault(
  require('./DialogFullSizeOverride.scss')
);
const defaultActionOptions = {
  direction: 'row',
  items: [],
};
class Dialog extends react_1.Component {
  render() {
    const {
      title,
      subtitle,
      children,
      footer,
      actions,
      closeOnOverlayClick,
      onClose,
      className,
      closeButton,
      backButton,
      primaryButtonAutoFocus,
      defaultThemeOverrides,
      fullSize,
      scrollWrapperRef,
    } = this.props;
    const { items, direction } = Array.isArray(actions)
      ? { ...defaultActionOptions, items: actions }
      : { ...defaultActionOptions, ...actions };
    let themeOverrides;
    if (defaultThemeOverrides) themeOverrides = DialogOverride_scss_1.default;
    else if (fullSize) themeOverrides = DialogFullSizeOverride_scss_1.default;
    const classActionsClasses = (0, classnames_1.default)([
      Dialog_scss_1.default.actions,
      Dialog_scss_1.default[`${direction}Direction`],
    ]);
    return react_1.default.createElement(
      Modal_1.Modal,
      {
        isOpen: true,
        triggerCloseOnOverlayClick: closeOnOverlayClick,
        onClose: onClose,
        skin: ModalSkin_1.ModalSkin,
        themeOverrides: themeOverrides,
      },
      react_1.default.createElement(
        'div',
        {
          className: (0, classnames_1.default)([
            Dialog_scss_1.default.dialogWrapper,
            className,
          ]),
        },
        title &&
          react_1.default.createElement(
            'div',
            { className: Dialog_scss_1.default.title },
            react_1.default.createElement('h1', null, title)
          ),
        subtitle &&
          react_1.default.createElement(
            'div',
            { className: Dialog_scss_1.default.subtitle },
            react_1.default.createElement('h1', null, subtitle)
          ),
        children &&
          react_1.default.createElement(
            'div',
            {
              className: (0, classnames_1.default)(
                Dialog_scss_1.default.contentWrapper,
                items.length && Dialog_scss_1.default.contentWithActions
              ),
              ref: (ref) => {
                // @ts-ignore ts-migrate(2339) FIXME: Property 'current' does not exist on type 'unknown... Remove this comment to see the full error message
                if (scrollWrapperRef) scrollWrapperRef.current = ref;
              },
            },
            react_1.default.createElement(
              'div',
              { className: Dialog_scss_1.default.content },
              children
            )
          ),
        footer && react_1.default.createElement('div', null, footer),
        items &&
          react_1.default.createElement(
            'div',
            { className: classActionsClasses },
            (0, lodash_1.map)(items, (action, key) => {
              const buttonClasses = (0, classnames_1.default)([
                action.className ? action.className : null,
                action.primary ? 'primary' : 'flat',
              ]);
              return !action.isLink
                ? react_1.default.createElement(Button_1.Button, {
                    key: key,
                    className: buttonClasses,
                    label: action.label,
                    onClick: action.onClick,
                    disabled: action.disabled,
                    skin: ButtonSkin_1.ButtonSkin,
                    autoFocus: action.primary ? primaryButtonAutoFocus : false,
                  })
                : react_1.default.createElement(Link_1.Link, {
                    key: key,
                    className: action.className,
                    onClick: action.onClick,
                    label: action.label,
                    skin: LinkSkin_1.LinkSkin,
                    hasIconAfter: action.hasIconAfter,
                    hasIconBefore: action.hasIconBefore,
                  });
            })
          ),
        closeButton
          ? react_1.default.cloneElement(closeButton, {
              onClose,
            })
          : null,
        backButton
      )
    );
  }
}
exports.default = Dialog;
//# sourceMappingURL=Dialog.js.map
