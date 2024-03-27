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
const react_1 = __importStar(require('react'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const Notification_scss_1 = __importDefault(require('./Notification.scss'));
const close_cross_inline_svg_1 = __importDefault(
  require('../../assets/images/close-cross.inline.svg')
);
const NotificationActions_1 = __importDefault(require('./NotificationActions'));
class Notification extends react_1.Component {
  static defaultProps = {
    clickToClose: true,
    hasCloseButton: true,
    hasEllipsis: false,
    hasSpinner: false,
  };
  render() {
    const {
      actions,
      children,
      clickToClose,
      hasCloseButton,
      hasEllipsis,
      hasSpinner,
      icon,
      index,
      isVisible,
      onClose,
      themeOverride,
    } = this.props;
    const isClickToClose = clickToClose && !actions;
    const notificationMessageStyles = (0, classnames_1.default)([
      Notification_scss_1.default.component,
      isVisible ? Notification_scss_1.default.isVisible : null,
      isClickToClose ? Notification_scss_1.default.clickToClose : null,
      themeOverride === 'grey'
        ? Notification_scss_1.default.themeOverrideGrey
        : null,
    ]);
    const messageStyles = (0, classnames_1.default)([
      Notification_scss_1.default.message,
      hasEllipsis ? Notification_scss_1.default.hasEllipsis : null,
    ]);
    const iconStyles = (0, classnames_1.default)([
      Notification_scss_1.default.icon,
      hasSpinner ? Notification_scss_1.default.spinnerIcon : null,
    ]);
    return react_1.default.createElement(
      'div',
      {
        className: notificationMessageStyles,
        onClick: () => isClickToClose && onClose && onClose(),
        role: 'link',
        'aria-hidden': true,
        style: {
          zIndex: 9999999 + (index || 0),
        },
      },
      isVisible &&
        react_1.default.createElement(
          react_1.Fragment,
          null,
          icon &&
            react_1.default.createElement(react_svg_inline_1.default, {
              svg: icon,
              className: iconStyles,
            }),
          react_1.default.createElement(
            'div',
            { className: messageStyles },
            children
          ),
          !!actions &&
            react_1.default.createElement(NotificationActions_1.default, {
              actions: actions,
            }),
          hasCloseButton &&
            react_1.default.createElement(
              'button',
              {
                className: Notification_scss_1.default.closeButton,
                onClick: () => onClose && onClose(),
              },
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: close_cross_inline_svg_1.default,
              })
            )
        )
    );
  }
}
exports.default = Notification;
//# sourceMappingURL=Notification.js.map
