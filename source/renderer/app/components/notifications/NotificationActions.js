'use strict';
// @ts-nocheck
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const lodash_1 = require('lodash');
const Button_1 = require('@react-polymorph/components/Button');
const Link_1 = require('@react-polymorph/components/Link');
const mobx_react_1 = require('mobx-react');
const NotificationActions_scss_1 = __importDefault(
  require('./NotificationActions.scss')
);
const NotificationActions = (0, mobx_react_1.observer)(({ actions }) => {
  const componentStyles = (0, classnames_1.default)([
    NotificationActions_scss_1.default.component,
  ]);
  return react_1.default.createElement(
    'div',
    { className: componentStyles },
    (0, lodash_1.map)(
      actions,
      (
        {
          className,
          label,
          primary,
          disabled,
          onClick,
          isLink,
          hasIconAfter,
          hasIconBefore,
          autoFocus,
        },
        key
      ) => {
        const buttonClasses = (0, classnames_1.default)([
          NotificationActions_scss_1.default.button,
          className,
          primary ? 'primary' : 'flat',
          primary
            ? NotificationActions_scss_1.default.primaryButton
            : NotificationActions_scss_1.default.secondaryButton,
        ]);
        const isAutoFocus = (primary && autoFocus) !== false || !!autoFocus;
        return !isLink
          ? react_1.default.createElement(Button_1.Button, {
              key: key,
              className: buttonClasses,
              label: label,
              onClick: onClick,
              disabled: disabled,
              autoFocus: isAutoFocus,
            })
          : react_1.default.createElement(Link_1.Link, {
              key: key,
              className: className,
              onClick: onClick,
              label: label,
              hasIconAfter: hasIconAfter,
              hasIconBefore: hasIconBefore,
            });
      }
    )
  );
});
exports.default = NotificationActions;
//# sourceMappingURL=NotificationActions.js.map
