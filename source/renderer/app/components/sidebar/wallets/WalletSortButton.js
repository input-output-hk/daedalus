'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.WalletSortButton = void 0;
const react_1 = __importDefault(require('react'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const Button_1 = require('@react-polymorph/components/Button');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const sidebarTypes_1 = require('../../../types/sidebarTypes');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/sort-ar... Remove this comment to see the full error message
const sort_arrow_inline_svg_1 = __importDefault(
  require('../../../assets/images/sort-arrow.inline.svg')
);
const WalletSortButton_scss_1 = __importDefault(
  require('./WalletSortButton.scss')
);
function WalletSortButton({ onClick, label, isActive, sortOrder, tooltip }) {
  const walletSortButtonStyles = (0, classnames_1.default)([
    WalletSortButton_scss_1.default.walletSortButton,
    isActive ? WalletSortButton_scss_1.default.walletSortButtonActive : null,
  ]);
  const walletSortOrderArrowStyles = (0, classnames_1.default)([
    WalletSortButton_scss_1.default.walletSortOrderArrowContainer,
    sortOrder === sidebarTypes_1.WalletSortOrder.Asc
      ? WalletSortButton_scss_1.default.walletSortOrderArrowAsc
      : null,
  ]);
  return react_1.default.createElement(
    PopOver_1.PopOver,
    { content: tooltip },
    react_1.default.createElement(
      'div',
      { className: WalletSortButton_scss_1.default.walletSortButtonContainer },
      react_1.default.createElement(Button_1.Button, {
        className: walletSortButtonStyles,
        onClick: onClick,
        label: react_1.default.createElement(
          react_1.default.Fragment,
          null,
          isActive
            ? react_1.default.createElement(
                'div',
                { className: walletSortOrderArrowStyles },
                react_1.default.createElement(react_svg_inline_1.default, {
                  svg: sort_arrow_inline_svg_1.default,
                  className:
                    WalletSortButton_scss_1.default.walletSortOrderArrow,
                })
              )
            : null,
          label
        ),
      })
    )
  );
}
exports.WalletSortButton = WalletSortButton;
//# sourceMappingURL=WalletSortButton.js.map
