'use strict';
// @ts-nocheck
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const lodash_1 = require('lodash');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_intl_1 = require('react-intl');
const SidebarCategory_scss_1 = __importDefault(
  require('./SidebarCategory.scss')
);
const SidebarCategory_messages_1 = require('./SidebarCategory.messages');
const timingConfig_1 = require('../../config/timingConfig');
function SidebarCategory({ category, intl, isActive, onClick, content }) {
  const { name, icon, route, tooltipTextId } = category;
  const className = (0, lodash_1.camelCase)(name);
  const componentStyles = (0, classnames_1.default)(
    SidebarCategory_scss_1.default.component,
    className,
    SidebarCategory_scss_1.default[className],
    isActive && SidebarCategory_scss_1.default.active
  );
  const iconClassName = (0, classnames_1.default)(
    SidebarCategory_scss_1.default.icon,
    SidebarCategory_scss_1.default[`${className}Icon`]
  );
  return react_1.default.createElement(
    PopOver_1.PopOver,
    {
      delay: timingConfig_1.TOOLTIP_DELAY,
      offset: [0, -20],
      content:
        tooltipTextId &&
        intl.formatMessage(SidebarCategory_messages_1.messages[tooltipTextId]),
      placement: 'bottom',
    },
    react_1.default.createElement(
      'button',
      { className: componentStyles, onClick: () => onClick(route) },
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: icon,
        className: iconClassName,
      }),
      content
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(SidebarCategory)
);
//# sourceMappingURL=SidebarCategory.js.map
