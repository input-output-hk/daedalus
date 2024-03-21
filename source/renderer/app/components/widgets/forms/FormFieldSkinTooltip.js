'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.FormFieldSkin = void 0;
// @ts-nocheck
/* eslint-disable */
const react_1 = __importDefault(require('react'));
const lodash_1 = require('lodash');
const classnames_1 = __importDefault(require('classnames'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const exclamation_point_inline_svg_1 = __importDefault(
  require('../../../assets/images/exclamation-point.inline.svg')
);
const FormFieldSkinTooltip_scss_1 = __importDefault(
  require('./FormFieldSkinTooltip.scss')
);
const PopOver_1 = require('@react-polymorph/components/PopOver');
const FormFieldSkin = (props) =>
  react_1.default.createElement(
    'div',
    {
      className: (0, classnames_1.default)([
        FormFieldSkinTooltip_scss_1.default.component,
        props.className,
        props.theme[props.themeId].root,
        props.disabled ? props.theme[props.themeId].disabled : null,
        props.error ? props.theme[props.themeId].errored : null,
      ]),
    },
    props.label &&
      react_1.default.createElement(
        'label',
        {
          role: 'presentation',
          'aria-hidden': true,
          className: props.theme[props.themeId].label,
          onClick: props.focusChild,
        },
        props.label,
        props.error &&
          react_1.default.createElement(
            PopOver_1.PopOver,
            { content: props.error, key: 'tooltip', placement: 'bottom' },
            react_1.default.createElement(react_svg_inline_1.default, {
              svg: exclamation_point_inline_svg_1.default,
              className:
                FormFieldSkinTooltip_scss_1.default.exclamationPointIcon,
            })
          )
      ),
    react_1.default.createElement(
      'div',
      { className: props.theme[props.themeId].inputWrapper },
      props.render((0, lodash_1.omit)(props, ['themeId']))
    )
  );
exports.FormFieldSkin = FormFieldSkin;
//# sourceMappingURL=FormFieldSkinTooltip.js.map
