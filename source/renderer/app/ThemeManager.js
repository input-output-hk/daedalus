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
Object.defineProperty(exports, '__esModule', { value: true });
/* eslint-disable react/prop-types */
const react_1 = __importStar(require('react'));
const lodash_1 = require('lodash');
class ThemeManager extends react_1.Component {
  componentDidMount() {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'variables' does not exist on type 'Reado... Remove this comment to see the full error message
    this.updateCSSVariables(this.props.variables);
  }
  componentDidUpdate(prevProps) {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'variables' does not exist on type 'Reado... Remove this comment to see the full error message
    if (this.props.variables !== prevProps.variables) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'variables' does not exist on type 'Reado... Remove this comment to see the full error message
      this.updateCSSVariables(this.props.variables);
    }
  }
  updateCSSVariables(variables) {
    const flattenedTheme = this.flattenTheme(variables);
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
    (0, lodash_1.map)(flattenedTheme, (value, prop) => {
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
      document.documentElement.style.setProperty(prop, value);
    });
  }
  flattenTheme(daedalusTheme) {
    return Object.values(daedalusTheme).reduce(
      // @ts-ignore ts-migrate(2698) FIXME: Spread types may only be created from object types... Remove this comment to see the full error message
      (theme, componentVars) => ({ ...theme, ...componentVars }),
      {}
    );
  }
  render() {
    return react_1.default.createElement(
      react_1.Fragment,
      null,
      this.props.children
    );
  }
}
exports.default = ThemeManager;
//# sourceMappingURL=ThemeManager.js.map
