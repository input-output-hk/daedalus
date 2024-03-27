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
exports.VirtualAddressesList = void 0;
const react_1 = __importStar(require('react'));
const lodash_1 = require('lodash');
const mobx_react_1 = require('mobx-react');
const react_virtualized_1 = require('react-virtualized');
const VirtualAddressesList_scss_1 = __importDefault(
  require('./VirtualAddressesList.scss')
);
/**
 *
 * The breakpoints define the number of lines
 * based on the width of the following element:
 *
 * `.ReactVirtualized__Grid__innerScrollContainer`
 *
 */
const BREAKPOINT_1_LINE = 1108;
const BREAKPOINT_2_LINES = 635;
const ADDRESS_LINE_HEIGHT = 22;
const ADDRESS_LINE_PADDING = 21;
const ADDRESS_SELECTOR = '.Address';
let VirtualAddressesList = class VirtualAddressesList extends react_1.Component {
  list;
  listWidth = 0;
  addressHeight = 0;
  /**
   * Estimate the address height based on number of lines
   */
  estimateAddressHeight = (lines) =>
    ADDRESS_LINE_HEIGHT * lines + ADDRESS_LINE_PADDING;
  /**
   * Gets the number of lines based on the container width
   */
  getLinesFromWidth = (width) => {
    if (width >= BREAKPOINT_1_LINE) return 1;
    if (width >= BREAKPOINT_2_LINES) return 2;
    return 3;
  };
  /**
   * Virtual row heights only once per tick (debounced)
   */
  updateRowHeights = () => {
    const { list, addressHeight } = this;
    if (!list) return;
    const firstAddress = document.querySelector(ADDRESS_SELECTOR);
    if (firstAddress instanceof HTMLElement) {
      this.addressHeight = firstAddress.offsetHeight;
    } else {
      this.addressHeight = this.estimateAddressHeight(
        this.getLinesFromWidth(this.listWidth)
      );
      // Since we could only estimate the address heights, re-try
      // the update and hope that DOM is rendered then (for exact measurements)
      setTimeout(this.updateRowHeights, 100);
    }
    if (addressHeight !== this.addressHeight) {
      list.recomputeRowHeights(0);
    }
  };
  /**
   * Update row height and recompute virtual rows.
   */
  onResize = ({ width }) => {
    this.listWidth = width;
    this.updateRowHeights();
  };
  rowRenderer = ({
    index,
    // Index of row
    key,
    // Unique key within array of rendered rows
    style, // Style object to be applied to row (to position it);
  }) => {
    const { rows, renderRow } = this.props;
    const address = rows[index];
    return (
      // @ts-ignore ts-migrate(2559) FIXME: Type 'string' has no properties in common with typ... Remove this comment to see the full error message
      react_1.default.createElement(
        'div',
        {
          key: key,
          style: style,
          className: VirtualAddressesList_scss_1.default.address,
        },
        renderRow(address, index)
      )
    );
  };
  render() {
    const { rows } = this.props;
    if (!rows.length) return null;
    return react_1.default.createElement(
      'div',
      { className: VirtualAddressesList_scss_1.default.component },
      react_1.default.createElement(
        react_virtualized_1.AutoSizer,
        { onResize: (0, lodash_1.throttle)(this.onResize, 100) },
        ({ width, height }) =>
          react_1.default.createElement(react_virtualized_1.List, {
            className: VirtualAddressesList_scss_1.default.list,
            ref: (list) => {
              this.list = list;
            },
            width: width,
            height: height,
            rowCount: rows.length,
            rowHeight: () => this.addressHeight,
            rowRenderer: this.rowRenderer,
            style: {
              overflowY: 'scroll',
            },
          })
      )
    );
  }
};
VirtualAddressesList = __decorate(
  [mobx_react_1.observer],
  VirtualAddressesList
);
exports.VirtualAddressesList = VirtualAddressesList;
//# sourceMappingURL=VirtualAddressesList.js.map
