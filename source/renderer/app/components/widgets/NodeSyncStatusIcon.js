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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const react_intl_1 = require('react-intl');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const classnames_1 = __importDefault(require('classnames'));
const formatters_1 = require('../../utils/formatters');
const node_sync_spinner_inline_svg_1 = __importDefault(
  require('../../assets/images/top-bar/node-sync-spinner.inline.svg')
);
const node_sync_synced_inline_svg_1 = __importDefault(
  require('../../assets/images/top-bar/node-sync-synced.inline.svg')
);
const NodeSyncStatusIcon_scss_1 = __importDefault(
  require('./NodeSyncStatusIcon.scss')
);
const timingConfig_1 = require('../../config/timingConfig');
const messages = (0, react_intl_1.defineMessages)({
  blocksSynced: {
    id: 'cardano.node.sync.status.blocksSynced',
    defaultMessage: '!!!Blocks synced {percentage}%',
    description:
      'Label for the blocks synced info overlay on node sync status icon.',
  },
});
class NodeSyncStatusIcon extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { isSynced, syncPercentage, hasTadaIcon } = this.props;
    const { intl } = this.context;
    const statusIcon = isSynced
      ? node_sync_synced_inline_svg_1.default
      : node_sync_spinner_inline_svg_1.default;
    const componentClasses = (0, classnames_1.default)([
      NodeSyncStatusIcon_scss_1.default.component,
      !isSynced && NodeSyncStatusIcon_scss_1.default.syncing,
      hasTadaIcon && NodeSyncStatusIcon_scss_1.default.hasTadaIcon,
    ]);
    const percentage = syncPercentage.toFixed(syncPercentage === 100 ? 0 : 2);
    return react_1.default.createElement(
      'div',
      { className: componentClasses },
      react_1.default.createElement(
        PopOver_1.PopOver,
        {
          delay: timingConfig_1.TOOLTIP_DELAY,
          offset: [0, 10],
          content: intl.formatMessage(messages.blocksSynced, {
            percentage: (0, formatters_1.formattedNumber)(percentage),
          }),
        },
        react_1.default.createElement(
          'div',
          null,
          react_1.default.createElement(react_svg_inline_1.default, {
            className: NodeSyncStatusIcon_scss_1.default.icon,
            svg: statusIcon,
          })
        )
      )
    );
  }
}
exports.default = NodeSyncStatusIcon;
//# sourceMappingURL=NodeSyncStatusIcon.js.map
