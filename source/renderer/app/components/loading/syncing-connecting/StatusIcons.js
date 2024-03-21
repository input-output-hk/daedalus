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
const react_intl_1 = require('react-intl');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const classnames_1 = __importDefault(require('classnames'));
const StatusIcons_scss_1 = __importDefault(require('./StatusIcons.scss'));
const cardano_node_types_1 = require('../../../../../common/types/cardano-node.types');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/node-st... Remove this comment to see the full error message
const node_state_icon_inline_svg_1 = __importDefault(
  require('../../../assets/images/node-state-icon.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/is-node... Remove this comment to see the full error message
const is_node_responding_icon_inline_svg_1 = __importDefault(
  require('../../../assets/images/is-node-responding-icon.inline.svg')
);
// import isNodeSubscribedIcon from '../../../assets/images/is-node-subscribed-icon.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/is-node... Remove this comment to see the full error message
const is_node_time_correct_icon_inline_svg_1 = __importDefault(
  require('../../../assets/images/is-node-time-correct-icon.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/is-node... Remove this comment to see the full error message
const is_node_syncing_icon_inline_svg_1 = __importDefault(
  require('../../../assets/images/is-node-syncing-icon.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  nodeIsRunning: {
    id: 'status.icons.nodeIsRunning',
    defaultMessage: '!!!Cardano node is running!',
    description: 'Message "Cardano node is running" on the status icon tooltip',
  },
  nodeIsStarting: {
    id: 'status.icons.nodeIsStarting',
    defaultMessage: '!!!Cardano node is starting!',
    description: 'Message "Node is starting" on the status icon tooltip',
  },
  nodeIsExiting: {
    id: 'status.icons.nodeIsExiting',
    defaultMessage: '!!!Cardano node is exiting!',
    description: 'Message "Cardano node is exiting" on the status icon tooltip',
  },
  nodeIsStopping: {
    id: 'status.icons.nodeIsStopping',
    defaultMessage: '!!!Cardano node is stopping!',
    description:
      'Message "Cardano node is stopping" on the status icon tooltip',
  },
  nodeHasStopped: {
    id: 'status.icons.nodeHasStopped',
    defaultMessage: '!!!Cardano node has stopped!',
    description:
      'Message "Cardano node has stopped" on the status icon tooltip',
  },
  nodeIsUpdating: {
    id: 'status.icons.nodeIsUpdating',
    defaultMessage: '!!!Cardano node is updating!',
    description:
      'Message "Cardano node is updating" on the status icon tooltip',
  },
  nodeHasBeenUpdated: {
    id: 'status.icons.nodeHasBeenUpdated',
    defaultMessage: '!!!Cardano node has been updated!',
    description:
      'Message "Cardano node has been updated" on the status icon tooltip',
  },
  nodeHasCrashed: {
    id: 'status.icons.nodeHasCrashed',
    defaultMessage: '!!!Cardano node has crashed!',
    description:
      'Message "Cardano node has crashed" on the status icon tooltip',
  },
  nodeHasErrored: {
    id: 'status.icons.nodeHasErrored',
    defaultMessage: '!!!Cardano node has errored!',
    description:
      'Message "Cardano node has errored" on the status icon tooltip',
  },
  nodeIsUnrecoverable: {
    id: 'status.icons.nodeIsUnrecoverable',
    defaultMessage: '!!!Cardano node is unrecoverable!',
    description:
      'Message "Cardano node is unrecoverable" on the status icon tooltip',
  },
  checkYourInternetConnection: {
    id: 'status.icons.checkYourInternetConnection',
    defaultMessage: '!!!Check your Internet connection!',
    description:
      'Message "Check your Internet connection" on the status icon tooltip',
  },
  isNodeRespondingOn: {
    id: 'status.icons.isNodeRespondingOn',
    defaultMessage: '!!!Cardano node is responding!',
    description:
      'Message "Cardano node is responding" on the status icon tooltip',
  },
  isNodeRespondingOff: {
    id: 'status.icons.isNodeRespondingOff',
    defaultMessage: '!!!Cardano node is not responding!',
    description:
      'Message "Cardano node is not responding" on the status icon tooltip',
  },
  isNodeRespondingLoading: {
    id: 'status.icons.isNodeRespondingLoading',
    defaultMessage: '!!!Checking if Cardano node is responding!',
    description:
      'Message "Checking if Cardano node is responding" on the status icon tooltip',
  },
  isNodeSubscribedOn: {
    id: 'status.icons.isNodeSubscribedOn',
    defaultMessage: '!!!Cardano node is subscribed!',
    description:
      'Message "Cardano node is subscribed" on the status icon tooltip',
  },
  isNodeSubscribedOff: {
    id: 'status.icons.isNodeSubscribedOff',
    defaultMessage: '!!!Cardano node is not subscribed!',
    description:
      'Message "Cardano node is not subscribed" on the status icon tooltip',
  },
  isNodeSubscribedLoading: {
    id: 'status.icons.isNodeSubscribedLoading',
    defaultMessage: '!!!Checking if Cardano node is subscribed!',
    description:
      'Message "Checking if Cardano node is subscribed" on the status icon tooltip',
  },
  isNodeTimeCorrectOn: {
    id: 'status.icons.isNodeTimeCorrectOn',
    defaultMessage: '!!!Cardano node time is correct!',
    description:
      'Message "Cardano node time is correct" on the status icon tooltip',
  },
  isNodeTimeCorrectOff: {
    id: 'status.icons.isNodeTimeCorrectOff',
    defaultMessage: '!!!Cardano node time is not correct!',
    description:
      'Message "Cardano node time is not correct" on the status icon tooltip',
  },
  isNodeTimeCorrectLoading: {
    id: 'status.icons.isNodeTimeCorrectLoading',
    defaultMessage: '!!!Checking if Cardano node time is correct!',
    description:
      'Message "Checking if Cardano node time is correct" on the status icon tooltip',
  },
  isNodeSyncingOn: {
    id: 'status.icons.isNodeSyncingOn',
    defaultMessage: '!!!Cardano node is syncing!',
    description: 'Message "Cardano node is syncing" on the status icon tooltip',
  },
  isNodeSyncingOff: {
    id: 'status.icons.isNodeSyncingOff',
    defaultMessage: '!!!Cardano node is not syncing!',
    description:
      'Message "Cardano node is not syncing" on the status icon tooltip',
  },
  isNodeSyncingLoading: {
    id: 'status.icons.isNodeSyncingLoading',
    defaultMessage: '!!!Checking if Cardano node is syncing!',
    description:
      'Message "Checking if Cardano node is syncing" on the status icon tooltip',
  },
});
const STATUS_CLASSNAMES = {
  [cardano_node_types_1.CardanoNodeStates.STARTING]: 'unloaded',
  [cardano_node_types_1.CardanoNodeStates.RUNNING]: 'on',
  [cardano_node_types_1.CardanoNodeStates.EXITING]: 'unloaded',
  [cardano_node_types_1.CardanoNodeStates.STOPPING]: 'unloaded',
  [cardano_node_types_1.CardanoNodeStates.STOPPED]: 'unloaded',
  [cardano_node_types_1.CardanoNodeStates.UPDATING]: 'unloaded',
  [cardano_node_types_1.CardanoNodeStates.UPDATED]: 'unloaded',
  [cardano_node_types_1.CardanoNodeStates.CRASHED]: 'off',
  [cardano_node_types_1.CardanoNodeStates.ERRORED]: 'off',
  [cardano_node_types_1.CardanoNodeStates.UNRECOVERABLE]: 'off',
  true: 'on',
  false: 'off',
  undefined: 'unloaded',
};
const NODE_STATE_MESSAGES = {
  [cardano_node_types_1.CardanoNodeStates.RUNNING]: messages.nodeIsRunning,
  [cardano_node_types_1.CardanoNodeStates.STARTING]: messages.nodeIsStarting,
  [cardano_node_types_1.CardanoNodeStates.EXITING]: messages.nodeIsExiting,
  [cardano_node_types_1.CardanoNodeStates.STOPPING]: messages.nodeIsStopping,
  [cardano_node_types_1.CardanoNodeStates.STOPPED]: messages.nodeHasStopped,
  [cardano_node_types_1.CardanoNodeStates.UPDATING]: messages.nodeIsUpdating,
  [cardano_node_types_1.CardanoNodeStates.UPDATED]: messages.nodeHasBeenUpdated,
  [cardano_node_types_1.CardanoNodeStates.CRASHED]: messages.nodeHasCrashed,
  [cardano_node_types_1.CardanoNodeStates.ERRORED]: messages.nodeHasErrored,
  [cardano_node_types_1.CardanoNodeStates.UNRECOVERABLE]:
    messages.nodeIsUnrecoverable,
};
const VARIABLE_VALUES = {
  true: 'On',
  false: 'Off',
  undefined: 'Loading',
  null: 'IsStarting',
};
class StatusIcons extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  getTip = (paramName, paramValue) => {
    let message;
    if (paramName === 'nodeState' && paramValue) {
      message = NODE_STATE_MESSAGES[String(paramValue)];
    } else {
      message = messages[`${paramName}${VARIABLE_VALUES[String(paramValue)]}`];
    }
    return (
      message &&
      react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
        ...message,
      })
    );
  };
  getClassName = (paramName) => {
    // If node is not running, it displays the icons with opacity
    // Whether {isNodeSyncing} it displays the icons for syncing or loading screen
    const { isNodeSyncing } = this.props;
    const paramValue = this.props[paramName];
    let status = STATUS_CLASSNAMES[paramValue];
    if (this.isDisabled(paramName)) {
      status = 'unknown';
    }
    const classesMap = {
      on: StatusIcons_scss_1.default.iconOn,
      off: StatusIcons_scss_1.default.iconOff,
      unloaded: StatusIcons_scss_1.default.iconUnloaded,
      unknown: StatusIcons_scss_1.default.iconUnknown,
    };
    return (0, classnames_1.default)([
      StatusIcons_scss_1.default.icon,
      classesMap[status],
      classesMap[paramName],
      isNodeSyncing
        ? StatusIcons_scss_1.default.syncing
        : StatusIcons_scss_1.default.loading,
    ]);
  };
  getTooltipClassname = (paramName) => {
    const paramValue = this.props[paramName];
    return (0, classnames_1.default)([
      StatusIcons_scss_1.default.tooltip,
      typeof paramValue === 'undefined'
        ? StatusIcons_scss_1.default.ellipsis
        : null,
      this.isDisabled(paramName) ? StatusIcons_scss_1.default.disabled : null,
    ]);
  };
  isDisabled = (paramName) =>
    paramName !== 'nodeState' &&
    this.props.nodeState !== cardano_node_types_1.CardanoNodeStates.RUNNING;
  getIconWithPopover = (icon, paramName) =>
    react_1.default.createElement(
      PopOver_1.PopOver,
      {
        themeVariables: {
          '--rp-pop-over-bg-color':
            'var(--theme-loading-status-icons-tooltip-color)',
          '--rp-pop-over-border-radius': '5px',
          '--rp-bubble-padding': '6px 12px 7px',
        },
        contentClassName: this.getTooltipClassname(paramName),
        key: paramName,
        content: this.getTip(paramName, this.props[paramName]),
      },
      react_1.default.createElement(
        'button',
        {
          className: StatusIcons_scss_1.default.iconButton,
          onClick: this.props.onIconClick,
        },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: icon,
          className: this.getClassName(paramName),
        })
      )
    );
  render() {
    return react_1.default.createElement(
      'div',
      { className: StatusIcons_scss_1.default.component },
      [
        this.getIconWithPopover(
          node_state_icon_inline_svg_1.default,
          'nodeState'
        ),
        this.getIconWithPopover(
          is_node_responding_icon_inline_svg_1.default,
          'isNodeResponding'
        ),
        this.getIconWithPopover(
          is_node_time_correct_icon_inline_svg_1.default,
          'isNodeTimeCorrect'
        ),
        this.getIconWithPopover(
          is_node_syncing_icon_inline_svg_1.default,
          'isNodeSyncing'
        ),
      ]
    );
  }
}
exports.default = StatusIcons;
//# sourceMappingURL=StatusIcons.js.map
