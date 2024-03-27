'use strict';
// @ts-nocheck
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
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const mobx_react_1 = require('mobx-react');
const lodash_1 = require('lodash');
const react_intl_1 = require('react-intl');
const react_copy_to_clipboard_1 = __importDefault(
  require('react-copy-to-clipboard')
);
const PopOver_1 = require('@react-polymorph/components/PopOver');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const timingConfig_1 = require('../../config/timingConfig');
const global_messages_1 = __importDefault(
  require('../../i18n/global-messages')
);
const DialogCloseButton_1 = __importDefault(
  require('../widgets/DialogCloseButton')
);
const close_cross_thin_inline_svg_1 = __importDefault(
  require('../../assets/images/close-cross-thin.inline.svg')
);
const clipboard_ic_inline_svg_1 = __importDefault(
  require('../../assets/images/clipboard-ic.inline.svg')
);
const sand_clock_xs_inline_svg_1 = __importDefault(
  require('../../assets/images/sand-clock-xs.inline.svg')
);
const formatters_1 = require('../../utils/formatters');
const cardano_node_types_1 = require('../../../../common/types/cardano-node.types');
const DaedalusDiagnostics_scss_1 = __importDefault(
  require('./DaedalusDiagnostics.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  systemInfo: {
    id: 'daedalus.diagnostics.dialog.system.info',
    defaultMessage: '!!!SYSTEM INFO',
    description: 'System info',
  },
  platform: {
    id: 'daedalus.diagnostics.dialog.platform',
    defaultMessage: '!!!Platform',
    description: 'Platform',
  },
  platformVersion: {
    id: 'daedalus.diagnostics.dialog.platform.version',
    defaultMessage: '!!!Platform version',
    description: 'Platform version',
  },
  cpu: {
    id: 'daedalus.diagnostics.dialog.cpu',
    defaultMessage: '!!!CPU',
    description: 'CPU',
  },
  ram: {
    id: 'daedalus.diagnostics.dialog.ram',
    defaultMessage: '!!!RAM',
    description: 'RAM',
  },
  availableDiskSpace: {
    id: 'daedalus.diagnostics.dialog.availableDiskSpace',
    defaultMessage: '!!!Available disk space',
    description: 'Available disk space',
  },
  unknownDiskSpace: {
    id: 'daedalus.diagnostics.dialog.unknownDiskSpace',
    defaultMessage: '!!!Unknown',
    description: 'Unknown amount of disk space',
  },
  unknownDiskSpaceSupportUrl: {
    id: 'daedalus.diagnostics.dialog.unknownDiskSpaceSupportUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc',
    description: '"Support" link URL while disk space is unknown',
  },
  hasMetHardwareRequirementsLabel: {
    id: 'daedalus.diagnostics.dialog.hasMetHardwareRequirementsStatus',
    defaultMessage: '!!!Recommended system requirements status',
    description:
      'Displayed on the left of the Recommended system requirements status row',
  },
  hasMetHardwareRequirementsStatusLowValue: {
    id: 'daedalus.diagnostics.dialog.hasMetHardwareRequirementsStatusLowValue',
    defaultMessage: '!!!Low',
    description:
      'Displayed on the right of the Recommended system requirements status row when hardware requirements are insufficient',
  },
  hasMetHardwareRequirementsStatusGoodValue: {
    id: 'daedalus.diagnostics.dialog.hasMetHardwareRequirementsStatusGoodValue',
    defaultMessage: '!!!Good',
    description:
      'Displayed on the right of the Recommended system requirements status row when hardware requirements are ok',
  },
  hasMetHardwareRequirementsStatusLowTooltip: {
    id:
      'daedalus.diagnostics.dialog.hasMetHardwareRequirementsStatusLowTooltip',
    defaultMessage:
      '!!!Your system specifications do not meet Daedalus’ recommended hardware requirements. We suggest using a machine with at least 16 GB of RAM',
    description:
      'Visible on hovering over Recommended system requirement status when status is Low',
  },
  hasMetHardwareRequirementsStatusGoodTooltip: {
    id:
      'daedalus.diagnostics.dialog.hasMetHardwareRequirementsStatusGoodTooltip',
    defaultMessage:
      '!!!Your system specifications meet Daedalus’ recommended hardware requirements',
    description:
      'Visible on hovering over Recommended system requirement status when status is Good',
  },
  isRTSFlagsModeEnabled: {
    id: 'daedalus.diagnostics.dialog.isRTSFlagsModeEnabled',
    defaultMessage: '!!!RTS Flags Mode',
    description: 'Indicates whether RTS Flags Mode is enabled or not',
  },
  coreInfo: {
    id: 'daedalus.diagnostics.dialog.coreInfo',
    defaultMessage: '!!!CORE INFO',
    description: 'CORE INFO',
  },
  daedalusVersion: {
    id: 'daedalus.diagnostics.dialog.daedalusVersion',
    defaultMessage: '!!!Daedalus version',
    description: 'Daedalus version',
  },
  daedalusBuildNumber: {
    id: 'daedalus.diagnostics.dialog.daedalusBuildNumber',
    defaultMessage: '!!!Daedalus build number',
    description: 'Daedalus build number',
  },
  daedalusMainProcessID: {
    id: 'daedalus.diagnostics.dialog.daedalusMainProcessID',
    defaultMessage: '!!!Daedalus main process ID',
    description: 'Daedalus main process ID',
  },
  daedalusProcessID: {
    id: 'daedalus.diagnostics.dialog.daedalusProcessID',
    defaultMessage: '!!!Daedalus renderer process ID',
    description: 'Daedalus renderer process ID',
  },
  blankScreenFix: {
    id: 'daedalus.diagnostics.dialog.blankScreenFix',
    defaultMessage: "!!!Daedalus 'Blank Screen Fix' active",
    description: "Daedalus 'Blank Screen Fix' active",
  },
  cardanoNodeVersion: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeVersion',
    defaultMessage: '!!!Cardano node version',
    description: 'Cardano node version',
  },
  cardanoNodePID: {
    id: 'daedalus.diagnostics.dialog.cardanoNodePID',
    defaultMessage: '!!!Cardano node process ID',
    description: 'Cardano node process ID',
  },
  cardanoNodeApiPort: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeApiPort',
    defaultMessage: '!!!Cardano node port',
    description: 'Cardano node port',
  },
  cardanoWalletPID: {
    id: 'daedalus.diagnostics.dialog.cardanoWalletPID',
    defaultMessage: '!!!Cardano wallet process ID',
    description: 'Cardano wallet process ID',
  },
  cardanoWalletVersion: {
    id: 'daedalus.diagnostics.dialog.cardanoWalletVersion',
    defaultMessage: '!!!Cardano wallet version',
    description: 'Cardano wallet version',
  },
  cardanoWalletApiPort: {
    id: 'daedalus.diagnostics.dialog.cardanoWalletApiPort',
    defaultMessage: '!!!Cardano wallet port',
    description: 'Cardano wallet port',
  },
  cardanoNetwork: {
    id: 'daedalus.diagnostics.dialog.cardanoNetwork',
    defaultMessage: '!!!Cardano network',
    description: 'Cardano network',
  },
  stateDirectoryPath: {
    id: 'daedalus.diagnostics.dialog.stateDirectory',
    defaultMessage: '!!!Daedalus state directory',
    description: 'Daedalus state directory',
  },
  stateDirectoryPathOpenBtn: {
    id: 'daedalus.diagnostics.dialog.stateDirectoryPathOpenBtn',
    defaultMessage: '!!!Open',
    description: 'Open',
  },
  connectionError: {
    id: 'daedalus.diagnostics.dialog.connectionError',
    defaultMessage: '!!!CONNECTION ERROR',
    description: 'CONNECTION ERROR',
  },
  daedalusStatus: {
    id: 'daedalus.diagnostics.dialog.daedalusStatus',
    defaultMessage: '!!!DAEDALUS STATUS',
    description: 'DAEDALUS STATUS',
  },
  connected: {
    id: 'daedalus.diagnostics.dialog.connected',
    defaultMessage: '!!!Connected',
    description: 'Connected',
  },
  synced: {
    id: 'daedalus.diagnostics.dialog.synced',
    defaultMessage: '!!!Synced',
    description: 'Synced',
  },
  syncPercentage: {
    id: 'daedalus.diagnostics.dialog.syncPercentage',
    defaultMessage: '!!!Sync percentage',
    description: 'Sync percentage',
  },
  localTimeDifference: {
    id: 'daedalus.diagnostics.dialog.localTimeDifference',
    defaultMessage: '!!!Local time difference',
    description: 'Local time difference',
  },
  systemTimeCorrect: {
    id: 'daedalus.diagnostics.dialog.systemTimeCorrect',
    defaultMessage: '!!!System time correct',
    description: 'System time correct',
  },
  systemTimeIgnored: {
    id: 'daedalus.diagnostics.dialog.systemTimeIgnored',
    defaultMessage: '!!!System time ignored',
    description: 'System time ignored',
  },
  checkingNodeTime: {
    id: 'daedalus.diagnostics.dialog.checkingNodeTime',
    defaultMessage: '!!!Checking system time',
    description: 'Checking system time',
  },
  cardanoNodeStatus: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeStatus',
    defaultMessage: '!!!CARDANO NODE STATUS',
    description: 'CARDANO NODE STATUS',
  },
  cardanoNodeStatusRestarting: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeStatusRestarting',
    defaultMessage: '!!!Restarting Cardano node...',
    description: 'Restarting Cardano node...',
  },
  cardanoNodeStatusRestart: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeStatusRestart',
    defaultMessage: '!!!Restart Cardano node',
    description: 'Restart Cardano node',
  },
  cardanoNodeState: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeState',
    defaultMessage: '!!!Cardano node state',
    description: 'Cardano node state',
  },
  nodeHasBeenUpdated: {
    id: 'daedalus.diagnostics.dialog.nodeHasBeenUpdated',
    defaultMessage: '!!!Updated',
    description: 'Updated',
  },
  nodeHasCrashed: {
    id: 'daedalus.diagnostics.dialog.nodeHasCrashed',
    defaultMessage: '!!!Crashed',
    description: 'Crashed',
  },
  nodeHasErrored: {
    id: 'daedalus.diagnostics.dialog.nodeHasErrored',
    defaultMessage: '!!!Errored',
    description: 'Errored',
  },
  nodeHasStopped: {
    id: 'daedalus.diagnostics.dialog.nodeHasStopped',
    defaultMessage: '!!!Stopped',
    description: 'Stopped',
  },
  nodeIsExiting: {
    id: 'daedalus.diagnostics.dialog.nodeIsExiting',
    defaultMessage: '!!!Exiting',
    description: 'Exiting',
  },
  nodeIsRunning: {
    id: 'daedalus.diagnostics.dialog.nodeIsRunning',
    defaultMessage: '!!!Running',
    description: 'Running',
  },
  nodeIsStarting: {
    id: 'daedalus.diagnostics.dialog.nodeIsStarting',
    defaultMessage: '!!!Starting',
    description: 'Starting',
  },
  nodeIsStopping: {
    id: 'daedalus.diagnostics.dialog.nodeIsStopping',
    defaultMessage: '!!!Stopping',
    description: 'Stopping',
  },
  nodeIsUnrecoverable: {
    id: 'daedalus.diagnostics.dialog.nodeIsUnrecoverable',
    defaultMessage: '!!!Unrecoverable',
    description: 'Unrecoverable',
  },
  nodeIsUpdating: {
    id: 'daedalus.diagnostics.dialog.nodeIsUpdating',
    defaultMessage: '!!!Updating',
    description: 'Updating',
  },
  cardanoNodeResponding: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeResponding',
    defaultMessage: '!!!Cardano node responding',
    description: 'Cardano node responding',
  },
  cardanoNodeSubscribed: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeSubscribed',
    defaultMessage: '!!!Cardano node subscribed',
    description: 'Cardano node subscribed',
  },
  cardanoNodeTimeCorrect: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeTimeCorrect',
    defaultMessage: '!!!Cardano node time correct',
    description: 'Cardano node time correct',
  },
  cardanoNodeSyncing: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeSyncing',
    defaultMessage: '!!!Cardano node syncing',
    description: 'Cardano node syncing',
  },
  cardanoNodeInSync: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeInSync',
    defaultMessage: '!!!Cardano node in sync',
    description: 'Cardano node in sync',
  },
  localTimeDifferenceChecking: {
    id: 'daedalus.diagnostics.dialog.localTimeDifferenceChecking',
    defaultMessage: '!!!Checking...',
    description: 'Checking...',
  },
  localTimeDifferenceCheckTime: {
    id: 'daedalus.diagnostics.dialog.localTimeDifferenceCheckTime',
    defaultMessage: '!!!Check time',
    description: 'Check time',
  },
  statusOn: {
    id: 'daedalus.diagnostics.dialog.statusOn',
    defaultMessage: '!!!Yes',
    description: 'Yes',
  },
  statusOff: {
    id: 'daedalus.diagnostics.dialog.statusOff',
    defaultMessage: '!!!No',
    description: 'No',
  },
  statusOnForUserSettings: {
    id: 'daedalus.diagnostics.dialog.statusOnForUserSettings',
    defaultMessage: '!!!On',
    description: 'On',
  },
  statusOffForUserSettings: {
    id: 'daedalus.diagnostics.dialog.statusOffForUserSettings',
    defaultMessage: '!!!Off',
    description: 'Off',
  },
  serviceUnreachable: {
    id: 'daedalus.diagnostics.dialog.serviceUnreachable',
    defaultMessage: '!!!NTP service unreachable',
    description: 'NTP service unreachable',
  },
  message: {
    id: 'daedalus.diagnostics.dialog.message',
    defaultMessage: '!!!message',
    description: 'message',
  },
  code: {
    id: 'daedalus.diagnostics.dialog.code',
    defaultMessage: '!!!code',
    description: 'code',
  },
  lastNetworkBlock: {
    id: 'daedalus.diagnostics.dialog.lastNetworkBlock',
    defaultMessage: '!!!Last network block',
    description: 'Last network block',
  },
  lastSynchronizedBlock: {
    id: 'daedalus.diagnostics.dialog.lastSynchronizedBlock',
    defaultMessage: '!!!Last synchronized block',
    description: 'Last synchronized block',
  },
  epoch: {
    id: 'daedalus.diagnostics.dialog.epoch',
    defaultMessage: '!!!epoch',
    description: 'epoch',
  },
  slot: {
    id: 'daedalus.diagnostics.dialog.slot',
    defaultMessage: '!!!slot',
    description: 'slot',
  },
});
const FINAL_CARDANO_NODE_STATES = [
  cardano_node_types_1.CardanoNodeStates.RUNNING,
  cardano_node_types_1.CardanoNodeStates.UPDATED,
  cardano_node_types_1.CardanoNodeStates.CRASHED,
  cardano_node_types_1.CardanoNodeStates.ERRORED,
  cardano_node_types_1.CardanoNodeStates.UNRECOVERABLE,
];
let DaedalusDiagnostics = class DaedalusDiagnostics extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  constructor(props) {
    super(props);
    this.state = {
      isNodeRestarting: false,
    };
  }
  componentDidUpdate(prevProps) {
    const { cardanoNodeState: prevCardanoNodeState } = prevProps;
    const { cardanoNodeState } = this.props;
    if (
      cardanoNodeState !== prevCardanoNodeState &&
      (0, lodash_1.includes)(FINAL_CARDANO_NODE_STATES, cardanoNodeState)
    ) {
      this.setState({
        isNodeRestarting: false,
      }); // eslint-disable-line
    }
  }
  getSectionRow = (messageId, content) => {
    return react_1.default.createElement(
      'div',
      { className: DaedalusDiagnostics_scss_1.default.layoutRow },
      react_1.default.createElement(
        'div',
        { className: DaedalusDiagnostics_scss_1.default.sectionTitle },
        react_1.default.createElement(
          'span',
          null,
          this.context.intl.formatMessage(messages[messageId])
        ),
        content,
        react_1.default.createElement('hr', null)
      )
    );
  };
  getRow = (messageId, value) => {
    const { intl } = this.context;
    const key = intl.formatMessage(messages[messageId]);
    const colon = intl.formatMessage(
      global_messages_1.default.punctuationColon
    );
    let content = value;
    let className = (0, classnames_1.default)([
      DaedalusDiagnostics_scss_1.default[messageId],
      DaedalusDiagnostics_scss_1.default.layoutData,
    ]);
    const classNameHeader = (0, classnames_1.default)([
      DaedalusDiagnostics_scss_1.default[messageId],
      DaedalusDiagnostics_scss_1.default.layoutHeader,
    ]);
    const classNameRow = (0, classnames_1.default)([
      DaedalusDiagnostics_scss_1.default.layoutRow,
      messageId,
    ]);
    if (typeof value === 'boolean') {
      content = value
        ? intl.formatMessage(messages.statusOn)
        : intl.formatMessage(messages.statusOff);
      className =
        (value && messageId !== 'systemTimeIgnored') ||
        (!value && messageId === 'systemTimeIgnored')
          ? (0, classnames_1.default)([
              className,
              DaedalusDiagnostics_scss_1.default.green,
            ])
          : (0, classnames_1.default)([
              className,
              DaedalusDiagnostics_scss_1.default.red,
            ]);
    }
    return react_1.default.createElement(
      'div',
      { className: classNameRow },
      react_1.default.createElement(
        'div',
        { className: classNameHeader },
        key,
        colon
      ),
      react_1.default.createElement('div', { className: className }, content)
    );
  };
  render() {
    const { intl } = this.context;
    const {
      systemInfo,
      coreInfo,
      cardanoNodeState,
      isNodeResponding,
      // isNodeSubscribed,
      isNodeSyncing,
      isNodeInSync,
      isNodeTimeCorrect,
      isConnected,
      isSynced,
      syncPercentage,
      localTimeDifference,
      isSystemTimeCorrect,
      isSystemTimeIgnored,
      localTip,
      networkTip,
      onOpenStateDirectory,
      onClose,
      onCopyStateDirectoryPath,
      nodeConnectionError,
      onOpenExternalLink,
      isCheckingSystemTime,
      isForceCheckingSystemTime,
    } = this.props;
    const {
      platform,
      platformVersion,
      cpu,
      ram,
      availableDiskSpace: availableDiskSpaceInOriginalFormat,
      hasMetHardwareRequirements,
      isRTSFlagsModeEnabled,
    } = systemInfo;
    const availableDiskSpace = (0, formatters_1.formattedSize)(
      availableDiskSpaceInOriginalFormat
    );
    const {
      daedalusVersion,
      daedalusBuildNumber,
      daedalusProcessID,
      daedalusMainProcessID,
      isBlankScreenFixActive,
      cardanoNodeVersion,
      cardanoNodePID,
      cardanoWalletVersion,
      cardanoWalletPID,
      cardanoWalletApiPort,
      cardanoNetwork,
      daedalusStateDirectoryPath,
    } = coreInfo;
    const { isNodeRestarting } = this.state;
    const isNTPServiceReachable = localTimeDifference != null;
    const connectionError = (0, lodash_1.get)(
      nodeConnectionError,
      'values',
      '{}'
    );
    const { message, code } = connectionError;
    const unknownDiskSpaceSupportUrl = intl.formatMessage(
      messages.unknownDiskSpaceSupportUrl
    );
    const cardanoNetworkValue = intl.formatMessage(
      global_messages_1.default[`network_${cardanoNetwork}`]
    );
    const localTimeDifferenceClasses = isCheckingSystemTime
      ? (0, classnames_1.default)([
          DaedalusDiagnostics_scss_1.default.layoutData,
          DaedalusDiagnostics_scss_1.default.localTimeDifference,
        ])
      : (0, classnames_1.default)([
          DaedalusDiagnostics_scss_1.default.layoutData,
          DaedalusDiagnostics_scss_1.default.localTimeDifference,
          !isNTPServiceReachable ||
          (localTimeDifference &&
            Math.abs(localTimeDifference) >
              timingConfig_1.ALLOWED_TIME_DIFFERENCE)
            ? DaedalusDiagnostics_scss_1.default.red
            : DaedalusDiagnostics_scss_1.default.green,
        ]);
    const { getSectionRow, getRow } = this;
    return react_1.default.createElement(
      'div',
      { className: DaedalusDiagnostics_scss_1.default.component },
      react_1.default.createElement(DialogCloseButton_1.default, {
        className: DaedalusDiagnostics_scss_1.default.closeButton,
        icon: close_cross_thin_inline_svg_1.default,
        onClose: onClose,
      }),
      react_1.default.createElement(
        'div',
        { className: DaedalusDiagnostics_scss_1.default.tables },
        react_1.default.createElement(
          'div',
          { className: DaedalusDiagnostics_scss_1.default.table },
          react_1.default.createElement(
            'div',
            null,
            getSectionRow('systemInfo'),
            getRow('platform', platform),
            getRow('platformVersion', platformVersion),
            getRow(
              'cpu',
              react_1.default.createElement(
                PopOver_1.PopOver,
                { content: cpu },
                cpu
              )
            ),
            getRow('ram', ram),
            getRow(
              'availableDiskSpace',
              availableDiskSpace ||
                react_1.default.createElement(Link_1.Link, {
                  onClick: () => onOpenExternalLink(unknownDiskSpaceSupportUrl),
                  label: intl.formatMessage(messages.unknownDiskSpace),
                  skin: LinkSkin_1.LinkSkin,
                })
            ),
            getRow(
              'hasMetHardwareRequirementsLabel',
              react_1.default.createElement(
                PopOver_1.PopOver,
                {
                  content: intl.formatMessage(
                    hasMetHardwareRequirements
                      ? messages.hasMetHardwareRequirementsStatusGoodTooltip
                      : messages.hasMetHardwareRequirementsStatusLowTooltip
                  ),
                },
                react_1.default.createElement(
                  'div',
                  {
                    className: (0, classnames_1.default)(
                      DaedalusDiagnostics_scss_1.default.layoutData,
                      hasMetHardwareRequirements
                        ? DaedalusDiagnostics_scss_1.default.green
                        : DaedalusDiagnostics_scss_1.default.red
                    ),
                  },
                  intl.formatMessage(
                    hasMetHardwareRequirements
                      ? messages.hasMetHardwareRequirementsStatusGoodValue
                      : messages.hasMetHardwareRequirementsStatusLowValue
                  )
                )
              )
            ),
            getRow(
              'isRTSFlagsModeEnabled',
              intl.formatMessage(
                isRTSFlagsModeEnabled
                  ? messages.statusOnForUserSettings
                  : messages.statusOffForUserSettings
              )
            )
          ),
          react_1.default.createElement(
            'div',
            null,
            getSectionRow('coreInfo'),
            getRow('daedalusVersion', daedalusVersion),
            getRow('daedalusBuildNumber', daedalusBuildNumber),
            getRow('daedalusMainProcessID', daedalusMainProcessID),
            getRow('daedalusProcessID', daedalusProcessID),
            getRow(
              'blankScreenFix',
              isBlankScreenFixActive
                ? intl.formatMessage(messages.statusOnForUserSettings)
                : intl.formatMessage(messages.statusOffForUserSettings)
            ),
            getRow(
              'stateDirectoryPath',
              react_1.default.createElement(
                react_1.Fragment,
                null,
                react_1.default.createElement(
                  'button',
                  {
                    className:
                      DaedalusDiagnostics_scss_1.default.stateDirectoryOpenBtn,
                    onClick: () =>
                      onOpenStateDirectory(daedalusStateDirectoryPath),
                  },
                  intl.formatMessage(messages.stateDirectoryPathOpenBtn)
                ),
                react_1.default.createElement(
                  react_copy_to_clipboard_1.default,
                  {
                    text: daedalusStateDirectoryPath,
                    onCopy: onCopyStateDirectoryPath,
                  },
                  react_1.default.createElement(
                    'div',
                    {
                      className:
                        DaedalusDiagnostics_scss_1.default.stateDirectoryPath,
                    },
                    react_1.default.createElement(
                      PopOver_1.PopOver,
                      {
                        maxWidth: 400,
                        content: react_1.default.createElement(
                          'div',
                          {
                            className:
                              DaedalusDiagnostics_scss_1.default
                                .tooltipLabelWrapper,
                          },
                          react_1.default.createElement(
                            'div',
                            null,
                            daedalusStateDirectoryPath
                          )
                        ),
                      },
                      react_1.default.createElement(
                        'div',
                        {
                          className:
                            DaedalusDiagnostics_scss_1.default
                              .daedalusStateDirectoryPath,
                        },
                        daedalusStateDirectoryPath
                      ),
                      react_1.default.createElement(
                        react_svg_inline_1.default,
                        { svg: clipboard_ic_inline_svg_1.default }
                      )
                    )
                  )
                )
              )
            ),
            getRow('cardanoNodeVersion', cardanoNodeVersion),
            getRow('cardanoNodePID', cardanoNodePID || '-'),
            getRow('cardanoWalletVersion', cardanoWalletVersion),
            getRow('cardanoWalletPID', cardanoWalletPID || '-'),
            getRow('cardanoWalletApiPort', cardanoWalletApiPort || '-')
          ),
          isConnected && nodeConnectionError
            ? react_1.default.createElement(
                'div',
                null,
                getSectionRow('connectionError'),
                react_1.default.createElement(
                  'div',
                  { className: DaedalusDiagnostics_scss_1.default.layoutRow },
                  react_1.default.createElement(
                    'div',
                    {
                      className:
                        DaedalusDiagnostics_scss_1.default.layoutHeader,
                    },
                    react_1.default.createElement(
                      'div',
                      { className: DaedalusDiagnostics_scss_1.default.error },
                      intl.formatMessage(messages.message),
                      ': ',
                      message || '-',
                      react_1.default.createElement('br', null),
                      intl.formatMessage(messages.code),
                      ': ',
                      code || '-'
                    )
                  )
                )
              )
            : null
        ),
        react_1.default.createElement(
          'div',
          { className: DaedalusDiagnostics_scss_1.default.table },
          react_1.default.createElement(
            'div',
            null,
            getSectionRow('daedalusStatus'),
            getRow('cardanoNetwork', cardanoNetworkValue),
            getRow('connected', isConnected),
            getRow('synced', isSynced),
            getRow(
              'syncPercentage',
              `${(0, formatters_1.formattedNumber)(syncPercentage, 2)}%`
            ),
            getRow(
              'lastNetworkBlock',
              react_1.default.createElement(
                react_1.Fragment,
                null,
                react_1.default.createElement(
                  'span',
                  null,
                  intl.formatMessage(messages.epoch),
                  ':'
                ),
                ' ',
                networkTip && networkTip.epoch
                  ? (0, formatters_1.formattedNumber)(networkTip.epoch)
                  : react_1.default.createElement(react_svg_inline_1.default, {
                      svg: sand_clock_xs_inline_svg_1.default,
                      className:
                        DaedalusDiagnostics_scss_1.default.networkTipSandClock,
                    }),
                react_1.default.createElement(
                  'span',
                  null,
                  intl.formatMessage(messages.slot),
                  ':'
                ),
                ' ',
                networkTip && networkTip.slot
                  ? (0, formatters_1.formattedNumber)(networkTip.slot)
                  : react_1.default.createElement(react_svg_inline_1.default, {
                      svg: sand_clock_xs_inline_svg_1.default,
                      className:
                        DaedalusDiagnostics_scss_1.default.networkTipSandClock,
                    })
              )
            ),
            getRow(
              'lastSynchronizedBlock',
              react_1.default.createElement(
                react_1.Fragment,
                null,
                react_1.default.createElement(
                  'span',
                  null,
                  intl.formatMessage(messages.epoch),
                  ':'
                ),
                ' ',
                localTip && localTip.epoch
                  ? (0, formatters_1.formattedNumber)(localTip.epoch)
                  : react_1.default.createElement(react_svg_inline_1.default, {
                      svg: sand_clock_xs_inline_svg_1.default,
                      className:
                        DaedalusDiagnostics_scss_1.default.networkTipSandClock,
                    }),
                react_1.default.createElement(
                  'span',
                  null,
                  intl.formatMessage(messages.slot),
                  ':'
                ),
                ' ',
                localTip && localTip.slot
                  ? (0, formatters_1.formattedNumber)(localTip.slot)
                  : react_1.default.createElement(react_svg_inline_1.default, {
                      svg: sand_clock_xs_inline_svg_1.default,
                      className:
                        DaedalusDiagnostics_scss_1.default.networkTipSandClock,
                    })
              )
            ),
            react_1.default.createElement(
              'div',
              { className: DaedalusDiagnostics_scss_1.default.layoutRow },
              react_1.default.createElement(
                'div',
                { className: DaedalusDiagnostics_scss_1.default.layoutHeader },
                intl.formatMessage(messages.localTimeDifference),
                intl.formatMessage(global_messages_1.default.punctuationColon)
              ),
              react_1.default.createElement(
                'div',
                { className: localTimeDifferenceClasses },
                react_1.default.createElement(
                  'button',
                  {
                    onClick: () => this.checkTime(),
                    disabled: isForceCheckingSystemTime || !isNodeResponding,
                  },
                  isForceCheckingSystemTime
                    ? intl.formatMessage(messages.localTimeDifferenceChecking)
                    : intl.formatMessage(messages.localTimeDifferenceCheckTime)
                ),
                isCheckingSystemTime
                  ? react_1.default.createElement(
                      'span',
                      { className: localTimeDifferenceClasses },
                      react_1.default.createElement(
                        react_svg_inline_1.default,
                        {
                          svg: sand_clock_xs_inline_svg_1.default,
                          className:
                            DaedalusDiagnostics_scss_1.default
                              .networkTipSandClock,
                        }
                      )
                    )
                  : react_1.default.createElement(
                      'span',
                      { className: localTimeDifferenceClasses },
                      isNTPServiceReachable
                        ? `${(0, formatters_1.formattedNumber)(
                            localTimeDifference || 0
                          )} μs`
                        : intl.formatMessage(messages.serviceUnreachable)
                    )
              )
            ),
            getRow('systemTimeCorrect', isSystemTimeCorrect),
            getRow('systemTimeIgnored', isSystemTimeIgnored),
            react_1.default.createElement(
              'div',
              { className: DaedalusDiagnostics_scss_1.default.layoutRow },
              react_1.default.createElement(
                'div',
                { className: DaedalusDiagnostics_scss_1.default.layoutHeader },
                intl.formatMessage(messages.checkingNodeTime),
                intl.formatMessage(global_messages_1.default.punctuationColon)
              ),
              react_1.default.createElement(
                'div',
                { className: DaedalusDiagnostics_scss_1.default.layoutData },
                isCheckingSystemTime
                  ? intl.formatMessage(messages.statusOn)
                  : intl.formatMessage(messages.statusOff)
              )
            )
          ),
          react_1.default.createElement(
            'div',
            null,
            getSectionRow(
              'cardanoNodeStatus',
              react_1.default.createElement(
                'button',
                {
                  className:
                    DaedalusDiagnostics_scss_1.default.cardanoNodeStatusBtn,
                  onClick: () => this.restartNode(),
                  disabled: !(0, lodash_1.includes)(
                    FINAL_CARDANO_NODE_STATES,
                    cardanoNodeState
                  ),
                },
                isNodeRestarting
                  ? intl.formatMessage(messages.cardanoNodeStatusRestarting)
                  : intl.formatMessage(messages.cardanoNodeStatusRestart)
              )
            ),
            getRow(
              'cardanoNodeState',
              (0, lodash_1.upperFirst)(
                cardanoNodeState != null
                  ? intl.formatMessage(
                      this.getLocalisationForCardanoNodeState()
                    )
                  : 'unknown'
              )
            ),
            getRow('cardanoNodeResponding', isNodeResponding),
            getRow('cardanoNodeTimeCorrect', isNodeTimeCorrect),
            getRow('cardanoNodeSyncing', isNodeSyncing),
            getRow('cardanoNodeInSync', isNodeInSync)
          )
        )
      )
    );
  }
  getLocalisationForCardanoNodeState = () => {
    const { cardanoNodeState } = this.props;
    let localisationKey;
    switch (cardanoNodeState) {
      case cardano_node_types_1.CardanoNodeStates.STARTING:
        localisationKey = messages.nodeIsStarting;
        break;
      case cardano_node_types_1.CardanoNodeStates.EXITING:
        localisationKey = messages.nodeIsExiting;
        break;
      case cardano_node_types_1.CardanoNodeStates.STOPPING:
        localisationKey = messages.nodeIsStopping;
        break;
      case cardano_node_types_1.CardanoNodeStates.STOPPED:
        localisationKey = messages.nodeHasStopped;
        break;
      case cardano_node_types_1.CardanoNodeStates.UPDATING:
        localisationKey = messages.nodeIsUpdating;
        break;
      case cardano_node_types_1.CardanoNodeStates.UPDATED:
        localisationKey = messages.nodeHasBeenUpdated;
        break;
      case cardano_node_types_1.CardanoNodeStates.CRASHED:
        localisationKey = messages.nodeHasCrashed;
        break;
      case cardano_node_types_1.CardanoNodeStates.ERRORED:
        localisationKey = messages.nodeHasErrored;
        break;
      case cardano_node_types_1.CardanoNodeStates.UNRECOVERABLE:
        localisationKey = messages.nodeIsUnrecoverable;
        break;
      default:
        localisationKey = messages.nodeIsRunning;
        break;
    }
    return localisationKey;
  };
  restoreDialogCloseOnEscKey = () => {
    // This method is to be used on buttons which get disabled after click
    // as without it the ReactModal is not closing if you press the ESC key
    // even after the button is later re-enabled
    // @ts-ignore ts-migrate(2339) FIXME: Property 'focus' does not exist on type 'Element'.
    document.getElementsByClassName('ReactModal__Content')[0].focus();
  };
  checkTime = () => {
    this.props.onForceCheckNetworkClock();
    this.restoreDialogCloseOnEscKey();
  };
  restartNode = () => {
    this.setState({
      isNodeRestarting: true,
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property 'trigger' does not exist on type '(...arg... Remove this comment to see the full error message
    this.props.onRestartNode.trigger();
    this.restoreDialogCloseOnEscKey();
  };
};
DaedalusDiagnostics = __decorate(
  [mobx_react_1.observer, __metadata('design:paramtypes', [Object])],
  DaedalusDiagnostics
);
exports.default = DaedalusDiagnostics;
//# sourceMappingURL=DaedalusDiagnostics.js.map
