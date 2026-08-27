// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import React, { Component, FormEvent, Fragment, Node } from 'react';
import classNames from 'classnames';
import { observer } from 'mobx-react';
import { get, includes, upperFirst } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import CopyToClipboard from 'react-copy-to-clipboard';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import SVGInline from 'react-svg-inline';
import globalMessages from '../../i18n/global-messages';
import DialogCloseButton from '../widgets/DialogCloseButton';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import iconCopy from '../../assets/images/clipboard-ic.inline.svg';
import sandClockIcon from '../../assets/images/sand-clock-xs.inline.svg';
import LocalizableError from '../../i18n/LocalizableError';
import { formattedNumber, formattedSize } from '../../utils/formatters';
import { getSupportUrl } from '../../../../common/utils/reporting';
import { CardanoNodeStates } from '../../../../common/types/cardano-node.types';
import styles from './DaedalusDiagnostics.scss';
import type { CardanoNodeState } from '../../../../common/types/cardano-node.types';
import type { SystemInfo } from '../../types/systemInfoTypes';
import type { CoreSystemInfo } from '../../types/coreSystemInfoTypes';
import type { TipInfo } from '../../api/network/types';
import { ErrorType } from '../../domains/ApiError';
import DiagnosticsTimeStatusRow from './DiagnosticsTimeStatusRow';

export const messages = defineMessages({
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
  cardanoNodeUptime: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeUptime',
    defaultMessage: '!!!Cardano node uptime',
    description: 'How long cardano-node has been running',
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
  cardanoWalletUptime: {
    id: 'daedalus.diagnostics.dialog.cardanoWalletUptime',
    defaultMessage: '!!!Cardano wallet uptime',
    description: 'How long cardano-wallet has been running since last start',
  },
  cardanoWalletRestartCount: {
    id: 'daedalus.diagnostics.dialog.cardanoWalletRestartCount',
    defaultMessage: '!!!Cardano wallet restarts',
    description:
      'Number of times cardano-wallet has been restarted by the watchdog',
  },
  watchdogPid: {
    id: 'daedalus.diagnostics.dialog.watchdogPid',
    defaultMessage: '!!!Watchdog process ID',
    description: 'PID of the cardano-watchdog supervisor process',
  },
  nodeForceKilled: {
    id: 'daedalus.diagnostics.dialog.nodeForceKilled',
    defaultMessage: '!!!Node force-killed',
    description:
      'Whether the watchdog had to SIGKILL cardano-node during shutdown',
  },
  cardanoWalletLastExitCode: {
    id: 'daedalus.diagnostics.dialog.cardanoWalletLastExitCode',
    defaultMessage: '!!!Wallet last exit code',
    description:
      'Exit code from the most recent cardano-wallet crash before watchdog restart',
  },
  nodeSocketWaitMs: {
    id: 'daedalus.diagnostics.dialog.nodeSocketWaitMs',
    defaultMessage: '!!!Node socket wait',
    description:
      'Time the watchdog waited for the cardano-node socket to appear',
  },
  walletReadyWaitMs: {
    id: 'daedalus.diagnostics.dialog.walletReadyWaitMs',
    defaultMessage: '!!!Wallet ready wait',
    description: 'Time the watchdog waited for cardano-wallet to become ready',
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
  cardanoNode: {
    id: 'daedalus.diagnostics.dialog.cardanoNode',
    defaultMessage: '!!!Cardano node',
    description: 'Cardano node label',
  },
  cardanoWallet: {
    id: 'daedalus.diagnostics.dialog.cardanoWallet',
    defaultMessage: '!!!Cardano wallet',
    description: 'Cardano wallet label',
  },
  cardanoNodeStatusRestarting: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeStatusRestarting',
    defaultMessage: '!!!Restarting {subject}...',
    description: 'Restarting a cardano process',
  },
  cardanoNodeStatusRestart: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeStatusRestart',
    defaultMessage: '!!!Restart {subject}',
    description: 'Restart a cardano process',
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
  dappBrowser: {
    id: 'daedalus.diagnostics.dialog.dappBrowser',
    defaultMessage: '!!!UNTRUSTED DAPP BROWSER',
    description: 'Heading for the Diagnostics arbitrary dApp launcher.',
  },
  dappBrowserWarning: {
    id: 'daedalus.diagnostics.dialog.dappBrowserWarning',
    defaultMessage:
      '!!!This opens an untrusted website. Daedalus does not audit or endorse it. Review every wallet request.',
    description: 'Security warning above the Diagnostics dApp launcher.',
  },
  dappBrowserUrl: {
    id: 'daedalus.diagnostics.dialog.dappBrowserUrl',
    defaultMessage: '!!!DApp URL',
    description: 'Label for the Diagnostics dApp URL field.',
  },
  dappBrowserWallet: {
    id: 'daedalus.diagnostics.dialog.dappBrowserWallet',
    defaultMessage: '!!!Wallet',
    description: 'Label for the Diagnostics dApp wallet selector.',
  },
  dappBrowserLaunch: {
    id: 'daedalus.diagnostics.dialog.dappBrowserLaunch',
    defaultMessage: '!!!Launch untrusted dApp',
    description: 'Button that launches an arbitrary Diagnostics dApp.',
  },
  dappBrowserLaunchFailed: {
    id: 'daedalus.diagnostics.dialog.dappBrowserLaunchFailed',
    defaultMessage: '!!!The dApp could not be opened.',
    description: 'Privacy-safe Diagnostics dApp launch failure.',
  },
  dappBrowserWindowTitle: {
    id: 'daedalus.diagnostics.dialog.dappBrowserWindowTitle',
    defaultMessage: '!!!Untrusted dApp',
    description: 'Local native window title for a Diagnostics dApp.',
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
type Props = {
  systemInfo: SystemInfo;
  coreInfo: CoreSystemInfo;
  cardanoNodeState: CardanoNodeState | null | undefined;
  isNodeResponding: boolean;
  // isNodeSubscribed: boolean,
  isNodeSyncing: boolean;
  isNodeInSync: boolean;
  isNodeTimeCorrect: boolean;
  nodeConnectionError: LocalizableError | null | undefined;
  isConnected: boolean;
  isSynced: boolean;
  syncPercentage: number;
  localTimeDifference: number | null | undefined;
  isSystemTimeCorrect: boolean;
  isSystemTimeIgnored: boolean;
  isCheckingSystemTime: boolean;
  isForceCheckingSystemTime: boolean;
  localTip: TipInfo | null | undefined;
  networkTip: TipInfo | null | undefined;
  onOpenStateDirectory: (...args: Array<any>) => any;
  onOpenExternalLink: (...args: Array<any>) => any;
  onRestartNode: { trigger: (...args: Array<any>) => any };
  onRestartWallet: { trigger: (...args: Array<any>) => any };
  onClose: (...args: Array<any>) => any;
  onCopyStateDirectoryPath: (...args: Array<any>) => any;
  onForceCheckNetworkClock: (...args: Array<any>) => any;
  diagnosticsWallets: readonly Readonly<{ id: string; name: string }>[];
  defaultDiagnosticsWalletId: string;
  diagnosticsAvailable: boolean;
  diagnosticsReady: boolean;
  isDappLaunching: boolean;
  onLaunchDapp: (
    url: string,
    walletId: string,
    localName: string
  ) => Promise<void>;
};
type State = {
  isNodeRestarting: boolean;
  isWalletRestarting: boolean;
  dappUrl: string;
  diagnosticsWalletId: string;
  dappLaunchFailed: boolean;
};
const FINAL_CARDANO_NODE_STATES = [
  CardanoNodeStates.RUNNING,
  CardanoNodeStates.UPDATED,
  CardanoNodeStates.CRASHED,
  CardanoNodeStates.ERRORED,
  CardanoNodeStates.UNRECOVERABLE,
  CardanoNodeStates.READY,
];

@observer
class DaedalusDiagnostics extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: Props) {
    super(props);
    this.state = {
      isNodeRestarting: false,
      isWalletRestarting: false,
      dappUrl: '',
      diagnosticsWalletId: props.defaultDiagnosticsWalletId,
      dappLaunchFailed: false,
    };
  }

  componentDidUpdate(prevProps: Props) {
    const { cardanoNodeState: prevCardanoNodeState } = prevProps;
    const { cardanoNodeState } = this.props;

    // Reset node-restarting spinner once the node socket is ready and the phase
    // is back to a final state. Waiting for 'ready' (not just PID change) means
    // the spinner stays up through socket-wait and wallet startup after restart.
    const prevNodePID = prevProps.coreInfo.cardanoNodePID;
    const nextNodePID = this.props.coreInfo.cardanoNodePID;
    const nodeBackUp =
      nextNodePID !== 0 &&
      nextNodePID !== prevNodePID &&
      cardanoNodeState === CardanoNodeStates.READY;
    if (
      (cardanoNodeState !== prevCardanoNodeState &&
        includes(FINAL_CARDANO_NODE_STATES, cardanoNodeState)) ||
      nodeBackUp
    ) {
      this.setState({ isNodeRestarting: false }); // eslint-disable-line
    }

    // Reset wallet-restarting spinner when the wallet PID changes.
    const prevWalletPID = prevProps.coreInfo.cardanoWalletPID;
    const nextWalletPID = this.props.coreInfo.cardanoWalletPID;
    if (nextWalletPID !== 0 && nextWalletPID !== prevWalletPID) {
      this.setState({ isWalletRestarting: false }); // eslint-disable-line
    }
    if (
      !this.props.diagnosticsWallets.some(
        ({ id }) => id === this.state.diagnosticsWalletId
      )
    ) {
      const diagnosticsWalletId =
        this.props.defaultDiagnosticsWalletId ||
        this.props.diagnosticsWallets[0]?.id ||
        '';
      if (diagnosticsWalletId !== this.state.diagnosticsWalletId)
        this.setState({ diagnosticsWalletId });
    }
  }

  getSectionRow = (messageId: string, content?: Node) => (
    <div className={styles.layoutRow}>
      <div className={styles.sectionTitle}>
        <span>{this.context.intl.formatMessage(messages[messageId])}</span>
        {content}
        <hr />
      </div>
    </div>
  );
  getRow = (messageId: string, value: Node | boolean) => {
    const { intl } = this.context;
    const key = intl.formatMessage(messages[messageId]);
    const colon = intl.formatMessage(globalMessages.punctuationColon);
    let content = value;
    let className = classNames([styles[messageId], styles.layoutData]);
    const classNameHeader = classNames([
      styles[messageId],
      styles.layoutHeader,
    ]);
    const classNameRow = classNames([styles.layoutRow, messageId]);

    if (typeof value === 'boolean') {
      content = value
        ? intl.formatMessage(messages.statusOn)
        : intl.formatMessage(messages.statusOff);
      className =
        (value && messageId !== 'systemTimeIgnored') ||
        (!value && messageId === 'systemTimeIgnored')
          ? classNames([className, styles.green])
          : classNames([className, styles.red]);
    }

    return (
      <div className={classNameRow}>
        <div className={classNameHeader}>
          {key}
          {colon}
        </div>
        <div className={className}>{content}</div>
      </div>
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
    const availableDiskSpace = formattedSize(
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
      cardanoNodeUptime,
      cardanoWalletVersion,
      cardanoWalletPID,
      cardanoWalletUptime,
      cardanoWalletRestartCount,
      cardanoWalletApiPort,
      cardanoNetwork,
      daedalusStateDirectoryPath,
      watchdogPid,
      nodeForceKilled,
      lastWalletExitCode,
      nodeSocketWaitMs,
      walletReadyWaitMs,
    } = coreInfo;
    const { isNodeRestarting, isWalletRestarting } = this.state;
    const connectionError = get(nodeConnectionError, 'values', '{}');
    const { message, code } = connectionError as ErrorType;
    const unknownDiskSpaceSupportUrl = getSupportUrl(intl.locale);
    const formattedSyncPercentage = formattedNumber(syncPercentage, 2);
    const cardanoNetworkValue = intl.formatMessage(
      globalMessages[`network_${cardanoNetwork}`]
    );
    const { getSectionRow, getRow } = this;

    return (
      <div
        className={classNames(styles.component, {
          [styles.dappBrowserAvailable]: this.props.diagnosticsAvailable,
        })}
      >
        <DialogCloseButton
          className={styles.closeButton}
          icon={closeCrossThin}
          onClose={onClose}
        />

        <div className={styles.tables}>
          <div className={styles.table}>
            <div>
              {getSectionRow('systemInfo')}
              {getRow('platform', platform)}
              {getRow('platformVersion', platformVersion)}
              {getRow('cpu', <PopOver content={cpu}>{cpu}</PopOver>)}
              {getRow('ram', ram)}
              {getRow(
                'availableDiskSpace',
                availableDiskSpace || (
                  <Link
                    onClick={() =>
                      onOpenExternalLink(unknownDiskSpaceSupportUrl)
                    }
                    label={intl.formatMessage(messages.unknownDiskSpace)}
                    skin={LinkSkin}
                  />
                )
              )}
              {getRow(
                'hasMetHardwareRequirementsLabel',
                <PopOver
                  content={intl.formatMessage(
                    hasMetHardwareRequirements
                      ? messages.hasMetHardwareRequirementsStatusGoodTooltip
                      : messages.hasMetHardwareRequirementsStatusLowTooltip
                  )}
                >
                  <div
                    className={classNames(
                      styles.layoutData,
                      hasMetHardwareRequirements ? styles.green : styles.red
                    )}
                  >
                    {intl.formatMessage(
                      hasMetHardwareRequirements
                        ? messages.hasMetHardwareRequirementsStatusGoodValue
                        : messages.hasMetHardwareRequirementsStatusLowValue
                    )}
                  </div>
                </PopOver>
              )}
              {getRow(
                'isRTSFlagsModeEnabled',
                intl.formatMessage(
                  isRTSFlagsModeEnabled
                    ? messages.statusOnForUserSettings
                    : messages.statusOffForUserSettings
                )
              )}
            </div>
            <div>
              {getSectionRow('coreInfo')}
              {getRow('daedalusVersion', daedalusVersion)}
              {getRow('daedalusBuildNumber', daedalusBuildNumber)}
              {getRow('daedalusMainProcessID', daedalusMainProcessID)}
              {getRow('daedalusProcessID', daedalusProcessID)}
              {getRow(
                'blankScreenFix',
                isBlankScreenFixActive
                  ? intl.formatMessage(messages.statusOnForUserSettings)
                  : intl.formatMessage(messages.statusOffForUserSettings)
              )}
              {getRow(
                'stateDirectoryPath',
                <Fragment>
                  <button
                    className={styles.stateDirectoryOpenBtn}
                    onClick={() =>
                      onOpenStateDirectory(daedalusStateDirectoryPath)
                    }
                  >
                    {intl.formatMessage(messages.stateDirectoryPathOpenBtn)}
                  </button>
                  <CopyToClipboard
                    text={daedalusStateDirectoryPath}
                    onCopy={onCopyStateDirectoryPath}
                  >
                    <div className={styles.stateDirectoryPath}>
                      <PopOver
                        maxWidth={400}
                        content={
                          <div className={styles.tooltipLabelWrapper}>
                            <div>{daedalusStateDirectoryPath}</div>
                          </div>
                        }
                      >
                        <div className={styles.daedalusStateDirectoryPath}>
                          {daedalusStateDirectoryPath}
                        </div>
                        <SVGInline svg={iconCopy} />
                      </PopOver>
                    </div>
                  </CopyToClipboard>
                </Fragment>
              )}
              {getRow('cardanoNodeVersion', cardanoNodeVersion)}
              {getRow('cardanoNodePID', cardanoNodePID || '-')}
              {getRow('cardanoNodeUptime', cardanoNodeUptime)}
              {/* getRow('cardanoNodeApiPort', '-') */}
              {getRow('cardanoWalletVersion', cardanoWalletVersion)}
              {getRow('cardanoWalletPID', cardanoWalletPID || '-')}
              {getRow('cardanoWalletUptime', cardanoWalletUptime)}
              {getRow('cardanoWalletApiPort', cardanoWalletApiPort || '-')}
              {getRow('cardanoWalletRestartCount', cardanoWalletRestartCount)}
              {watchdogPid != null &&
                watchdogPid > 0 &&
                getRow('watchdogPid', watchdogPid)}
              {nodeForceKilled != null &&
                getRow('nodeForceKilled', nodeForceKilled)}
              {cardanoWalletRestartCount > 0 &&
                lastWalletExitCode != null &&
                getRow('cardanoWalletLastExitCode', String(lastWalletExitCode))}
              {nodeSocketWaitMs != null &&
                getRow('nodeSocketWaitMs', `${nodeSocketWaitMs}ms`)}
              {walletReadyWaitMs != null &&
                getRow('walletReadyWaitMs', `${walletReadyWaitMs}ms`)}
            </div>
            {isConnected && nodeConnectionError ? (
              <div>
                {getSectionRow('connectionError')}
                <div className={styles.layoutRow}>
                  <div className={styles.layoutHeader}>
                    <div className={styles.error}>
                      {intl.formatMessage(messages.message)}: {message || '-'}
                      <br />
                      {intl.formatMessage(messages.code)}: {code || '-'}
                    </div>
                  </div>
                </div>
              </div>
            ) : null}
          </div>

          <div className={styles.table}>
            <div>
              {getSectionRow('daedalusStatus')}
              {getRow('cardanoNetwork', cardanoNetworkValue)}
              {getRow('connected', isConnected)}
              {getRow('synced', isSynced)}
              {getRow('syncPercentage', `${formattedSyncPercentage}%`)}

              {getRow(
                'lastNetworkBlock',
                <Fragment>
                  <span>{intl.formatMessage(messages.epoch)}:</span>{' '}
                  {networkTip && networkTip.epoch ? (
                    formattedNumber(networkTip.epoch)
                  ) : (
                    <SVGInline
                      svg={sandClockIcon}
                      className={styles.networkTipSandClock}
                    />
                  )}
                  <span>{intl.formatMessage(messages.slot)}:</span>{' '}
                  {networkTip && networkTip.slot ? (
                    formattedNumber(networkTip.slot)
                  ) : (
                    <SVGInline
                      svg={sandClockIcon}
                      className={styles.networkTipSandClock}
                    />
                  )}
                </Fragment>
              )}
              {getRow(
                'lastSynchronizedBlock',
                <Fragment>
                  <span>{intl.formatMessage(messages.epoch)}:</span>{' '}
                  {localTip && localTip.epoch ? (
                    formattedNumber(localTip.epoch)
                  ) : (
                    <SVGInline
                      svg={sandClockIcon}
                      className={styles.networkTipSandClock}
                    />
                  )}
                  <span>{intl.formatMessage(messages.slot)}:</span>{' '}
                  {localTip && localTip.slot ? (
                    formattedNumber(localTip.slot)
                  ) : (
                    <SVGInline
                      svg={sandClockIcon}
                      className={styles.networkTipSandClock}
                    />
                  )}
                </Fragment>
              )}
              <DiagnosticsTimeStatusRow
                isCheckingSystemTime={isCheckingSystemTime}
                isForceCheckingSystemTime={isForceCheckingSystemTime}
                isNodeResponding={isNodeResponding}
                localTimeDifference={localTimeDifference}
                onCheckTime={this.checkTime}
              />
              {getRow('systemTimeCorrect', isSystemTimeCorrect)}
              {getRow('systemTimeIgnored', isSystemTimeIgnored)}
              {
                <div className={styles.layoutRow}>
                  <div className={styles.layoutHeader}>
                    {intl.formatMessage(messages.checkingNodeTime)}
                    {intl.formatMessage(globalMessages.punctuationColon)}
                  </div>
                  <div className={styles.layoutData}>
                    {isCheckingSystemTime
                      ? intl.formatMessage(messages.statusOn)
                      : intl.formatMessage(messages.statusOff)}
                  </div>
                </div>
              }
            </div>
            <div>
              {getSectionRow(
                'cardanoNodeStatus',
                <span style={{ display: 'flex', float: 'right', gap: '8px' }}>
                  <button
                    className={styles.cardanoNodeStatusBtn}
                    style={{ float: 'none' }}
                    onClick={() => this.restartNode()}
                    disabled={
                      !includes(FINAL_CARDANO_NODE_STATES, cardanoNodeState)
                    }
                  >
                    {isNodeRestarting
                      ? intl.formatMessage(
                          messages.cardanoNodeStatusRestarting,
                          {
                            subject: intl.formatMessage(messages.cardanoNode),
                          }
                        )
                      : intl.formatMessage(messages.cardanoNodeStatusRestart, {
                          subject: intl.formatMessage(messages.cardanoNode),
                        })}
                  </button>
                  <button
                    className={styles.cardanoNodeStatusBtn}
                    style={{ float: 'none' }}
                    onClick={() => this.restartWallet()}
                    disabled={cardanoNodeState !== CardanoNodeStates.READY}
                  >
                    {isWalletRestarting
                      ? intl.formatMessage(
                          messages.cardanoNodeStatusRestarting,
                          {
                            subject: intl.formatMessage(messages.cardanoWallet),
                          }
                        )
                      : intl.formatMessage(messages.cardanoNodeStatusRestart, {
                          subject: intl.formatMessage(messages.cardanoWallet),
                        })}
                  </button>
                </span>
              )}
              {getRow(
                'cardanoNodeState',
                upperFirst(
                  cardanoNodeState != null
                    ? intl.formatMessage(
                        this.getLocalisationForCardanoNodeState()
                      )
                    : 'unknown'
                )
              )}
              {getRow('cardanoNodeResponding', isNodeResponding)}
              {/* getRow('cardanoNodeSubscribed', isNodeSubscribed) */}
              {getRow('cardanoNodeTimeCorrect', isNodeTimeCorrect)}
              {getRow('cardanoNodeSyncing', isNodeSyncing)}
              {getRow('cardanoNodeInSync', isNodeInSync)}
            </div>
            {this.props.diagnosticsAvailable && (
              <form className={styles.dappBrowser} onSubmit={this.launchDapp}>
                {getSectionRow('dappBrowser')}
                <p className={styles.dappBrowserWarning}>
                  {intl.formatMessage(messages.dappBrowserWarning)}
                </p>
                <label htmlFor="diagnostics-dapp-url">
                  {intl.formatMessage(messages.dappBrowserUrl)}
                </label>
                <input
                  id="diagnostics-dapp-url"
                  type="url"
                  required
                  value={this.state.dappUrl}
                  onChange={(event) =>
                    this.setState({
                      dappUrl: event.target.value,
                      dappLaunchFailed: false,
                    })
                  }
                />
                <label htmlFor="diagnostics-dapp-wallet">
                  {intl.formatMessage(messages.dappBrowserWallet)}
                </label>
                <select
                  id="diagnostics-dapp-wallet"
                  value={this.state.diagnosticsWalletId}
                  onChange={(event) =>
                    this.setState({
                      diagnosticsWalletId: event.target.value,
                      dappLaunchFailed: false,
                    })
                  }
                >
                  {this.props.diagnosticsWallets.map(({ id, name }) => (
                    <option key={id} value={id}>
                      {name}
                    </option>
                  ))}
                </select>
                {this.state.dappLaunchFailed && (
                  <div className={styles.error} role="alert">
                    {intl.formatMessage(messages.dappBrowserLaunchFailed)}
                  </div>
                )}
                <button
                  type="submit"
                  disabled={
                    !this.props.diagnosticsReady ||
                    this.props.isDappLaunching ||
                    this.state.dappUrl.trim() === '' ||
                    this.state.diagnosticsWalletId === ''
                  }
                >
                  {intl.formatMessage(messages.dappBrowserLaunch)}
                </button>
              </form>
            )}
          </div>
        </div>
      </div>
    );
  }

  launchDapp = async (event: FormEvent<HTMLFormElement>): Promise<void> => {
    event.preventDefault();
    this.setState({ dappLaunchFailed: false });
    try {
      await this.props.onLaunchDapp(
        this.state.dappUrl,
        this.state.diagnosticsWalletId,
        this.context.intl.formatMessage(messages.dappBrowserWindowTitle)
      );
    } catch {
      this.setState({ dappLaunchFailed: true });
    }
  };
  getLocalisationForCardanoNodeState = () => {
    const { cardanoNodeState } = this.props;
    let localisationKey;

    switch (cardanoNodeState) {
      case CardanoNodeStates.STARTING:
        localisationKey = messages.nodeIsStarting;
        break;

      case CardanoNodeStates.EXITING:
        localisationKey = messages.nodeIsExiting;
        break;

      case CardanoNodeStates.STOPPING:
        localisationKey = messages.nodeIsStopping;
        break;

      case CardanoNodeStates.STOPPED:
        localisationKey = messages.nodeHasStopped;
        break;

      case CardanoNodeStates.UPDATING:
        localisationKey = messages.nodeIsUpdating;
        break;

      case CardanoNodeStates.UPDATED:
        localisationKey = messages.nodeHasBeenUpdated;
        break;

      case CardanoNodeStates.CRASHED:
        localisationKey = messages.nodeHasCrashed;
        break;

      case CardanoNodeStates.ERRORED:
        localisationKey = messages.nodeHasErrored;
        break;

      case CardanoNodeStates.UNRECOVERABLE:
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
    const modalContent = document.getElementsByClassName(
      'ReactModal__Content'
    )[0];

    if (modalContent && 'focus' in modalContent) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'focus' does not exist on type 'Element'.
      modalContent.focus();
    }
  };
  checkTime = () => {
    this.props.onForceCheckNetworkClock();
    this.restoreDialogCloseOnEscKey();
  };
  restartNode = () => {
    this.setState({
      isNodeRestarting: true,
    });
    this.props.onRestartNode.trigger();
    this.restoreDialogCloseOnEscKey();
  };

  restartWallet = () => {
    this.setState({
      isWalletRestarting: true,
    });
    this.props.onRestartWallet.trigger();
    this.restoreDialogCloseOnEscKey();
  };
}

export default DaedalusDiagnostics;
