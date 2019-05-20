// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get, includes, upperFirst } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import classNames from 'classnames';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import SVGInline from 'react-svg-inline';
import {
  ALLOWED_TIME_DIFFERENCE,
  MAX_ALLOWED_STALL_DURATION,
} from '../../config/timingConfig';
import { UNSYNCED_BLOCKS_ALLOWED } from '../../config/numbersConfig';
import { getNetworkEkgUrl } from '../../utils/network';
import closeCross from '../../assets/images/close-cross.inline.svg';
import LocalizableError from '../../i18n/LocalizableError';
import { CardanoNodeStates } from '../../../../common/types/cardano-node.types';
import styles from './NetworkStatus.scss';
import type { CardanoNodeState } from '../../../../common/types/cardano-node.types';
import type { SystemInfo } from '../../types/systemInfoTypes';
import type { CoreSystemInfo } from '../../types/coreSystemInfoTypes';

let syncingInterval = null;

const messages = defineMessages({
  systemInfo: {
    id: 'status.network.dialog.system.info',
    defaultMessage: '!!!SYSTEM INFO',
    description: 'System info',
  },
  platform: {
    id: 'status.network.dialog.platform',
    defaultMessage: '!!!Platform',
    description: 'Platform',
  },
  platformVersion: {
    id: 'status.network.dialog.platform.version',
    defaultMessage: '!!!Platform Version',
    description: 'Platform Version',
  },
  cpu: {
    id: 'status.network.dialog.cpu',
    defaultMessage: '!!!CPU',
    description: 'CPU',
  },
  ram: {
    id: 'status.network.dialog.ram',
    defaultMessage: '!!!RAM',
    description: 'RAM',
  },
  availableDiskSpace: {
    id: 'status.network.dialog.availableDiskSpace',
    defaultMessage: '!!!Available disk space',
    description: 'Available disk space',
  },
  coreInfo: {
    id: 'status.network.dialog.coreInfo',
    defaultMessage: '!!!CORE INFO',
    description: 'CORE INFO',
  },
  daedalusVersion: {
    id: 'status.network.dialog.daedalusVersion',
    defaultMessage: '!!!Daedalus Version',
    description: 'Daedalus Version',
  },
  daedalusMainProcessID: {
    id: 'status.network.dialog.daedalusMainProcessID',
    defaultMessage: '!!!Daedalus Main Process ID',
    description: 'Daedalus Main Process ID',
  },
  daedalusProcessID: {
    id: 'status.network.dialog.daedalusProcessID',
    defaultMessage: '!!!Daedalus Renderer Process ID',
    description: 'Daedalus Renderer Process ID',
  },
  safeMode: {
    id: 'status.network.dialog.safeMode',
    defaultMessage: '!!!Daedalus is running in safe mode',
    description: 'Daedalus is running in safe mode',
  },
  cardanoVersion: {
    id: 'status.network.dialog.cardanoVersion',
    defaultMessage: '!!!Cardano Version',
    description: 'Cardano Version',
  },
  cardanoProcessID: {
    id: 'status.network.dialog.cardanoProcessID',
    defaultMessage: '!!!Cardano Process ID',
    description: 'Cardano Process ID',
  },
  cardanoApiPort: {
    id: 'status.network.dialog.cardanoApiPort',
    defaultMessage: '!!!Cardano API Port',
    description: 'Cardano API Port',
  },
  cardanoNetwork: {
    id: 'status.network.dialog.cardanoNetwork',
    defaultMessage: '!!!Cardano Network',
    description: 'Cardano Network',
  },
  stateDirectory: {
    id: 'status.network.dialog.stateDirectory',
    defaultMessage: '!!!Daedalus State Directory',
    description: 'Daedalus State Directory',
  },
  connectionError: {
    id: 'status.network.dialog.connectionError',
    defaultMessage: '!!!CONNECTION ERROR',
    description: 'CONNECTION ERROR',
  },
  daedalusStatus: {
    id: 'status.network.dialog.daedalusStatus',
    defaultMessage: '!!!DAEDALUS STATUS',
    description: 'DAEDALUS STATUS',
  },
  connected: {
    id: 'status.network.dialog.connected',
    defaultMessage: '!!!Connected',
    description: 'Connected',
  },
  synced: {
    id: 'status.network.dialog.synced',
    defaultMessage: '!!!Synced',
    description: 'Synced',
  },
  syncPercentage: {
    id: 'status.network.dialog.syncPercentage',
    defaultMessage: '!!!Sync Percentage',
    description: 'Sync Percentage',
  },
  networkBlockHeight: {
    id: 'status.network.dialog.networkBlockHeight',
    defaultMessage: '!!!Network Block Height',
    description: 'Network Block Height',
  },
  localBlockHeight: {
    id: 'status.network.dialog.localBlockHeight',
    defaultMessage: '!!!Local Block Height',
    description: 'Local Block Height',
  },
  remainingUnsyncedBlocks: {
    id: 'status.network.dialog.remainingUnsyncedBlocks',
    defaultMessage: '!!!Remaining Unsynced Blocks',
    description: 'Remaining Unsynced Blocks',
  },
  latestLocalBlockAge: {
    id: 'status.network.dialog.latestLocalBlockAge',
    defaultMessage: '!!!Latest Local Block Age',
    description: 'Latest Local Block Age',
  },
  latestNetworkBlockAge: {
    id: 'status.network.dialog.latestNetworkBlockAge',
    defaultMessage: '!!!Latest Network Block Age',
    description: 'Latest Network Block Age',
  },
  localTimeDifference: {
    id: 'status.network.dialog.localTimeDifference',
    defaultMessage: '!!!Local Time Difference',
    description: 'Local Time Difference',
  },
  systemTimeCorrect: {
    id: 'status.network.dialog.systemTimeCorrect',
    defaultMessage: '!!!System Time Correct',
    description: 'System Time Correct',
  },
  systemTimeIgnored: {
    id: 'status.network.dialog.systemTimeIgnored',
    defaultMessage: '!!!System Time Ignored',
    description: 'System Time Ignored',
  },
  checkingNodeTime: {
    id: 'status.network.dialog.checkingNodeTime',
    defaultMessage: '!!!Checking Node Time',
    description: 'Checking Node Time',
  },
  cardanoNodeStatus: {
    id: 'status.network.dialog.cardanoNodeStatus',
    defaultMessage: '!!!CARDANO NODE STATUS',
    description: 'CARDANO NODE STATUS',
  },
  cardanoNodeStatusRestarting: {
    id: 'status.network.dialog.cardanoNodeStatusRestarting',
    defaultMessage: '!!!Restarting Cardano Node...',
    description: 'Restarting Cardano Node...',
  },
  cardanoNodeStatusRestart: {
    id: 'status.network.dialog.cardanoNodeStatusRestart',
    defaultMessage: '!!!Restart Cardano Node',
    description: 'Restart Cardano Node',
  },
  cardanoNodeDiagnostics: {
    id: 'status.network.dialog.cardanoNodeDiagnostics',
    defaultMessage: '!!!Cardano Node Diagnostics',
    description: 'Cardano Node Diagnostics',
  },
  realtimeStatisticsMonitor: {
    id: 'status.network.dialog.realtimeStatisticsMonitor',
    defaultMessage: '!!!Realtime statistics monitor',
    description: 'Realtime statistics monitor',
  },
  cardanoNodeState: {
    id: 'status.network.dialog.cardanoNodeState',
    defaultMessage: '!!!Cardano Node State',
    description: 'Cardano Node State',
  },
  nodeHasBeenUpdated: {
    id: 'status.network.dialog.nodeHasBeenUpdated',
    defaultMessage: '!!!Updated',
    description: 'Updated',
  },
  nodeHasCrashed: {
    id: 'status.network.dialog.nodeHasCrashed',
    defaultMessage: '!!!Crashed',
    description: 'Crashed',
  },
  nodeHasErrored: {
    id: 'status.network.dialog.nodeHasErrored',
    defaultMessage: '!!!Errored',
    description: 'Errored',
  },
  nodeHasStopped: {
    id: 'status.network.dialog.nodeHasStopped',
    defaultMessage: '!!!Stopped',
    description: 'Stopped',
  },
  nodeIsExiting: {
    id: 'status.network.dialog.nodeIsExiting',
    defaultMessage: '!!!Exiting',
    description: 'Exiting',
  },
  nodeIsRunning: {
    id: 'status.network.dialog.nodeIsRunning',
    defaultMessage: '!!!Running',
    description: 'Running',
  },
  nodeIsStarting: {
    id: 'status.network.dialog.nodeIsStarting',
    defaultMessage: '!!!Starting',
    description: 'Starting',
  },
  nodeIsStopping: {
    id: 'status.network.dialog.nodeIsStopping',
    defaultMessage: '!!!Stopping',
    description: 'Stopping',
  },
  nodeIsUnrecoverable: {
    id: 'status.network.dialog.nodeIsUnrecoverable',
    defaultMessage: '!!!Unrecoverable',
    description: 'Unrecoverable',
  },
  nodeIsUpdating: {
    id: 'status.network.dialog.nodeIsUpdating',
    defaultMessage: '!!!Updating',
    description: 'Updating',
  },
  cardanoNodeResponding: {
    id: 'status.network.dialog.cardanoNodeResponding',
    defaultMessage: '!!!Node Responding',
    description: 'Node Responding',
  },
  cardanoNodeSubscribed: {
    id: 'status.network.dialog.cardanoNodeSubscribed',
    defaultMessage: '!!!Node Subscribed',
    description: 'Node Subscribed',
  },
  cardanoNodeTimeCorrect: {
    id: 'status.network.dialog.cardanoNodeTimeCorrect',
    defaultMessage: '!!!Node Time Correct',
    description: 'Node Time Correct',
  },
  cardanoNodeSyncing: {
    id: 'status.network.dialog.cardanoNodeSyncing',
    defaultMessage: '!!!Node Syncing',
    description: 'Node Syncing',
  },
  cardanoNodeInSync: {
    id: 'status.network.dialog.cardanoNodeInSync',
    defaultMessage: '!!!Node In Sync',
    description: 'Node In Sync',
  },
  localTimeDifferenceChecking: {
    id: 'status.network.dialog.localTimeDifferenceChecking',
    defaultMessage: '!!!Checking...',
    description: 'Checking...',
  },
  localTimeDifferenceCheckTime: {
    id: 'status.network.dialog.localTimeDifferenceCheckTime',
    defaultMessage: '!!!Check time',
    description: 'Check time',
  },
  statusOn: {
    id: 'status.network.dialog.statusOn',
    defaultMessage: '!!!YES',
    description: 'YES',
  },
  statusOff: {
    id: 'status.network.dialog.statusOff',
    defaultMessage: '!!!NO',
    description: 'NO',
  },
  serviceUnreachable: {
    id: 'status.network.dialog.serviceUnreachable',
    defaultMessage: '!!!NTP Service unreachable',
    description: 'NTP Service unreachable',
  },
});

type Props = {
  systemInfo: SystemInfo,
  coreInfo: CoreSystemInfo,
  cardanoNodeState: ?CardanoNodeState,
  isDev: boolean,
  isMainnet: boolean,
  isStaging: boolean,
  isTestnet: boolean,
  isNodeResponding: boolean,
  isNodeSubscribed: boolean,
  isNodeSyncing: boolean,
  isNodeInSync: boolean,
  isNodeTimeCorrect: boolean,
  nodeConnectionError: ?LocalizableError,
  isConnected: boolean,
  isSynced: boolean,
  syncPercentage: number,
  localTimeDifference: ?number,
  isSystemTimeIgnored: boolean,
  isSystemTimeCorrect: boolean,
  isForceCheckingNodeTime: boolean,
  latestLocalBlockTimestamp: number,
  latestNetworkBlockTimestamp: number,
  localBlockHeight: number,
  networkBlockHeight: number,
  onForceCheckLocalTimeDifference: Function,
  onOpenExternalLink: Function,
  onRestartNode: Function,
  onClose: Function,
};

type State = {
  data: Array<{
    localBlockHeight: ?number,
    networkBlockHeight: ?number,
    time: number,
  }>,
  isNodeRestarting: boolean,
};

@observer
export default class NetworkStatus extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: Props) {
    super(props);
    let { localBlockHeight, networkBlockHeight } = props;
    localBlockHeight = localBlockHeight || null;
    networkBlockHeight = networkBlockHeight || null;
    this.state = {
      data: [
        {
          localBlockHeight,
          networkBlockHeight,
          time: moment(Date.now() - 20000).format('HH:mm:ss'),
        },
        {
          localBlockHeight,
          networkBlockHeight,
          time: moment(Date.now() - 18000).format('HH:mm:ss'),
        },
        {
          localBlockHeight,
          networkBlockHeight,
          time: moment(Date.now() - 16000).format('HH:mm:ss'),
        },
        {
          localBlockHeight,
          networkBlockHeight,
          time: moment(Date.now() - 14000).format('HH:mm:ss'),
        },
        {
          localBlockHeight,
          networkBlockHeight,
          time: moment(Date.now() - 12000).format('HH:mm:ss'),
        },
        {
          localBlockHeight,
          networkBlockHeight,
          time: moment(Date.now() - 10000).format('HH:mm:ss'),
        },
        {
          localBlockHeight,
          networkBlockHeight,
          time: moment(Date.now() - 8000).format('HH:mm:ss'),
        },
        {
          localBlockHeight,
          networkBlockHeight,
          time: moment(Date.now() - 6000).format('HH:mm:ss'),
        },
        {
          localBlockHeight,
          networkBlockHeight,
          time: moment(Date.now() - 4000).format('HH:mm:ss'),
        },
        {
          localBlockHeight,
          networkBlockHeight,
          time: moment(Date.now() - 2000).format('HH:mm:ss'),
        },
      ],
      isNodeRestarting: false,
    };
  }

  componentWillMount() {
    syncingInterval = setInterval(this.syncingTimer, 2000);
  }

  componentWillReceiveProps(nextProps: Props) {
    const { cardanoNodeState } = this.props;
    const { cardanoNodeState: nextCardanoNodeState } = nextProps;
    const { isNodeRestarting } = this.state;
    const finalCardanoNodeStates = [
      CardanoNodeStates.RUNNING,
      CardanoNodeStates.STOPPED,
      CardanoNodeStates.UPDATED,
      CardanoNodeStates.CRASHED,
      CardanoNodeStates.ERRORED,
      CardanoNodeStates.UNRECOVERABLE,
    ];
    if (
      isNodeRestarting &&
      cardanoNodeState === CardanoNodeStates.STARTING &&
      includes(finalCardanoNodeStates, nextCardanoNodeState)
    ) {
      this.setState({ isNodeRestarting: false });
    }
  }

  componentWillUnmount() {
    this.resetSyncingTimer();
  }

  render() {
    const { intl } = this.context;

    const {
      systemInfo,
      coreInfo,
      cardanoNodeState,
      isNodeResponding,
      isNodeSubscribed,
      isNodeSyncing,
      isNodeInSync,
      isNodeTimeCorrect,
      isConnected,
      isSynced,
      syncPercentage,
      localTimeDifference,
      isSystemTimeCorrect,
      isForceCheckingNodeTime,
      localBlockHeight,
      networkBlockHeight,
      latestLocalBlockTimestamp,
      latestNetworkBlockTimestamp,
      onForceCheckLocalTimeDifference,
      onClose,
      nodeConnectionError,
      isSystemTimeIgnored,
      onOpenExternalLink,
      isDev,
      isTestnet,
      isStaging,
      isMainnet,
    } = this.props;

    const {
      platform,
      platformVersion,
      cpu,
      ram,
      availableDiskSpace,
    } = systemInfo;

    const {
      daedalusVersion,
      daedalusProcessID,
      daedalusMainProcessID,
      isInSafeMode,
      cardanoVersion,
      cardanoProcessID,
      cardanoAPIPort,
      cardanoNetwork,
      daedalusStateDirectory,
    } = coreInfo;

    const { isNodeRestarting } = this.state;
    const isNTPServiceReachable = localTimeDifference != null;
    const connectionError = get(nodeConnectionError, 'values', '{}');
    const { message, code } = connectionError;

    const localTimeDifferenceClasses = classNames([
      !isNTPServiceReachable ||
      (localTimeDifference && localTimeDifference > ALLOWED_TIME_DIFFERENCE)
        ? styles.red
        : styles.green,
    ]);

    const remainingUnsyncedBlocks = networkBlockHeight - localBlockHeight;
    const remainingUnsyncedBlocksClasses = classNames([
      remainingUnsyncedBlocks < 0 ||
      remainingUnsyncedBlocks > UNSYNCED_BLOCKS_ALLOWED
        ? styles.red
        : styles.green,
    ]);

    const latestLocalBlockAge = moment(Date.now()).diff(
      moment(latestLocalBlockTimestamp)
    );
    const isLocalBlockHeightStalling =
      latestLocalBlockAge > MAX_ALLOWED_STALL_DURATION;
    const latestLocalBlockAgeClasses = classNames([
      latestLocalBlockTimestamp > 0 && !isLocalBlockHeightStalling
        ? styles.green
        : styles.red,
    ]);

    const latestNetworkBlockAge = moment(Date.now()).diff(
      moment(latestNetworkBlockTimestamp)
    );
    const isNetworkBlockHeightStalling =
      latestNetworkBlockAge > MAX_ALLOWED_STALL_DURATION;
    const latestNetworkBlockAgeClasses = classNames([
      latestNetworkBlockTimestamp > 0 && !isNetworkBlockHeightStalling
        ? styles.green
        : styles.red,
    ]);

    // Cardano Node EKG server is not enabled for the Mainnet!
    const cardanoNodeEkgLink = isMainnet
      ? false
      : getNetworkEkgUrl({
          isDev,
          isStaging,
          isTestnet,
        });

    return (
      <div className={styles.component}>
        <div className={styles.tables}>
          <table className={styles.table}>
            <tbody>
              <tr>
                <th colSpan={2}>
                  {intl.formatMessage(messages.systemInfo)}
                  <hr />
                </th>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.platform)}:</td>
                <Tooltip skin={TooltipSkin} tip={platform}>
                  <td>{platform}</td>
                </Tooltip>
              </tr>
              <tr className={styles.platformVersion}>
                <td>{intl.formatMessage(messages.platformVersion)}:</td>
                <Tooltip
                  skin={TooltipSkin}
                  tip={platformVersion}
                  className={styles.platformTooltip}
                >
                  <td className={styles.platform}>{platformVersion}</td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cpu)}:</td>
                <Tooltip skin={TooltipSkin} tip={cpu}>
                  <td>{cpu}</td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.ram)}:</td>
                <Tooltip skin={TooltipSkin} tip={ram}>
                  <td>{ram}</td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.availableDiskSpace)}:</td>
                <Tooltip skin={TooltipSkin} tip={availableDiskSpace}>
                  <td>{availableDiskSpace}</td>
                </Tooltip>
              </tr>
              <tr>
                <th colSpan={2}>
                  {intl.formatMessage(messages.coreInfo)}
                  <hr />
                </th>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.daedalusVersion)}:</td>
                <Tooltip skin={TooltipSkin} tip={daedalusVersion}>
                  <td>{daedalusVersion}</td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.daedalusMainProcessID)}:</td>
                <Tooltip skin={TooltipSkin} tip={daedalusMainProcessID}>
                  <td>{daedalusMainProcessID}</td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.daedalusProcessID)}:</td>
                <Tooltip skin={TooltipSkin} tip={daedalusProcessID}>
                  <td>{daedalusProcessID}</td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.safeMode)}:</td>
                <td className={styles.safeMode}>
                  {isInSafeMode
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoVersion)}:</td>
                <Tooltip skin={TooltipSkin} tip={cardanoVersion}>
                  <td>{cardanoVersion}</td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoProcessID)}:</td>
                <Tooltip skin={TooltipSkin} tip={cardanoProcessID}>
                  <td>{cardanoProcessID}</td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoApiPort)}:</td>
                <Tooltip skin={TooltipSkin} tip={cardanoAPIPort}>
                  <td>{cardanoAPIPort}</td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoNetwork)}:</td>
                <Tooltip skin={TooltipSkin} tip={cardanoNetwork}>
                  <td>{cardanoNetwork}</td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.stateDirectory)}:</td>
                <Tooltip skin={TooltipSkin} tip={daedalusStateDirectory}>
                  <td className={styles.stateDirectory}>
                    {daedalusStateDirectory}
                  </td>
                </Tooltip>
              </tr>
              {!isConnected && nodeConnectionError ? (
                <tr>
                  <td className={styles.topPadding} colSpan={2}>
                    {intl.formatMessage(messages.connectionError)}
                    <br />
                    <Tooltip skin={TooltipSkin} tip={message}>
                      <div className={styles.error}>
                        message: {message || '-'}
                        <br />
                        code: {code || '-'}
                      </div>
                    </Tooltip>
                  </td>
                </tr>
              ) : null}
            </tbody>
          </table>

          <table className={styles.table}>
            <tbody>
              <tr>
                <th colSpan={2}>
                  {intl.formatMessage(messages.daedalusStatus)}
                  <hr />
                </th>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.connected)}:</td>
                <td className={this.getClass(isConnected)}>
                  {isConnected
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.synced)}:</td>
                <td className={this.getClass(isSynced)}>
                  {isSynced
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.syncPercentage)}:</td>
                <Tooltip
                  skin={TooltipSkin}
                  tip={`${syncPercentage.toFixed(2)}%`}
                >
                  <td>{syncPercentage.toFixed(2)}%</td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.networkBlockHeight)}:</td>
                <Tooltip skin={TooltipSkin} tip={networkBlockHeight}>
                  <td>{networkBlockHeight}</td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.localBlockHeight)}:</td>
                <Tooltip skin={TooltipSkin} tip={localBlockHeight}>
                  <td>{localBlockHeight}</td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.remainingUnsyncedBlocks)}:</td>
                <td className={remainingUnsyncedBlocksClasses}>
                  {remainingUnsyncedBlocks >= 0 ? remainingUnsyncedBlocks : '-'}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.latestLocalBlockAge)}:</td>
                <Tooltip
                  skin={TooltipSkin}
                  tip={
                    latestLocalBlockTimestamp >= 0
                      ? `${latestLocalBlockAge} ms`
                      : '-'
                  }
                >
                  <td className={latestLocalBlockAgeClasses}>
                    {latestLocalBlockTimestamp > 0
                      ? `${latestLocalBlockAge} ms`
                      : '-'}
                  </td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.latestNetworkBlockAge)}:</td>
                <Tooltip
                  skin={TooltipSkin}
                  tip={
                    latestNetworkBlockTimestamp > 0
                      ? `${latestNetworkBlockAge} ms`
                      : '-'
                  }
                >
                  <td className={latestNetworkBlockAgeClasses}>
                    {latestNetworkBlockTimestamp > 0
                      ? `${latestNetworkBlockAge} ms`
                      : '-'}
                  </td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.localTimeDifference)}:</td>
                <td className={styles.localTimeDifferenceItem}>
                  <button
                    onClick={() => onForceCheckLocalTimeDifference()}
                    disabled={isForceCheckingNodeTime || !isConnected}
                  >
                    {isForceCheckingNodeTime
                      ? intl.formatMessage(messages.localTimeDifferenceChecking)
                      : intl.formatMessage(
                          messages.localTimeDifferenceCheckTime
                        )}
                  </button>
                  <Tooltip
                    skin={TooltipSkin}
                    tip={
                      isNTPServiceReachable
                        ? `${localTimeDifference || 0} μs`
                        : intl.formatMessage(messages.serviceUnreachable)
                    }
                  >
                    <span className={localTimeDifferenceClasses}>
                      {isNTPServiceReachable
                        ? `${localTimeDifference || 0} μs`
                        : intl.formatMessage(messages.serviceUnreachable)}
                    </span>
                  </Tooltip>
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.systemTimeCorrect)}:</td>
                <td className={this.getClass(isSystemTimeCorrect)}>
                  {isSystemTimeCorrect
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.systemTimeIgnored)}:</td>
                <td className={this.getClass(!isSystemTimeIgnored)}>
                  {isSystemTimeIgnored
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.checkingNodeTime)}:</td>
                <td>
                  {isForceCheckingNodeTime
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <th colSpan={2}>
                  {intl.formatMessage(messages.cardanoNodeStatus)}
                  <button
                    className={styles.statusBtn}
                    onClick={() => this.restartNode()}
                    disabled={isNodeRestarting}
                  >
                    {isNodeRestarting
                      ? intl.formatMessage(messages.cardanoNodeStatusRestarting)
                      : intl.formatMessage(messages.cardanoNodeStatusRestart)}
                  </button>
                  <hr />
                </th>
              </tr>
              {cardanoNodeEkgLink ? (
                <tr>
                  <td>
                    {intl.formatMessage(messages.cardanoNodeDiagnostics)}:
                  </td>
                  <td>
                    <button
                      className={styles.realTimeStatusBtn}
                      onClick={() => onOpenExternalLink(cardanoNodeEkgLink)}
                    >
                      {intl.formatMessage(messages.realtimeStatisticsMonitor)}
                    </button>
                  </td>
                </tr>
              ) : null}
              <tr>
                <td>{intl.formatMessage(messages.cardanoNodeState)}:</td>
                <Tooltip
                  skin={TooltipSkin}
                  tip={upperFirst(
                    cardanoNodeState != null
                      ? intl.formatMessage(
                          this.getLocalisationForCardanoNodeState()
                        )
                      : 'unknown'
                  )}
                >
                  <td>
                    {upperFirst(
                      cardanoNodeState != null
                        ? intl.formatMessage(
                            this.getLocalisationForCardanoNodeState()
                          )
                        : 'unknown'
                    )}
                  </td>
                </Tooltip>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoNodeResponding)}:</td>
                <td className={this.getClass(isNodeResponding)}>
                  {isNodeResponding
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoNodeSubscribed)}:</td>
                <td className={this.getClass(isNodeSubscribed)}>
                  {isNodeSubscribed
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoNodeTimeCorrect)}:</td>
                <td className={this.getClass(isNodeTimeCorrect)}>
                  {isNodeTimeCorrect
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoNodeSyncing)}:</td>
                <td className={this.getClass(isNodeSyncing)}>
                  {isNodeSyncing
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoNodeInSync)}:</td>
                <td className={this.getClass(isNodeInSync)}>
                  {isNodeInSync
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
            </tbody>
          </table>
        </div>

        <button className={styles.closeButton} onClick={() => onClose()}>
          <SVGInline svg={closeCross} />
        </button>
      </div>
    );
  }

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

  restartNode = () => {
    this.setState({ isNodeRestarting: true });
    this.props.onRestartNode();
  };

  getClass = (isTrue: boolean) =>
    classNames([isTrue ? styles.green : styles.red]);

  syncingTimer = () => {
    const { localBlockHeight, networkBlockHeight } = this.props;
    const { data } = this.state;
    data.push({
      localBlockHeight,
      networkBlockHeight,
      time: moment().format('HH:mm:ss'),
    });
    this.setState({ data: data.slice(-10) });
  };

  resetSyncingTimer = () => {
    if (syncingInterval !== null) {
      clearInterval(syncingInterval);
      syncingInterval = null;
    }
  };
}
