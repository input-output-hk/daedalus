// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get, includes, upperFirst } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import classNames from 'classnames';
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
  daedalusProcessID: {
    id: 'status.network.dialog.daedalusProcessID',
    defaultMessage: '!!!Daedalus Process ID',
    description: 'Daedalus Process ID',
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
});

type Props = {
  systemInfo: Object,
  coreInfo: Object,
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
      isInSafeMode,
      cardanoVersion,
      cardanoProcessID,
      cardanoAPIPort,
      cardanoNetwork,
      daedalusStateDirectory,
    } = coreInfo;

    const { isNodeRestarting } = this.state;
    const isNTPServiceReachable = !!localTimeDifference;
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
                <td title={platform}>{platform}</td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.platformVersion)}:</td>
                <td className={styles.platform} title={platformVersion}>
                  {platformVersion}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cpu)}:</td>
                <td title={cpu}>{cpu}</td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.ram)}:</td>
                <td title={ram}>{ram}</td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.availableDiskSpace)}:</td>
                <td title={availableDiskSpace}>{availableDiskSpace}</td>
              </tr>
              <tr>
                <th colSpan={2}>
                  {intl.formatMessage(messages.coreInfo)}
                  <hr />
                </th>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.daedalusVersion)}:</td>
                <td title={daedalusVersion}>{daedalusVersion}</td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.daedalusProcessID)}:</td>
                <td title={daedalusProcessID}>{daedalusProcessID}</td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.safeMode)}:</td>
                <td
                  className={styles.safeMode}
                  title={isInSafeMode ? 'YES' : 'NO'}
                >
                  {isInSafeMode ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoVersion)}:</td>
                <td title={cardanoVersion}>{cardanoVersion}</td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoProcessID)}:</td>
                <td title={cardanoProcessID}>{cardanoProcessID}</td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoApiPort)}:</td>
                <td title={cardanoAPIPort}>{cardanoAPIPort}</td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoNetwork)}:</td>
                <td title={cardanoNetwork}>{cardanoNetwork}</td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.stateDirectory)}:</td>
                <td
                  className={styles.stateDirectory}
                  title={daedalusStateDirectory}
                >
                  {daedalusStateDirectory}
                </td>
              </tr>
              {!isConnected && nodeConnectionError ? (
                <tr>
                  <td className={styles.topPadding} colSpan={2}>
                    {intl.formatMessage(messages.connectionError)}
                    <br />
                    <div className={styles.error} title={message || '-'}>
                      message: {message || '-'}
                      <br />
                      code: {code || '-'}
                    </div>
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
                <td
                  className={this.getClass(isConnected)}
                  title={isConnected ? 'YES' : 'NO'}
                >
                  {isConnected ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.synced)}:</td>
                <td
                  className={this.getClass(isSynced)}
                  title={isSynced ? 'YES' : 'NO'}
                >
                  {isSynced ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.syncPercentage)}:</td>
                <td title={`${syncPercentage.toFixed(2)}%`}>
                  {syncPercentage.toFixed(2)}%
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.networkBlockHeight)}:</td>
                <td title={networkBlockHeight}>{networkBlockHeight}</td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.localBlockHeight)}:</td>
                <td title={localBlockHeight}>{localBlockHeight}</td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.remainingUnsyncedBlocks)}:</td>
                <td
                  className={remainingUnsyncedBlocksClasses}
                  title={
                    remainingUnsyncedBlocks >= 0 ? remainingUnsyncedBlocks : '-'
                  }
                >
                  {remainingUnsyncedBlocks >= 0 ? remainingUnsyncedBlocks : '-'}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.latestLocalBlockAge)}:</td>
                <td
                  className={latestLocalBlockAgeClasses}
                  title={
                    latestLocalBlockTimestamp > 0
                      ? `${latestLocalBlockAge} ms`
                      : '-'
                  }
                >
                  {latestLocalBlockTimestamp > 0
                    ? `${latestLocalBlockAge} ms`
                    : '-'}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.latestNetworkBlockAge)}:</td>
                <td
                  className={latestNetworkBlockAgeClasses}
                  title={
                    latestNetworkBlockTimestamp > 0
                      ? `${latestNetworkBlockAge} ms`
                      : '-'
                  }
                >
                  {latestNetworkBlockTimestamp > 0
                    ? `${latestNetworkBlockAge} ms`
                    : '-'}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.localTimeDifference)}:</td>
                <td>
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
                  <span
                    className={localTimeDifferenceClasses}
                    title={
                      isNTPServiceReachable
                        ? `${localTimeDifference || 0} μs`
                        : 'NTP service unreachable'
                    }
                  >
                    {isNTPServiceReachable
                      ? `${localTimeDifference || 0} μs`
                      : 'NTP service unreachable'}
                  </span>{' '}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.systemTimeCorrect)}:</td>
                <td
                  className={this.getClass(isSystemTimeCorrect)}
                  title={isSystemTimeCorrect ? 'YES' : 'NO'}
                >
                  {isSystemTimeCorrect ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.systemTimeIgnored)}:</td>
                <td
                  className={this.getClass(!isSystemTimeIgnored)}
                  title={isSystemTimeIgnored ? 'YES' : 'NO'}
                >
                  {isSystemTimeIgnored ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.checkingNodeTime)}:</td>
                <td title={isForceCheckingNodeTime ? 'YES' : 'NO'}>
                  {isForceCheckingNodeTime ? 'YES' : 'NO'}
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
                <td
                  title={upperFirst(
                    cardanoNodeState != null ? cardanoNodeState : 'unknown'
                  )}
                >
                  {upperFirst(
                    cardanoNodeState != null ? cardanoNodeState : 'unknown'
                  )}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoNodeResponding)}:</td>
                <td
                  className={this.getClass(isNodeResponding)}
                  title={isNodeResponding ? 'YES' : 'NO'}
                >
                  {isNodeResponding ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoNodeSubscribed)}:</td>
                <td
                  className={this.getClass(isNodeSubscribed)}
                  title={isNodeSubscribed ? 'YES' : 'NO'}
                >
                  {isNodeSubscribed ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoNodeTimeCorrect)}:</td>
                <td
                  className={this.getClass(isNodeTimeCorrect)}
                  title={isNodeTimeCorrect ? 'YES' : 'NO'}
                >
                  {isNodeTimeCorrect ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoNodeSyncing)}:</td>
                <td
                  className={this.getClass(isNodeSyncing)}
                  title={isNodeSyncing ? 'YES' : 'NO'}
                >
                  {isNodeSyncing ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>{intl.formatMessage(messages.cardanoNodeInSync)}:</td>
                <td
                  className={this.getClass(isNodeInSync)}
                  title={isNodeInSync ? 'YES' : 'NO'}
                >
                  {isNodeInSync ? 'YES' : 'NO'}
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
