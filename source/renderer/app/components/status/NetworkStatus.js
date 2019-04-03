// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get, includes, upperFirst } from 'lodash';
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
  hasBeenConnected: boolean,
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
      hasBeenConnected,
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
      availableDiskSpace
    } = systemInfo;

    const {
      isInSafeMode,
      daedalusVersion,
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
                  SYSTEM INFO
                  <hr />
                </th>
              </tr>
              <tr>
                <td>Platform:</td>
                <td>{platform}</td>
              </tr>
              <tr>
                <td>Platform Version:</td>
                <td className={styles.platform}>{platformVersion}</td>
              </tr>
              <tr>
                <td>CPU:</td>
                <td>{cpu}</td>
              </tr>
              <tr>
                <td>RAM:</td>
                <td>{ram}</td>
              </tr>
              <tr>
                <td>Available disk space:</td>
                <td>{availableDiskSpace}</td>
              </tr>
              <tr>
                <th colSpan={2}>
                  CORE INFO
                  <hr />
                </th>
              </tr>
              <tr>
                <td>Daedalus Version:</td>
                <td>{daedalusVersion}</td>
              </tr>
              <tr>
                <td>Daedalus Process ID:</td>
                <td></td>
              </tr>
              <tr>
                <td>Daedalus is running in safe mode:</td>
                <td>{isInSafeMode ? 'YES' : 'NO'}</td>
              </tr>
              <tr>
                <td>Cardano Version:</td>
                <td></td>
              </tr>
              <tr>
                <td>Cardano Process ID:</td>
                <td></td>
              </tr>
              <tr>
                <td>Cardano API Port:</td>
                <td></td>
              </tr>
              <tr>
                <td>Cardano Network:</td>
                <td></td>
              </tr>
              <tr>
                <td>Daedalus State Directory:</td>
                <td></td>
              </tr>
              {!isConnected && nodeConnectionError ? (
                <tr>
                  <td className={styles.topPadding} colSpan={2}>
                    CONNECTION ERROR
                    <br />
                    <div className={styles.error}>
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
                  DAEDALUS STATUS
                  <hr />
                </th>
              </tr>
              <tr>
                <td>Connected:</td>
                <td className={this.getClass(isConnected)}>
                  {isConnected ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>Has Been Connected:</td>
                <td>{hasBeenConnected ? 'YES' : 'NO'}</td>
              </tr>
              <tr>
                <td>Synced:</td>
                <td className={this.getClass(isSynced)}>
                  {isSynced ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>Sync Percentage:</td>
                <td>{syncPercentage.toFixed(2)}%</td>
              </tr>
              <tr>
                <td>Network Block Height:</td>
                <td>{networkBlockHeight}</td>
              </tr>
              <tr>
                <td>Local Block Height:</td>
                <td>{localBlockHeight}</td>
              </tr>
              <tr>
                <td>Remaining Unsynced Blocks:</td>
                <td className={remainingUnsyncedBlocksClasses}>
                  {remainingUnsyncedBlocks >= 0 ? remainingUnsyncedBlocks : '-'}
                </td>
              </tr>
              <tr>
                <td>Latest Local Block Age:</td>
                <td className={latestLocalBlockAgeClasses}>
                  {latestLocalBlockTimestamp > 0
                    ? `${latestLocalBlockAge} ms`
                    : '-'}
                </td>
              </tr>
              <tr>
                <td>Latest Network Block Age:</td>
                <td className={latestNetworkBlockAgeClasses}>
                  {latestNetworkBlockTimestamp > 0
                    ? `${latestNetworkBlockAge} ms`
                    : '-'}
                </td>
              </tr>
              <tr>
                <td>Local Time Difference:</td>
                <td>
                  <button
                    onClick={() => onForceCheckLocalTimeDifference()}
                    disabled={isForceCheckingNodeTime || !isConnected}
                  >
                    {isForceCheckingNodeTime ? 'Checking...' : 'Check time'}
                  </button>
                  <span className={localTimeDifferenceClasses}>
                    {isNTPServiceReachable
                      ? `${localTimeDifference || 0} μs`
                      : 'NTP service unreachable'}
                  </span>{' '}
                </td>
              </tr>
              <tr>
                <td>System Time Correct:</td>
                <td className={this.getClass(isSystemTimeCorrect)}>
                  {isSystemTimeCorrect ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>System Time Ignored:</td>
                <td className={this.getClass(!isSystemTimeIgnored)}>
                  {isSystemTimeIgnored ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>Force Checking Node Time:</td>
                <td>{isForceCheckingNodeTime ? 'YES' : 'NO'}</td>
              </tr>
              <tr>
                <th colSpan={2}>
                  CARDANO NODE STATUS
                  <button
                    className={styles.statusBtn}
                    onClick={() => this.restartNode()}
                    disabled={isNodeRestarting}
                  >
                    {isNodeRestarting
                      ? 'Restarting Cardano Node...'
                      : 'Restart Cardano Node'}
                  </button>
                  <hr />
                </th>
              </tr>
              <tr>
                <td>Cardano Node State:</td>
                <td>
                  {upperFirst(
                    cardanoNodeState != null ? cardanoNodeState : 'unknown'
                  )}
                </td>
              </tr>
              <tr>
                <td>Node Responding:</td>
                <td className={this.getClass(isNodeResponding)}>
                  {isNodeResponding ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>Node Subscribed:</td>
                <td className={this.getClass(isNodeSubscribed)}>
                  {isNodeSubscribed ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>Node Time Correct:</td>
                <td className={this.getClass(isNodeTimeCorrect)}>
                  {isNodeTimeCorrect ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>Node Syncing:</td>
                <td className={this.getClass(isNodeSyncing)}>
                  {isNodeSyncing ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>Node In Sync:</td>
                <td className={this.getClass(isNodeInSync)}>
                  {isNodeInSync ? 'YES' : 'NO'}
                </td>
              </tr>
              {cardanoNodeEkgLink ? (
                <tr>
                  <td>Cardano Node Diagnostics:</td>
                  <td>
                    <button
                      className={styles.realTimeStatusBtn}
                      onClick={() => onOpenExternalLink(cardanoNodeEkgLink)}
                    >
                      Realtime statistics monitor
                    </button>
                  </td>
                </tr>
              ) : null}
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
