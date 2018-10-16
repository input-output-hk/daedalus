// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get, includes, upperFirst } from 'lodash';
import moment from 'moment';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import {
  LineChart, YAxis, XAxis, Line,
  CartesianGrid, Tooltip, Legend,
  ResponsiveContainer,
} from 'recharts';
import {
  ALLOWED_TIME_DIFFERENCE,
  MAX_ALLOWED_STALL_DURATION,
} from '../../config/timingConfig';
import { UNSYNCED_BLOCKS_ALLOWED } from '../../config/numbersConfig';
import closeCross from '../../assets/images/close-cross.inline.svg';
import LocalizableError from '../../i18n/LocalizableError';
import { CardanoNodeStates } from '../../../../common/types/cardanoNode.types';
import styles from './NetworkStatus.scss';
import type { CardanoNodeState } from '../../../../common/types/cardanoNode.types';

let syncingInterval = null;

type Props = {
  cardanoNodeState: ?CardanoNodeState,
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
  isSystemTimeCorrect: boolean,
  isForceCheckingNodeTime: boolean,
  isSystemTimeChanged: boolean,
  mostRecentBlockTimestamp: number,
  localBlockHeight: number,
  networkBlockHeight: number,
  onForceCheckLocalTimeDifference: Function,
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
        { localBlockHeight, networkBlockHeight, time: moment(Date.now() - 20000).format('HH:mm:ss') },
        { localBlockHeight, networkBlockHeight, time: moment(Date.now() - 18000).format('HH:mm:ss') },
        { localBlockHeight, networkBlockHeight, time: moment(Date.now() - 16000).format('HH:mm:ss') },
        { localBlockHeight, networkBlockHeight, time: moment(Date.now() - 14000).format('HH:mm:ss') },
        { localBlockHeight, networkBlockHeight, time: moment(Date.now() - 12000).format('HH:mm:ss') },
        { localBlockHeight, networkBlockHeight, time: moment(Date.now() - 10000).format('HH:mm:ss') },
        { localBlockHeight, networkBlockHeight, time: moment(Date.now() - 8000).format('HH:mm:ss') },
        { localBlockHeight, networkBlockHeight, time: moment(Date.now() - 6000).format('HH:mm:ss') },
        { localBlockHeight, networkBlockHeight, time: moment(Date.now() - 4000).format('HH:mm:ss') },
        { localBlockHeight, networkBlockHeight, time: moment(Date.now() - 2000).format('HH:mm:ss') },
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
      CardanoNodeStates.UNRECOVERABLE
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
      cardanoNodeState, isNodeResponding, isNodeSubscribed, isNodeSyncing, isNodeInSync,
      isNodeTimeCorrect, isConnected, isSynced, syncPercentage, hasBeenConnected,
      localTimeDifference, isSystemTimeCorrect, isForceCheckingNodeTime,
      isSystemTimeChanged, mostRecentBlockTimestamp, localBlockHeight, networkBlockHeight,
      onForceCheckLocalTimeDifference, onClose, nodeConnectionError,
    } = this.props;
    const { data, isNodeRestarting } = this.state;
    const isNTPServiceReachable = !!localTimeDifference;
    const connectionError = get(nodeConnectionError, 'values', '{}');
    const { message, code } = connectionError;

    const localTimeDifferenceClasses = classNames([
      (
        !isNTPServiceReachable ||
        (localTimeDifference && (localTimeDifference > ALLOWED_TIME_DIFFERENCE))
      ) ? styles.red : styles.green,
    ]);

    const remainingUnsyncedBlocks = networkBlockHeight - localBlockHeight;
    const remainingUnsyncedBlocksClasses = classNames([
      (
        remainingUnsyncedBlocks < 0 ||
        remainingUnsyncedBlocks > UNSYNCED_BLOCKS_ALLOWED
      ) ? styles.red : styles.green,
    ]);

    const timeSinceLastBlock = moment(Date.now()).diff(moment(mostRecentBlockTimestamp));
    const isBlockchainHeightStalling = timeSinceLastBlock > MAX_ALLOWED_STALL_DURATION;
    const timeSinceLastBlockClasses = classNames([
      mostRecentBlockTimestamp > 0 && !isBlockchainHeightStalling ? styles.green : styles.red,
    ]);

    return (
      <div className={styles.component}>
        <div className={styles.tables}>
          <table className={styles.table}>
            <tbody>
              <tr>
                <th colSpan={2}>
                  DAEDALUS STATUS<hr />
                </th>
              </tr>
              <tr>
                <td>isConnected:</td>
                <td className={this.getClass(isConnected)}>
                  {isConnected ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>hasBeenConnected:</td>
                <td>
                  {hasBeenConnected ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>isSynced:</td>
                <td className={this.getClass(isSynced)}>
                  {isSynced ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>syncPercentage:</td>
                <td>{syncPercentage.toFixed(2)}%</td>
              </tr>
              <tr>
                <td>localBlockHeight:</td>
                <td>{localBlockHeight}</td>
              </tr>
              <tr>
                <td>networkBlockHeight:</td>
                <td>{networkBlockHeight}</td>
              </tr>
              <tr>
                <td>remainingUnsyncedBlocks:</td>
                <td className={remainingUnsyncedBlocksClasses}>
                  {remainingUnsyncedBlocks >= 0 ? remainingUnsyncedBlocks : '-'}
                </td>
              </tr>
              <tr>
                <td>timeSinceLastNetworkBlockChange:</td>
                <td className={timeSinceLastBlockClasses}>
                  {mostRecentBlockTimestamp > 0 ? `${timeSinceLastBlock} ms` : '-'}
                </td>
              </tr>
              <tr>
                <td>localTimeDifference:</td>
                <td>
                  <span className={localTimeDifferenceClasses}>
                    {isNTPServiceReachable ? (
                      `${localTimeDifference || 0} μs`
                    ) : (
                      'NTP service unreachable'
                    )}
                  </span> |&nbsp;
                  <button
                    onClick={() => onForceCheckLocalTimeDifference()}
                    disabled={isForceCheckingNodeTime}
                  >
                    {isForceCheckingNodeTime ? 'Checking...' : 'Check time'}
                  </button>
                </td>
              </tr>
              <tr>
                <td>isSystemTimeCorrect:</td>
                <td className={this.getClass(isSystemTimeCorrect)}>
                  {isSystemTimeCorrect ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>isSystemTimeChanged:</td>
                <td className={this.getClass(!isSystemTimeChanged)}>
                  {isSystemTimeChanged ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>isForceCheckingNodeTime:</td>
                <td>
                  {isForceCheckingNodeTime ? 'YES' : 'NO'}
                </td>
              </tr>
            </tbody>
          </table>

          <table className={styles.table}>
            <tbody>
              <tr>
                <th colSpan={2}>
                  CARDANO NODE STATUS<hr />
                </th>
              </tr>
              <tr>
                <td>cardanoNodeState:</td>
                <td>
                  {upperFirst(cardanoNodeState != null ? cardanoNodeState : 'unknown')}
                </td>
              </tr>
              <tr>
                <td>isNodeResponding:</td>
                <td className={this.getClass(isNodeResponding)}>
                  {isNodeResponding ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>isNodeSubscribed:</td>
                <td className={this.getClass(isNodeSubscribed)}>
                  {isNodeSubscribed ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>isNodeTimeCorrect:</td>
                <td className={this.getClass(isNodeTimeCorrect)}>
                  {isNodeTimeCorrect ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>isNodeSyncing:</td>
                <td className={this.getClass(isNodeSyncing)}>
                  {isNodeSyncing ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td>isNodeInSync:</td>
                <td className={this.getClass(isNodeInSync)}>
                  {isNodeInSync ? 'YES' : 'NO'}
                </td>
              </tr>
              <tr>
                <td className={styles.topPadding}>Cardano Node actions:</td>
                <td className={styles.topPadding}>
                  <button
                    onClick={() => this.restartNode()}
                    disabled={isNodeRestarting}
                  >
                    {isNodeRestarting ? 'Restarting...' : 'Restart'}
                  </button>
                </td>
              </tr>
              {!isConnected && nodeConnectionError ? (
                <tr>
                  <td className={styles.topPadding} colSpan={2}>
                    Connection error:<br />
                    <div className={styles.error}>
                      message: {message || '-'}<br />
                      code: {code || '-'}
                    </div>
                  </td>
                </tr>
              ) : null}
            </tbody>
          </table>
        </div>

        <ResponsiveContainer width="100%" height="50%">
          <LineChart data={data}>
            <XAxis
              dataKey="time"
              domain={['auto', 'auto']}
              name="Time"
            />
            <YAxis
              domain={[dataMin => (Math.max(0, dataMin - 20)), dataMax => (dataMax + 20)]}
              orientation="right"
              type="number"
              width={100}
            />
            <CartesianGrid stroke="#eee" strokeDasharray="5 5" />
            <Tooltip />
            <Legend wrapperStyle={{ color: '#fff' }} />
            <Line type="linear" dataKey="localBlockHeight" stroke="#8884d8" />
            <Line type="linear" dataKey="networkBlockHeight" stroke="#82ca9d" />
          </LineChart>
        </ResponsiveContainer>

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

  getClass = (isTrue: boolean) => (
    classNames([
      isTrue ? styles.green : styles.red,
    ])
  );

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
