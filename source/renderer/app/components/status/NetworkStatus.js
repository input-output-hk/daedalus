// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import moment from 'moment';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import {
  LineChart, YAxis, XAxis, Line,
  CartesianGrid, Tooltip, Legend,
  ResponsiveContainer,
} from 'recharts';
import closeCross from '../../assets/images/close-cross.inline.svg';
import LocalizableError from '../../i18n/LocalizableError';
import styles from './NetworkStatus.scss';

let syncingInterval = null;

type Props = {
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
  localBlockHeight: number,
  networkBlockHeight: number,
  onForceCheckLocalTimeDifference: Function,
  onClose: Function,
};

type State = {
  data: Array<{
    localBlockHeight: ?number,
    networkBlockHeight: ?number,
    time: number,
  }>,
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
    };
  }

  componentWillMount() {
    syncingInterval = setInterval(this.syncingTimer, 2000);
  }

  componentWillUnmount() {
    this.resetSyncingTimer();
  }

  render() {
    const {
      isNodeResponding, isNodeSubscribed, isNodeSyncing, isNodeInSync, isNodeTimeCorrect,
      isConnected, isSynced, syncPercentage, hasBeenConnected,
      localTimeDifference, isSystemTimeCorrect, isForceCheckingNodeTime,
      isSystemTimeChanged, localBlockHeight, networkBlockHeight,
      onForceCheckLocalTimeDifference, onClose, nodeConnectionError,
    } = this.props;
    const { data } = this.state;
    const isNTPServiceReachable = !!localTimeDifference;
    const connectionError = get(nodeConnectionError, 'values', '{}');
    const { message, code } = connectionError;

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
                <td>{syncPercentage}%</td>
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
                <td>{networkBlockHeight - localBlockHeight}</td>
              </tr>
              <tr>
                <td>localTimeDifference:</td>
                <td>
                  {isNTPServiceReachable ? (
                    `${localTimeDifference || 0} μs`
                  ) : (
                    'NTP service unreachable'
                  )} |&nbsp;
                  <button
                    onClick={() => onForceCheckLocalTimeDifference()}
                    disabled={isForceCheckingNodeTime}
                  >
                    Check again
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
              width={70}
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
