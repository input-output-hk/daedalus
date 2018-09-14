// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './NetworkStatus.scss';

type Props = {
  isNodeResponding: boolean,
  isNodeSubscribed: boolean,
  isNodeSyncing: boolean,
  isNodeInSync: boolean,
  isNodeTimeCorrect: boolean,
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

@observer
export default class NetworkStatus extends Component<Props> {

  render() {
    const {
      isNodeResponding, isNodeSubscribed, isNodeSyncing, isNodeInSync, isNodeTimeCorrect,
      isConnected, isSynced, syncPercentage, hasBeenConnected,
      localTimeDifference, isSystemTimeCorrect, isForceCheckingNodeTime,
      isSystemTimeChanged, localBlockHeight, networkBlockHeight,
      onForceCheckLocalTimeDifference, onClose,
    } = this.props;

    const isNTPServiceReachable = !!localTimeDifference;

    return (
      <div className={styles.component}>
        <table className={styles.table}>
          <tbody>
            <tr>
              <th colSpan="2">
                CARDANO NODE STATUS<hr />
              </th>
            </tr>
            <tr>
              <td>isNodeResponding:</td>
              <td>{isNodeResponding ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <td>isNodeSubscribed:</td>
              <td>{isNodeSubscribed ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <td>isNodeTimeCorrect:</td>
              <td>{isNodeTimeCorrect ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <td>isNodeSyncing:</td>
              <td>{isNodeSyncing ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <td>isNodeInSync:</td>
              <td>{isNodeInSync ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <th colSpan="2">
                DAEDALUS STATUS<hr />
              </th>
            </tr>
            <tr>
              <td>isConnected:</td>
              <td>{isConnected ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <td>hasBeenConnected:</td>
              <td>{hasBeenConnected ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <td>isSynced:</td>
              <td>{isSynced ? 'YES' : 'NO'}</td>
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
              <td>{isSystemTimeCorrect ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <td>isSystemTimeChanged:</td>
              <td>{isSystemTimeChanged ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <td>isForceCheckingNodeTime:</td>
              <td>{isForceCheckingNodeTime ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <th colSpan="2">
                <button onClick={() => onClose()}>
                  Close
                </button>
              </th>
            </tr>
          </tbody>
        </table>
      </div>
    );
  }

}
