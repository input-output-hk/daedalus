// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { ROUTES } from '../routes-config';
import CenteredLayout from '../components/layout/CenteredLayout';
import type { InjectedProps } from '../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class NetworkStatusPage extends Component<InjectedProps> {

  render() {
    const { stores } = this.props;
    const {
      // Node state
      isNodeResponding, isNodeSubscribed, isNodeSyncing, isNodeInSync, isNodeTimeCorrect,
      // Application state
      isConnected, isSynced, syncPercentage, hasBeenConnected,
      localTimeDifference, isSystemTimeCorrect, isForceCheckingNodeTime,
      forceCheckLocalTimeDifference, isSystemTimeChanged,
      localBlockHeight, networkBlockHeight,
    } = stores.networkStatus;
    return (
      <CenteredLayout>
        <table style={{ width: '50%' }}>
          <tbody>
            <tr>
              <th colSpan="2">
                NODE STATUS<hr />
              </th>
            </tr>
            <tr>
              <td style={{ width: '60%' }}>isNodeResponding:</td>
              <td>{isNodeResponding ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <td style={{ width: '60%' }}>isNodeSubscribed:</td>
              <td>{isNodeSubscribed ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <td style={{ width: '60%' }}>isNodeTimeCorrect:</td>
              <td>{isNodeTimeCorrect ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <td style={{ width: '60%' }}>isNodeSyncing:</td>
              <td>{isNodeSyncing ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <td style={{ width: '60%' }}>isNodeInSync:</td>
              <td>{isNodeInSync ? 'YES' : 'NO'}</td>
            </tr>
            <tr>
              <th colSpan="2" style={{ paddingTop: 50 }}>
                APPLICATION STATUS<hr />
              </th>
            </tr>
            <tr>
              <td style={{ width: '60%' }}>isConnected:</td>
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
                {localTimeDifference} μs |&nbsp;
                <button
                  onClick={() => forceCheckLocalTimeDifference()}
                  disabled={isForceCheckingNodeTime}
                >
                  Check time
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
              <th colSpan="2" style={{ paddingTop: 30 }}>
                <button
                  onClick={() => this.closeNetworkStatus()}
                >
                  Close
                </button>
              </th>
            </tr>
          </tbody>
        </table>
      </CenteredLayout>
    );
  }

  closeNetworkStatus = () => {
    const { actions } = this.props;
    actions.router.goToRoute.trigger({ route: ROUTES.ROOT });
  };

}
