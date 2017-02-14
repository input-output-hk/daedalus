// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import LoadingSpinner from '../widgets/LoadingSpinner';

@observer
export default class Loading extends Component {
  render() {
    return (
      <div style={{ display: 'flex', alignItems: 'center', flexDirection: 'column' }}>
        <div style={{ flex: 1 }} />
        <div style={{ fontFamily: 'SFUIDisplay-Regular', color: '#2f496e' }}>Joining network...</div>
        <div style={{ flex: 1 }} ><LoadingSpinner /></div>
      </div>
    );
  }
}
