// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import CenteredLayout from '../components/layout/CenteredLayout';
import Loading from '../components/loading/Loading';

@observer
export default class LoadingPage extends Component {
  render() {
    return (
      <CenteredLayout>
        <Loading />
      </CenteredLayout>
    );
  }
}
