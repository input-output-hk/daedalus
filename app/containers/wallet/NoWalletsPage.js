// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';

@inject('stores', 'actions') @observer
export default class NoWalletsPage extends Component {

  render() {
    return (
      <Layout>
        <div></div>
      </Layout>
    );
  }
}
