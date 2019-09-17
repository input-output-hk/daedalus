// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import NewsFeed from '../../components/news/NewsFeed';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class NewsFeedContainer extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { newsFeedData, newsItems, newsUpdatedAt } = this.props.stores.newsFeed;
    console.debug('newsFeedData: ', {newsFeedData, newsItems, newsUpdatedAt});
    // return <NewsFeed news={newsFeedData} />;
    return <p>das</p>;
  }
}
