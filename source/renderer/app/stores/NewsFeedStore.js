import { observable, action } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';

import type { GetNewsResponse } from '../api/news/types';

export default class NewsFeedStore extends Store {
  @observable
  getNewsRequest: Request<GetNewsResponse> = new Request(
    this.api.ada.getNews
  );

  setup() {
    const actions = this.actions.newsFeed;
    this.getNews();
  }

  getNews = async () => {
    const result = await this.getNewsRequest.execute().promise;
  };
}