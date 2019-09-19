import { expect } from 'chai';
import { Given, When, Then } from 'cucumber';

import newsDummyJson from '../../../../source/renderer/app/config/news.dummy';
import {
  expectTextInSelector,
  getVisibleElementsCountForSelector,
  getVisibleElementsForSelector,
} from '../helpers/shared-helpers';
import moment from 'moment';

async function prepareFakeNews(context, fakeNews, preparation, ...args) {
  // Run custom preparation logic
  await context.client.executeAsync(preparation, fakeNews, ...args);
  // Extract the computed newsfeed data from the store
  const newsData = await context.client.executeAsync(done => {
    const newsFeed = daedalus.stores.newsFeed;
    // Refresh the newsfeed request & store
    newsFeed.getNews().then(() => done(newsFeed.newsFeedData));
  });
  if (newsData.value) {
    // Provide the newsfeed data from the store to the other steps
    context.news = newsData.value;
  }
}

function getLatestAlert(news) {
  // Reduce to latest alert by comparing timestamps
  const alerts = news.filter(i => i.type === 'alert');
  return alerts.reduce((a, b) => (a.date > b.date ? a : b));
}

Given('there are unread news', async function() {
  await prepareFakeNews(this, newsDummyJson, (news, done) => {
    daedalus.api.ada.setFakeNewsFeedJsonForTesting({
      updatedAt: Date.now(),
      items: news.items.filter(i => i.type === 'info'),
    });
    done();
  });
});

Given('there are no unread news', async function() {
  await prepareFakeNews(this, newsDummyJson, (news, done) => {
    const api = daedalus.api;
    // Set dummy newsfeed data
    api.ada.setFakeNewsFeedJsonForTesting({
      updatedAt: Date.now(),
      items: news.items.filter(i => i.type === 'info'),
    });
    // Mark all news as read
    api.localStorage.markNewsAsRead(news.items.map(i => i.date)).then(done);
  });
});

Given('there is no news', async function() {
  await prepareFakeNews(this, newsDummyJson, (news, done) => {
    daedalus.api.ada.setFakeNewsFeedJsonForTesting({
      updatedAt: Date.now(),
      items: [],
    });
    done();
  });
});

Given('there are 5 read news', async function() {
  await prepareFakeNews(this, newsDummyJson, (news, done) => {
    const api = daedalus.api;
    const items = news.items.filter(i => i.type !== 'incident').slice(0, 5);
    // Set dummy newsfeed data
    api.ada.setFakeNewsFeedJsonForTesting({
      updatedAt: Date.now(),
      items,
    });
    // Mark all news as read
    api.localStorage.markNewsAsRead(items.map(i => i.date)).then(done);
  });
});

Given('there is an incident', async function() {
  await prepareFakeNews(this, newsDummyJson, (news, done) => {
    const incident = news.items.find(i => i.type === 'incident');
    daedalus.api.ada.setFakeNewsFeedJsonForTesting({
      updatedAt: Date.now(),
      items: [incident],
    });
    done();
  });
});

Given('there is unread {word}s', async function(newsType) {
  await prepareFakeNews(
    this,
    newsDummyJson,
    (news, type, done) => {
      const items = news.items.filter(i => i.type === type);
      daedalus.api.ada.setFakeNewsFeedJsonForTesting({
        updatedAt: Date.now(),
        items,
      });
      done();
    },
    newsType
  );
});

Given('there is 1 unread {word}', async function(newsType) {
  await prepareFakeNews(
    this,
    newsDummyJson,
    (news, type, done) => {
      const newsItem = news.items.find(i => i.type === type);
      daedalus.api.ada.setFakeNewsFeedJsonForTesting({
        updatedAt: Date.now(),
        items: [newsItem],
      });
      done();
    },
    newsType
  );
});

Given('the newsfeed server is unreachable', async function() {
  this.news = [];
});

Given('the latest alert will cover the screen', async function() {
  const latestAlert = this.news.alerts.unread[0];
  await expectTextInSelector(this.client, {
    selector: '.AlertsOverlay_date',
    text: moment(latestAlert.date).format('YYYY-MM-DD'),
  });
});

When('I click on the newsfeed icon', async function() {
  await this.waitAndClick('.NewsFeedIcon_component');
});

When('I open the newsfeed', async function() {
  await this.waitAndClick('.NewsFeedIcon_component');
});

When('I dismiss the alert', async function() {
  this.dismissedAlert = getLatestAlert(this.news);
  await this.waitAndClick('.AlertsOverlay_component .closeButton');
});

Then('i should see the newsfeed icon', async function() {
  await this.client.waitForVisible('.NewsFeedIcon_component');
});

Then('the newsfeed icon is highlighted', async function() {
  await this.client.waitForVisible('.NewsFeedIcon_highlighted');
});

Then('the newsfeed icon shows a dot', async function() {
  await this.client.waitForVisible('.NewsFeedIcon_withDot');
});

Then('the newsfeed icon is not highlighted', async function() {
  await this.client.waitForVisible('.NewsFeedIcon_highlighted', null, true);
});

Then('the newsfeed is open', async function() {
  await this.client.waitForVisible('.NewsFeed_component');
});

Then('the newsfeed is empty', async function() {
  await this.client.waitForVisible('.NewsFeed_newsFeedEmpty');
});

Then('no news are available', async function() {
  await this.client.waitForVisible('.NewsFeed_newsFeedNoFetch');
});

Then('the incident will cover the screen', async function() {
  await this.client.waitForVisible('.IncidentOverlay_component');
});

Then('the alert I have dismissed becomes read', async function() {
  await this.client.waitForVisible('.AlertsOverlay_component', null, true);
  await expectTextInSelector(this.client, {
    // We check if the alert appears as "read" by selecting by class
    selector: '.alert.NewsItem_component_is-read .NewsItem_newsItemDate',
    text: moment(this.dismissedAlert.date).format('YYYY-MM-DD'),
  });
});

Then('the newsfeed contains {int} read news', async function(
  expectedReadNewsCount
) {
  const readNewsCount = await getVisibleElementsCountForSelector(
    this.client,
    '.NewsItem_component_is-read'
  );
  expect(readNewsCount).to.equal(expectedReadNewsCount);
});
