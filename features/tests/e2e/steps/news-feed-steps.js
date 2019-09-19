import { expect } from 'chai';
import { Given, When, Then } from 'cucumber';
import moment from 'moment';

import newsDummyJson from '../../../../source/renderer/app/config/news.dummy';
import {
  expectTextInSelector,
  getVisibleElementsCountForSelector,
} from '../helpers/shared-helpers';

async function prepareFakeNews(context, fakeNews, preparation, ...args) {
  // Run custom preparation logic
  await context.client.executeAsync(preparation, fakeNews, ...args);
  // Extract the computed newsfeed data from the store
  const newsData = await context.client.executeAsync(done => {
    const newsFeed = daedalus.stores.newsFeed;
    // Refresh the newsfeed request & store
    newsFeed.getNews().then(() => {
      const d = newsFeed.newsFeedData;
      done({
        all: d.all,
        read: d.read,
        unread: d.unread,
        alerts: d.alerts,
        infos: d.infos,
        announcements: d.announcements,
        incident: d.incident,
      });
    });
  });
  if (newsData.value) {
    // Provide the newsfeed data from the store to the other steps
    context.news = newsData.value;
  }
}

async function prepareNewsOfType(
  context,
  type,
  count = null,
  markAsRead = false
) {
  const items = newsDummyJson.items
    .filter(i => i.type === type)
    .slice(0, count);
  const newsFeed = {
    updatedAt: Date.now(),
    items,
  };
  await prepareFakeNews(
    context,
    newsFeed,
    (news, isRead, done) => {
      const api = daedalus.api;
      api.ada.setFakeNewsFeedJsonForTesting(news);
      if (isRead) {
        api.localStorage.markNewsAsRead(news.items.map(i => i.date)).then(done);
      } else {
        done();
      }
    },
    markAsRead
  );
}

Given(/^there (?:are|is)\s?(\d+)? (read|unread) (\w+?)s?$/, async function(
  count,
  read,
  newsType
) {
  await prepareNewsOfType(
    this,
    newsType,
    parseInt(count || 2),
    read === 'read'
  );
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
  this.dismissedAlert = this.news.alerts.unread[0];
  await this.waitAndClick('.AlertsOverlay_closeButton');
});

When(/^I click on the unread (\w+?) to expand it$/, async function(type) {
  await this.waitAndClick(`.NewsItem_component.${type}`);
});

Then('i should see the newsfeed icon', async function() {
  await this.client.waitForVisible('.NewsFeedIcon_component');
});

Then('the newsfeed icon is highlighted', async function() {
  await this.client.waitForVisible('.NewsFeedIcon_withDot');
});

Then('the newsfeed icon is not highlighted', async function() {
  await this.client.waitForVisible('.NewsFeedIcon_withDot', null, true);
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

Then('the alert disappears', async function() {
  await this.client.waitForVisible('.AlertsOverlay_component', null, true);
});

Then(/^the newsfeed contains (\d+) read (\w+?)s$/, async function(
  expectedReadNewsCount,
  newsType
) {
  const readNewsCount = await getVisibleElementsCountForSelector(
    this.client,
    `.NewsItem_isRead.${newsType}`
  );
  expect(readNewsCount).to.equal(expectedReadNewsCount);
});

Then(/^the (\w+?) content is shown$/, async function(type) {
  await this.client.waitForVisible(
    `.NewsItem_component.${type} .NewsItem_newsItemContentContainer`
  );
});

Then(/^the (\w+?) is marked as read$/, async function(type) {
  await this.client.waitForVisible(`.NewsItem_isRead.${type}`);
});
