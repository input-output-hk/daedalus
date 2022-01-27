import { expect } from "chai";
import { get } from "lodash";
import { Given, When, Then } from "cucumber";
import moment from "moment";
import newsDummyJson from "../documents/dummy-news.json";
import { expectTextInSelector, getVisibleElementsCountForSelector } from "../../../common/e2e/steps/helpers";

async function prepareFakeNews(context, fakeNews, preparation, ...args) {
  // Run custom preparation logic
  await context.client.executeAsync(preparation, fakeNews, ...args);
  // Extract the computed newsfeed data from the store
  const newsData = await context.client.executeAsync(done => {
    const {
      newsFeed
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    } = daedalus.stores;
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
        incident: d.incident
      });
    });
  });

  if (newsData.value) {
    // Provide the newsfeed data from the store to the other steps
    context.news = newsData.value;
  }
}

async function prepareNewsOfType(context, type, count = null, markAsRead = false) {
  const items = newsDummyJson.items.filter(i => i.type === type).slice(0, count);
  const newsFeed = {
    updatedAt: Date.now(),
    items
  };
  await prepareFakeNews(context, newsFeed, (news, isRead, done) => {
    const {
      api
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    } = daedalus;
    api.ada.setTestingNewsFeed(news);

    if (isRead) {
      api.localStorage.markNewsAsRead(news.items.map(i => i.id)).then(done);
    } else {
      done();
    }
  }, markAsRead);
}

// Set newsfeed to open before each newsfeed step
export function setNewsFeedIsOpen(client: Record<string, any>, flag) {
  return client.execute(desiredState => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    if (daedalus.stores.app.newsFeedIsOpen !== desiredState) {
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      daedalus.actions.app.toggleNewsFeed.trigger();
    }
  }, flag);
}
// Reset the fake news before each newsfeed step
export function resetTestNews(client) {
  return client.executeAsync(done => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.setTestingNewsFeed({
      updatedAt: Date.now(),
      items: []
    });
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.stores.newsFeed.getNews().then(done);
  });
}
// GIVEN STEPS
Given(/^there (?:are|is)\s?(\d+)? (read|unread) (\w+?)s?$/, async function (count, read, newsType) {
  await prepareNewsOfType(this, newsType, parseInt(count || 2, 10), read === 'read');
});
Given('there is no news', async function () {
  await prepareFakeNews(this, newsDummyJson, (news, done) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.setTestingNewsFeed({
      updatedAt: Date.now(),
      items: []
    });
    done();
  });
});
Given('there is an incident', async function () {
  await prepareFakeNews(this, newsDummyJson, (news, done) => {
    const incident = news.items.find(i => i.type === 'incident');
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.setTestingNewsFeed({
      updatedAt: Date.now(),
      items: [incident]
    });
    done();
  });
});
Given('the newsfeed server is unreachable', async function () {
  this.client.executeAsync(done => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.setTestingNewsFeed(null);
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.stores.newsFeed.getNews().then(done);
  });
  this.news = [];
});
Given('the latest alert will cover the screen', async function () {
  const latestAlert = this.news.alerts.unread[0];
  await expectTextInSelector(this.client, {
    selector: '.AlertsOverlay_date',
    text: moment(latestAlert.date).format('YYYY-MM-DD')
  });
});
// WHEN STEPS
When('I click on the newsfeed icon', async function () {
  await this.waitAndClick('.NewsFeedIcon_component');
});
When('I open the newsfeed', async function () {
  await setNewsFeedIsOpen(this.client, true);
});
When('I dismiss the alert', async function () {
  this.dismissedAlert = get(this.news, ['alerts', 'unread', 0]);
  await this.waitAndClick('.AlertsOverlay_closeButton');
});
When(/^I click on the unread (\w+?) to expand it$/, async function (type) {
  setNewsFeedIsOpen(this.client, true);
  await this.waitAndClick(`.NewsItem_${type}`);
});
When('I click on the alert in the newsfeed', async function () {
  await this.waitAndClick('.NewsItem_alert.NewsItem_isRead');
});
// THEN STEPS
Then('I should see the newsfeed icon', async function () {
  await this.client.waitForVisible('.NewsFeedIcon_component');
});
Then('the newsfeed icon is highlighted', async function () {
  await this.client.waitForVisible('.NewsFeedIcon_notificationDot');
});
Then('the newsfeed icon is not highlighted', async function () {
  await this.client.waitForVisible('.NewsFeedIcon_notificationDot', null, true);
});
Then('the newsfeed is open', async function () {
  await this.client.waitForVisible('.NewsFeed_component');
});
Then('the newsfeed is empty', async function () {
  setNewsFeedIsOpen(this.client, true);
  await this.client.waitForVisible('.NewsFeed_newsFeedEmpty');
});
Then('no news are available', async function () {
  setNewsFeedIsOpen(this.client, true);
  await this.client.waitForVisible('.NewsFeed_newsFeedNoFetch');
});
Then('the incident will cover the screen', async function () {
  await this.client.waitForVisible('.IncidentOverlay_component');
});
Then('the alert disappears', async function () {
  await this.client.waitForVisible('.AlertsOverlay_component', null, true);
});
Then('the alert overlay opens', async function () {
  await this.client.waitForVisible('.AlertsOverlay_component');
});
Then(/^the newsfeed contains (\d+) read (\w+?)s$/, async function (expectedReadNewsCount, newsType) {
  setNewsFeedIsOpen(this.client, true);
  const readNewsCount = await getVisibleElementsCountForSelector(this.client, `.NewsItem_${newsType}.NewsItem_isRead`);
  expect(readNewsCount).to.equal(expectedReadNewsCount);
});
Then(/^the (\w+?) content is shown$/, async function (type) {
  setNewsFeedIsOpen(this.client, true);
  await this.client.waitForVisible(`.NewsItem_${type} .NewsItem_newsItemContentContainer`);
});
Then(/^the (\w+?) is marked as read$/, async function (type) {
  setNewsFeedIsOpen(this.client, true);
  await this.client.waitForVisible(`.NewsItem_${type}.NewsItem_isRead`);
});