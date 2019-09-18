import { Given, When, Then } from 'cucumber';
import newsDummyJson from '../../../../source/renderer/app/config/news.dummy';

async function prepareFakeNews(context, fakeNews, preparation, ...args) {
  // Run custom preparation logic
  await context.client.executeAsync(preparation, fakeNews, ...args);
  // Extract the computed news feed data from the store
  const newsData = await context.client.executeAsync(done => {
    const newsFeed = daedalus.stores.newsFeed;
    // Refresh the news feed request & store
    newsFeed.getNews().then(() => done(newsFeed.newsFeedData));
  });
  if (newsData.value) {
    // Provide the news feed data from the store to the other steps
    context.news = newsData.value;
  }
}

Given('there are unread news', async function() {
  await prepareFakeNews(this, newsDummyJson, (news, done) => {
    daedalus.api.ada.setFakeNewsFeedJsonForTesting(news);
    done();
  });
});

Given('there are no unread news', async function() {
  await prepareFakeNews(this, newsDummyJson, (news, done) => {
    const api = daedalus.api;
    // Set dummy news feed data
    api.ada.setFakeNewsFeedJsonForTesting(news);
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
    // Set dummy news feed data
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

Given('there are unread alerts', async function() {
  await prepareFakeNews(this, newsDummyJson, (news, done) => {
    const alerts = news.items.filter(i => i.type === 'alert');
    daedalus.api.ada.setFakeNewsFeedJsonForTesting({
      updatedAt: Date.now(),
      items: alerts,
    });
    done();
  });
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

Given('the news feed server is unreachable', async function() {
  this.news = [];
});

When('I click on the news feed icon', async function() {
  await this.waitAndClick('.NewsFeedIcon_component');
});

When('I open the news feed', async function() {
  await this.waitAndClick('.NewsFeedIcon_component');
});

Then('i should see the news feed icon', async function() {
  await this.client.waitForVisible('.NewsFeedIcon_component');
});

Then('the news feed icon is highlighted', async function() {
  await this.client.waitForVisible('.NewsFeedIcon_component_is-highlighted');
});

Then('the news feed icon is not highlighted', async function() {
  await this.client.waitForVisible(
    '.NewsFeedIcon_component_is-highlighted',
    null,
    true
  );
});

Then('the news feed is open', async function() {
  await this.client.waitForVisible('.NewsFeed_component');
});

Then('the news feed is empty', async function() {
  await this.client.waitForVisible('.NewsFeed_newsFeedNoFetch');
});
