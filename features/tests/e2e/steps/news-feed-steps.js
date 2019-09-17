import { Given } from 'cucumber';
import newsDummyJson from '../../../../source/renderer/app/config/news.dummy';

Given('there are unread news', async function() {
  // Prepare news in Daedalus client
  const newsFeedData = await this.client.executeAsync((news, done) => {
    const newsFeed = daedalus.stores.newsFeed;
    // Set dummy news feed data
    daedalus.api.ada.setFakeNewsFeedJsonForTesting(news);
    // Refresh the news feed request & store
    newsFeed.getNews().then(() => done(newsFeed.newsFeedData));
  }, newsDummyJson);
  if (newsFeedData.value) {
    // Provide the news feed data from the store to the other steps
    this.news = newsFeedData.value;
  }
});

Given('there are no unread news', async function() {
  // Prepare news in Daedalus client
  const newsFeedData = await this.client.executeAsync((news, done) => {
    const newsFeed = daedalus.stores.newsFeed;
    const api = daedalus.api;
    // Set dummy news feed data
    api.ada.setFakeNewsFeedJsonForTesting(news);
    // Mark all news as read
    api.localStorage
      .markNewsAsRead(news.items.map(i => i.date))
      // Refresh the news feed request & store
      .then(newsFeed.getNews)
      // Return the computed news feed data
      .then(() => done(newsFeed.newsFeedData));
  }, newsDummyJson);

  // Provide the news feed data from the store to the other steps
  if (newsFeedData.value) {
    this.news = newsFeedData.value;
  }
});

Given('there is no news', async function() {
  // Prepare news in Daedalus client
  await this.client.executeAsync(done => {
    // Set dummy news feed data
    daedalus.api.ada.setFakeNewsFeedJsonForTesting({
      updatedAt: Date.now(),
      items: [],
    });
    // Refresh the news feed request & store
    daedalus.stores.newsFeed.getNews().then(done);
  });
  this.news = [];
});

Given('there are 5 read news', async function() {
  // Prepare news in Daedalus client
  const newsFeedData = await this.client.executeAsync((news, done) => {
    const newsFeed = daedalus.stores.newsFeed;
    const items = news.items.slice(0, 5);
    const api = daedalus.api;
    // Set dummy news feed data
    api.ada.setFakeNewsFeedJsonForTesting({
      updatedAt: Date.now(),
      items,
    });
    // Mark all news as read
    api.localStorage
      .markNewsAsRead(items.map(i => i.date))
      // Refresh the news feed request & store
      .then(newsFeed.getNews)
      // Return the computed news feed data
      .then(() => done(newsFeed.newsFeedData));
  }, newsDummyJson);

  // Provide the news feed data from the store to the other steps
  if (newsFeedData.value) {
    this.news = newsFeedData.value;
  }
});

Given('there is an incident', async function() {
  const newsIncident = newsDummyJson.items.find(i => i.type === 'incident');
  const newsFeedData = await this.client.executeAsync((incident, done) => {
    const newsFeed = daedalus.stores.newsFeed;
    daedalus.api.ada.setFakeNewsFeedJsonForTesting({
      updatedAt: Date.now(),
      items: [incident],
    });
    // Refresh the news feed request & store
    newsFeed.getNews().then(() => done(newsFeed.newsFeedData));
  }, newsIncident);
  // Provide the news feed data from the store to the other steps
  if (newsFeedData.value) {
    this.news = newsFeedData.value;
  }
});

Given('there are unread alerts', async function() {
  const unreadAlerts = newsDummyJson.items.filter(i => i.type === 'alert');
  const newsFeedData = await this.client.executeAsync((alerts, done) => {
    const newsFeed = daedalus.stores.newsFeed;
    daedalus.api.ada.setFakeNewsFeedJsonForTesting({
      updatedAt: Date.now(),
      items: alerts,
    });
    // Refresh the news feed request & store
    newsFeed.getNews().then(() => done(newsFeed.newsFeedData));
  }, unreadAlerts);
  // Provide the news feed data from the store to the other steps
  if (newsFeedData.value) {
    this.news = newsFeedData.value;
  }
});

Given('there is 1 unread {word}', async function(type) {
  const newsAnnouncement = newsDummyJson.items.find(i => i.type === type);
  const newsFeedData = await this.client.executeAsync((announcement, done) => {
    const newsFeed = daedalus.stores.newsFeed;
    daedalus.api.ada.setFakeNewsFeedJsonForTesting({
      updatedAt: Date.now(),
      items: [announcement],
    });
    // Refresh the news feed request & store
    newsFeed.getNews().then(() => done(newsFeed.newsFeedData));
  }, newsAnnouncement);
  // Provide the news feed data from the store to the other steps
  if (newsFeedData.value) {
    this.news = newsFeedData.value;
  }
  console.log(this.news);
});

Given('the news feed server is unreachable', async function() {
  this.news = [];
});
