import { Given } from 'cucumber';

Given('there are unread news', async function() {
  await this.client.executeAsync(done => {
    daedalus.api.ada.setFakeNewsFeedJsonForTesting({
      updatedAt: 1568650464961,
      items: [
        {
          title: {
            'en-US': 'Some title 1 in English',
            'ja-JP': 'Some title 1 in Japanese',
          },
          content: {
            'en-US': 'Content 1 in English',
            'ja-JP': 'Content 1 in Japanese',
          },
          target: {
            daedalus: '0.14.0',
            platform: 'darwin',
            platformVersion: '17.7.0',
          },
          action: {
            label: {
              'en-US': 'Visit en-US',
              'ja-JP': 'Visit ja-JP',
            },
            url: {
              'en-US': 'https://iohk.zendesk.com/hc/en-us/articles/',
              'ja-JP': 'https://iohk.zendesk.com/hc/ja/articles/',
            },
          },
          date: 1568650464961,
          type: 'incident',
        },
      ],
    });
    daedalus.stores.newsFeed.getNewsRequest.execute().then(done);
  });
});
