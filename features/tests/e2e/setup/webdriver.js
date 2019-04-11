import { Before } from 'cucumber';

Before(function() {
  this.waitAndClick = async (selector, ...waitArgs) => {
    await this.client.waitForVisible(selector, ...waitArgs);
    return this.client.click(selector);
  };
});
