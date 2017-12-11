import { defineSupportCode } from 'cucumber';

defineSupportCode(({ Before }) => {
  Before(function () {
    this.waitAndClick = async (selector, ...waitArgs) => {
      await this.client.waitForVisible(selector, ...waitArgs);
      return this.client.click(selector);
    };
  });
});
