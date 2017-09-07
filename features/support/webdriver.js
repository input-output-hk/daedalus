export default function () {
  this.Before(() => {
    this.waitAndClick = async (selector, ...waitArgs) => {
      await this.client.waitForVisible(selector, ...waitArgs);
      return this.client.click(selector);
    };
  });
}
