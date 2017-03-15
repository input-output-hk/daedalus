export default function () {
  this.Before(function() {
    this.waitAndClick = async (selector, ...waitArgs) => {
      await this.client.waitForVisible(selector, ...waitArgs);
      await this.client.click(selector);
    }
  });
}
