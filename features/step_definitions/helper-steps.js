export default function () {
  const oneHour = 60 * 60 * 1000;
  // Helper step to pause execution for up to an hour ;)
  this.When(/^I freeze$/, { timeout: oneHour }, (callback) => {
    setTimeout(callback, oneHour);
  });
}
