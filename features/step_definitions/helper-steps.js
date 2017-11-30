import { When } from 'cucumber';

const oneHour = 60 * 60 * 1000;
// Helper step to pause execution for up to an hour ;)
When(/^I freeze$/, { timeout: oneHour }, (callback) => {
  setTimeout(callback, oneHour);
});

