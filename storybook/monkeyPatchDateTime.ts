// freezing the date is required for producing consistent snapshots
// for visual regression testing with Chromatic
if (process.env.STORYBOOK_FREEZE_DATE === 'true') {
  const timemachine = require('timemachine');

  timemachine.config({
    dateString: 'Sat, 01 Jan 2022 10:00:00 GMT',
  });
}
