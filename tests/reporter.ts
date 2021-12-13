const reporter = require('cucumber-html-reporter');

const options = {
  theme: 'bootstrap',
  jsonFile: 'tests-report/report-data.json',
  output: 'tests-report/report.html',
  reportSuiteAsScenarios: true,
  scenarioTimestamp: true,
  launchReport: true
};
reporter.generate(options);