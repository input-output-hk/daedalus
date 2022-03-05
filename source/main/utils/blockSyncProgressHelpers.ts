import moment, { Moment } from 'moment';

export function isItFreshLog(applicationStartDate: Moment, line: string) {
  const [, logDate] = line.match(
    /\[(\d{4}-\d{2}-\d{2}\s\d{2}:\d{2}:\d{2}\.\d+) UTC]/
  );
  return applicationStartDate.isBefore(moment.utc(logDate));
}
