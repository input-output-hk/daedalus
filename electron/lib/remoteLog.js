import Log from 'electron-log';
import winston from 'winston';

// Requiring `winston-papertrail` will expose
// `winston.transports.Papertrail`
require('winston-papertrail').Papertrail; // eslint-disable-line

const winstonPapertrail = new winston.transports.Papertrail({
  host: 'logs5.papertrailapp.com',
  port: 43689
});

winstonPapertrail.on('error', (error: Error) => {
  Log.error('Error connecting to papertrail logging service', error);
});

export default new winston.Logger({
  transports: [winstonPapertrail]
});
