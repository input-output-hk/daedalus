import Log from 'electron-log';
import winston from 'winston';

// Requiring `winston-papertrail` will expose
// `winston.transports.Papertrail`
require('winston-papertrail').Papertrail; // eslint-disable-line

const papertrailConfiguration = {
  host: 'logs5.papertrailapp.com',
  port: 43689,
};

const winstonPapertrailDaedalus = new winston.transports.Papertrail(
  Object.assign({}, papertrailConfiguration, {
    program: 'Daedalus'
  })
);

const winstonPapertrailCardanoNode = new winston.transports.Papertrail(
  Object.assign({}, papertrailConfiguration, {
    program: 'Cardano node'
  })
);

winstonPapertrailDaedalus.on('error', (error: Error) => {
  Log.error('Error connecting to papertrail logging service for Daedalus', error);
});

winstonPapertrailCardanoNode.on('error', (error: Error) => {
  Log.error('Error connecting to papertrail logging service for Cardano node', error);
});

export const daedalusLogger = new winston.Logger({
  transports: [winstonPapertrailDaedalus]
});

export const cardanoNodeLogger = new winston.Logger({
  transports: [winstonPapertrailCardanoNode]
});
