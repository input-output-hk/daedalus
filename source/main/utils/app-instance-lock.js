// @flow
import { app } from 'electron';

export const acquireDaedalusInstanceLock = () => {
  const isSingleInstance = app.requestSingleInstanceLock();
  if (!isSingleInstance) {
    throw new Error('Another Daedalus instance is already running.');
  }
};

export const releaseDaedalusInstanceLock = () => (
  app.releaseSingleInstanceLock()
);
