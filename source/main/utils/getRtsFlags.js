// @flow
import Store from 'electron-store';

const store = new Store();

export const getRtsFlags = (network: string): Array<string> => {
  try {
    const rtsFlags = store.get(`${network}-RTS-FLAGS`);
    if (rtsFlags) {
      return rtsFlags.split(' ');
    }
    return [];
  } catch (error) {
    return [];
  }
};
