// @flow
import TransportNodeHid from '@ledgerhq/hw-transport-node-hid';
import { getDevices } from '@ledgerhq/hw-transport-node-hid-noevents';
// import { identifyUSBProductId } from '@ledgerhq/devices';
import usb from 'usb';
import { includes, reject, without } from 'lodash';
import { logger } from './logging';
import { changeConnection } from '../ipc/getHardwareWalletChannel';

// Types
export type LedgerState = 'plugged_in' | 'unlocked' | 'ready';
export type LedgerErrorCode = 'not_supported' | 'unable_to_list_device_paths';
export type LedgerError = {
  code: LedgerErrorCode,
  message: any,
};

// Constants
const UPDATE_DEVICE_INFO_INTERVAL = 500; // unit: ms
export const LEDGER_STATES: {
  PLUGGED_IN: LedgerState,
  UNLOCKED: LedgerState,
  READY: LedgerState,
} = {
  PLUGGED_IN: 'plugged_in',
  UNLOCKED: 'unlocked',
  READY: 'ready', // Cardano app running
};
export const LEDGER_ERROR_CODES: {
  NOT_SUPPORTED: LedgerErrorCode,
  UNABLE_TO_LIST_DEVICE_PATHS: LedgerErrorCode,
} = {
  NOT_SUPPORTED: 'not_supported',
  UNABLE_TO_LIST_DEVICE_PATHS: 'unable_to_list_device_paths',
};

export class HardwareWalletsHandler {
  _ledger: {
    isInitialized: boolean,
    isSupported: ?boolean,
    isReady: ?boolean,
    devicePaths: Array<string>,
    devices: Array<Object>, // TODO: Introduce Device type
    rawDevices: Array<Object>, // TODO: Introduce Raw Device type
    observer: ?Function,
    error: ?LedgerError,
  } = {
    isInitialized: false,
    isSupported: null,
    isReady: null,
    devicePaths: [],
    devices: [],
    rawDevices: [],
    observer: null,
    error: null,
  };

  get isReady(): ?boolean {
    return this._ledger.isReady;
  }

  constructor(mainWindow) {
    logger.info('[HW-HANDLER]:constructor', { ledger: this._ledger });
    this.mainWindow = mainWindow;
    logEverywhere(mainWindow, { message: '>> Start constructor', params: { ledger: this._ledger }});
    return JSON.stringify({ data: this._ledger });
  }

  initialize = async () => {
    logEverywhere(this.mainWindow, { message: '>> Initialize', params: {}});
    // TransportNodeHid
    //   isSupported: [Function]: Promise: boolean,
    //   list: [Function]: Promise: Array<string>,
    //     -  returns a list of connected and unlocked Ledger device USB paths
    //   setListenDevicesDebounce: [Function], | defaults to 500ms
    //   setListenDevicesPollingSkip: [Function],
    //   setListenDevicesDebug: [Function],
    //   listen: [Function]
    //   open: [Function]

    try {
      this._ledger.isSupported = await TransportNodeHid.isSupported();
      TransportNodeHid.setListenDevicesDebounce(1000); // Defaults to 500ms
      logEverywhere(this.mainWindow, { message: '>> Initialize - is Ledger supported', params: {
        isSupported: this._ledger.isSupported,
      }});
    } catch (error) {
      Object.assign(this._ledger, {
        isSupported: false,
        error: {
          code: LEDGER_ERROR_CODES.NOT_SUPPORTED,
          message: error,
        },
      });

      logEverywhere(this.mainWindow, { message: '>> Initialize - Error 1', params: {
        isSupported: false,
        error: {
          code: LEDGER_ERROR_CODES.NOT_SUPPORTED,
          message: error,
        },
      }});
    }

    if (this._ledger.isSupported) {
      logEverywhere(this.mainWindow, { message: '>> Initialize - Ledger SUPPORTED', params: {}});
      try {
        await this.updateDevicesInfo();
        setInterval(this.updateDevicesInfo, UPDATE_DEVICE_INFO_INTERVAL);
      } catch (error) {
        Object.assign(this._ledger, {
          isSupported: false,
          error: {
            code: LEDGER_ERROR_CODES.UNABLE_TO_LIST_DEVICE_PATHS,
            message: error,
          },
        });

        logEverywhere(this.mainWindow, { message: '>> Initialize - Error 2', params: {
          isSupported: false,
          error: {
            code: LEDGER_ERROR_CODES.UNABLE_TO_LIST_DEVICE_PATHS,
            message: error,
          },
        }});
      }
      logEverywhere(this.mainWindow, { message: '>> Initialize - Ledger SUPPORTED - Continue', params: {}});
      const observer = new LedgerEventObserver(this.updateDevices);
      // TransportNodeHid.listen(observer);

      if (!this._ledger.observer) {
        this._ledger.observer = observer;
      }

      usb.on('attach', (device) => {
        logger.info('[HW-HANDLER]:usb:attach', { device });
      });
      usb.on('detach', (device) => {
        logger.info('[HW-HANDLER]:usb:dettach', { device });
      });
    }

    this._ledger.isInitialized = true;
    this._ledger.isReady =
      !!this._ledger.isSupported && this._ledger.error == null;

    logger.info('[HW-HANDLER]:initialize', { ledger: this._ledger });
    logEverywhere(this.mainWindow, { message: '>> Initialize - Ledger INITIALIZED', params: {
      ledger: this._ledger
    }});
  };

  updateDevicesInfo = async () => {
    // logEverywhere(this.mainWindow, { message: '>> updateDevicesInfo', params: {}});
    // $FlowFixMe
    const oldList = this._ledger.devicePaths
    this._ledger.devicePaths = await TransportNodeHid.list();
    this._ledger.rawDevices = getDevices();
    /* logEverywhere(this.mainWindow, { message: '>> updateDevicesInfo - DONE', params: {
      devicePaths: this._ledger.devicePaths,
      rawDevices: this._ledger.rawDevices,
    }}); */
    if (oldList.length < this._ledger.devicePaths.length) {
      // if (!this._ledger.observer) {
        const observer = new LedgerEventObserver(this.updateDevices);
      // }
      this._ledger.observer = observer;
      const device = this._ledger.rawDevices[0];
      logEverywhere(this.mainWindow, { message: '>> updateDevicesInfo - CHANGED', params: {
        oldList,
        newList: this._ledger.devicePaths,
        _ledger: this._ledger,
        device,
        changeConnection,
      }});

      /* observer.next({
        type: 'add',
        device: device,
        deviceModel: device,
      }) */

      try {
        changeConnection(
          device,
          this.mainWindow
        );
      } catch (e) {
        logEverywhere(this.mainWindow, { message: '>> SEND error', params: {
          error_t: e
        }});
      }


      this.updateDevices(device, 'add')
    }
  };

  updateDevices = (device: Object, action: 'add' | 'remove') => {
    if (action === 'add') {
      this._ledger.devices.push(device);
      if (!includes(this._ledger.devicePaths, device.path)) {
        this._ledger.devicePaths.push(device.path);
      }
    } else {
      this._ledger.devices = reject(this._ledger.devices, {
        path: device.path,
      });
      if (includes(this._ledger.devicePaths, device.path)) {
        this._ledger.devicePaths = without(
          this._ledger.devicePaths,
          device.path
        );
      }
    }
    logger.info('[HW-HANDLER]:updateDevices', {
      action,
      device,
      devices: this._ledger.devices,
      devicePaths: this._ledger.devicePaths,
      rawDevices: this._ledger.rawDevices,
    });
    logEverywhere(this.mainWindow, { message: '>> updateDevices', params: {
      action,
      device,
      devices: this._ledger.devices,
      devicePaths: this._ledger.devicePaths,
      rawDevices: this._ledger.rawDevices,
    }});
  };
}

class LedgerEventObserver {
  _onUpdateDevices: Function;

  constructor(onUpdateDevices: Function) {
    this._onUpdateDevices = onUpdateDevices;
  }

  next = async (event: {
    type: 'add' | 'remove',
    descriptor: ?string, // device.path
    device: Object,
    deviceModel: Object,
  }) => {
    logEverywhere(this.mainWindow, { message: '>> LedgerEventObserver - NEXT', params: {
      event
    }});
    return;
    // "event": {
    //   "type": "add",
    //   "descriptor": "IOService:/AppleACPIPlatformExpert/PCI0@0/AppleACPIPCI/XHC1@14/XHC1@14000000/HS03@14300000/USB2.0",
    //   "device": {
    //     "vendorId": 11415,
    //     "productId": 4113,
    //     "path": "IOService:/AppleACPIPlatformExpert/PCI0@0/AppleACPIPCI/XHC1@14/XHC1@14000000/HS03@14300000/USB2.0",
    //     "serialNumber": "0001",
    //     "manufacturer": "Ledger",
    //     "product": "Nano S",
    //     "release": 513,
    //     "interface": 0,
    //     "usagePage": 65440,
    //     "usage": 1
    //   },
    //   "deviceModel": {
    //     "id": "nanoS",
    //     "productName": "Ledger Nano S",
    //     "productIdMM": 16,
    //     "legacyUsbProductId": 1,
    //     "usbOnly": true,
    //     "memorySize": 327680,
    //     "blockSize": 4096
    //   }
    // }
    logger.info('[HW-HANDLER]:LedgerEventObserver:next', { event });

    const { type, device } = event;
    this._onUpdateDevices(device, type);

    // const transport = await TransportNodeHid.open(event.descriptor);
    // "transport": [
    //   "exchangeTimeout": 30000,
    //   "unresponsiveTimeout": 15000,
    //   "deviceModel": {
    //     "id": "nanoS",
    //     "productName": "Ledger Nano S",
    //     "productIdMM": 16,
    //     "legacyUsbProductId": 1,
    //     "usbOnly": true,
    //     "memorySize": 327680,
    //     "blockSize": 4096
    //   },
    //   "_events": {
    //     "_events": {},
    //     "_eventsCount": 0
    //   },
    //   "send",
    //   "exchangeBusyPromise",
    //   "exchangeAtomicImpl",
    //   "_appAPIlock": null,
    //   "device": [
    //     "_events": {},
    //     "_eventsCount": 1,
    //     "_maxListeners",
    //     "_raw": {},
    //     "write",
    //     "getFeatureReport",
    //     "sendFeatureReport",
    //     "setNonBlocking",
    //     "readSync",
    //     "readTimeout",
    //     "getDeviceInfo",
    //     "_paused": true
    //   ]
    //   "channel": 28767,
    //   "packetSize": 64,
    //   "disconnected": false
    //   "setDisconnected",
    //   "writeHID",
    //   "readHID",
    //   "exchange"
    // ]

    // transport.setDisconnected();
    // await transport.device.close();
    // await transport.close(); // https://github.com/LedgerHQ/ledgerjs/blob/master/packages/hw-transport-node-hid-noevents/src/TransportNodeHid.js#L177
  };

  error = (error) => {
    logger.info('[HW-HANDLER]:LedgerEventObserver:error', { error });
  };

  complete = () => {
    logger.info('[HW-HANDLER]:LedgerEventObserver:complete');
  };
}

export const logEverywhere = (mainWindow, props) => {
  const { message = '', params = {} } = props;
  if (mainWindow && mainWindow.webContents) {
    mainWindow.webContents.executeJavaScript(`console.log("${message}", ${JSON.stringify(params)})`)
  }
}