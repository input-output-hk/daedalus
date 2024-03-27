'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.handleDiskSpace = void 0;
const check_disk_space_1 = __importDefault(require('check-disk-space'));
const prettysize_1 = __importDefault(require('prettysize'));
const get_disk_space_status_1 = require('../ipc/get-disk-space-status');
const logging_1 = require('./logging');
const config_1 = require('../config');
const cardano_node_types_1 = require('../../common/types/cardano-node.types');
const getDiskCheckReport = async (
  path,
  timeout = config_1.DISK_SPACE_CHECK_TIMEOUT
) => {
  const initialReport = {
    isNotEnoughDiskSpace: false,
    diskSpaceRequired: '',
    diskSpaceMissing: '',
    diskSpaceRecommended: '',
    diskSpaceAvailable: '',
    hadNotEnoughSpaceLeft: false,
    diskSpaceAvailableRaw: 0,
    diskTotalSpaceRaw: 0,
    isError: false,
  };
  // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
  return Promise.race([
    // Disk space check promise
    new Promise((resolve) => {
      (0, check_disk_space_1.default)(path)
        .then(({ free, size }) => {
          logging_1.logger.info(
            '[DISK-SPACE-DEBUG] Disk space check completed',
            {
              free,
              size,
            }
          );
          resolve({
            ...initialReport,
            diskSpaceAvailableRaw: free,
            diskSpaceAvailable: (0, prettysize_1.default)(free),
            diskTotalSpace: size,
          });
        })
        .catch((error) => {
          logging_1.logger.error(
            '[DISK-SPACE-DEBUG] Error getting diskCheckReport',
            error
          );
          resolve({ ...initialReport, isError: true });
        });
    }),
    new Promise((resolve) => {
      setTimeout(() => {
        resolve({ ...initialReport, isError: true });
      }, timeout);
    }),
  ]);
};
const handleDiskSpace = (mainWindow, cardanoNode) => {
  let diskSpaceCheckInterval;
  let diskSpaceCheckIntervalLength = config_1.DISK_SPACE_CHECK_LONG_INTERVAL; // Default check interval
  let isNotEnoughDiskSpace = false; // Default check state
  const handleCheckDiskSpace = async (
    hadNotEnoughSpaceLeft,
    forceDiskSpaceRequired
  ) => {
    const diskSpaceRequired =
      forceDiskSpaceRequired || config_1.DISK_SPACE_REQUIRED;
    const response = await getDiskCheckReport(config_1.stateDirectoryPath);
    if (response.isError) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.info(
        '[DISK-SPACE-DEBUG] We could not check disk space, but we will try to start cardano-node anyway'
      );
      resetInterval(config_1.DISK_SPACE_CHECK_DONT_BOTHER_ME_INTERVAL);
    } else {
      const diskSpaceMissing = Math.max(
        diskSpaceRequired - response.diskSpaceAvailableRaw,
        0
      );
      const diskSpaceRecommended =
        (response.diskTotalSpaceRaw *
          config_1.DISK_SPACE_RECOMMENDED_PERCENTAGE) /
        100;
      const diskSpaceRequiredMargin =
        diskSpaceRequired -
        (diskSpaceRequired * config_1.DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE) /
          100;
      if (response.diskSpaceAvailableRaw <= diskSpaceRequiredMargin) {
        if (!isNotEnoughDiskSpace) {
          // State change: transitioning from enough to not-enough disk space
          setDiskSpaceIntervalChecking(
            config_1.DISK_SPACE_CHECK_SHORT_INTERVAL
          );
          isNotEnoughDiskSpace = true;
        }
      } else if (response.diskSpaceAvailableRaw >= diskSpaceRequired) {
        const newDiskSpaceCheckIntervalLength =
          response.diskSpaceAvailableRaw >= diskSpaceRequired * 2
            ? config_1.DISK_SPACE_CHECK_LONG_INTERVAL
            : config_1.DISK_SPACE_CHECK_MEDIUM_INTERVAL;
        if (isNotEnoughDiskSpace) {
          // State change: transitioning from not-enough to enough disk space
          setDiskSpaceIntervalChecking(newDiskSpaceCheckIntervalLength);
          isNotEnoughDiskSpace = false;
        } else if (
          newDiskSpaceCheckIntervalLength !== diskSpaceCheckIntervalLength
        ) {
          // Interval change: transitioning from medium to long interval (or vice versa)
          // This is a special case in which we adjust the disk space check polling interval:
          // - more than 2x of available space than required: LONG interval
          // - less than 2x of available space than required: MEDIUM interval
          setDiskSpaceIntervalChecking(newDiskSpaceCheckIntervalLength);
        }
      }
      response.isNotEnoughDiskSpace = isNotEnoughDiskSpace;
      response.diskSpaceRequired = (0, prettysize_1.default)(diskSpaceRequired);
      response.diskSpaceMissing = (0, prettysize_1.default)(diskSpaceMissing);
      response.diskSpaceRecommended = (0, prettysize_1.default)(
        diskSpaceRecommended
      );
      response.hadNotEnoughSpaceLeft = hadNotEnoughSpaceLeft;
    }
    const NO_SPACE_AND_CARDANO_NODE_CAN_BE_STOPPED =
      isNotEnoughDiskSpace &&
      cardanoNode.state !== cardano_node_types_1.CardanoNodeStates.STOPPING &&
      cardanoNode.state !== cardano_node_types_1.CardanoNodeStates.STOPPED;
    const CARDANO_NODE_CAN_BE_STARTED_FOR_THE_FIRST_TIME =
      !isNotEnoughDiskSpace &&
      cardanoNode.state === cardano_node_types_1.CardanoNodeStates.STOPPED &&
      cardanoNode._startupTries === 0;
    const CARDANO_NODE_CAN_BE_STARTED_AFTER_FREEING_SPACE =
      !isNotEnoughDiskSpace &&
      cardanoNode.state !== cardano_node_types_1.CardanoNodeStates.STOPPED &&
      cardanoNode.state !== cardano_node_types_1.CardanoNodeStates.STOPPING &&
      hadNotEnoughSpaceLeft;
    try {
      switch (true) {
        case NO_SPACE_AND_CARDANO_NODE_CAN_BE_STOPPED:
          try {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logging_1.logger.info('[DISK-SPACE-DEBUG] Stopping cardano node');
            await cardanoNode.stop();
          } catch (error) {
            logging_1.logger.error(
              '[DISK-SPACE-DEBUG] Cannot stop cardano node',
              error
            );
          }
          break;
        case CARDANO_NODE_CAN_BE_STARTED_FOR_THE_FIRST_TIME:
          await cardanoNode.start();
          break;
        case CARDANO_NODE_CAN_BE_STARTED_AFTER_FREEING_SPACE:
          try {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logging_1.logger.info(
              '[DISK-SPACE-DEBUG] restart cardano node after freeing up disk space'
            );
            if (cardanoNode._startupTries > 0) await cardanoNode.restart();
            else await cardanoNode.start();
            response.hadNotEnoughSpaceLeft = false;
          } catch (error) {
            logging_1.logger.error(
              '[DISK-SPACE-DEBUG] Daedalus tried to restart, but failed',
              error
            );
          }
          break;
        default:
      }
    } catch (error) {
      logging_1.logger.error('[DISK-SPACE-DEBUG] Unknown error', error);
      resetInterval(config_1.DISK_SPACE_CHECK_MEDIUM_INTERVAL);
    }
    await get_disk_space_status_1.getDiskSpaceStatusChannel.send(
      response,
      mainWindow.webContents
    );
    return response;
  };
  const resetInterval = (interval) => {
    // Remove diskSpaceCheckInterval if set
    if (diskSpaceCheckInterval) {
      clearInterval(diskSpaceCheckInterval);
      // Reset to default check interval
      diskSpaceCheckIntervalLength = interval;
    }
  };
  let hadNotEnoughSpaceLeft = false;
  const setDiskSpaceIntervalChecking = (interval) => {
    clearInterval(diskSpaceCheckInterval);
    diskSpaceCheckInterval = setInterval(async () => {
      const response = await handleCheckDiskSpace(hadNotEnoughSpaceLeft);
      hadNotEnoughSpaceLeft = response?.hadNotEnoughSpaceLeft;
    }, interval);
    diskSpaceCheckIntervalLength = interval;
  };
  // Start default interval
  setDiskSpaceIntervalChecking(diskSpaceCheckIntervalLength);
  get_disk_space_status_1.getDiskSpaceStatusChannel.onReceive(async () => {
    const diskReport = await getDiskCheckReport(config_1.stateDirectoryPath);
    await get_disk_space_status_1.getDiskSpaceStatusChannel.send(
      diskReport,
      mainWindow.webContents
    );
    return diskReport;
  });
  return handleCheckDiskSpace;
};
exports.handleDiskSpace = handleDiskSpace;
//# sourceMappingURL=handleDiskSpace.js.map
