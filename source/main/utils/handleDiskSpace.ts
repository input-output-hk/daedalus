import path from 'path';
import fs from 'fs-extra';
import { BrowserWindow } from 'electron';
import checkDiskSpace from 'check-disk-space';
import prettysize from 'prettysize';
import { getDiskSpaceStatusChannel } from '../ipc/get-disk-space-status';
import { logger } from './logging';
import {
  DISK_SPACE_CHECK_DONT_BOTHER_ME_INTERVAL,
  DISK_SPACE_CHECK_LONG_INTERVAL,
  DISK_SPACE_CHECK_MEDIUM_INTERVAL,
  DISK_SPACE_CHECK_SHORT_INTERVAL,
  DISK_SPACE_RECOMMENDED_PERCENTAGE,
  DISK_SPACE_REQUIRED,
  DISK_SPACE_CHECK_TIMEOUT,
  DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE,
  stateDirectoryPath,
  launcherConfig,
} from '../config';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import { CardanoNode } from '../cardano/CardanoNode';
import type { CheckDiskSpaceResponse } from '../../common/types/no-disk-space.types';
import type { MithrilBootstrapStatusUpdate } from '../../common/types/mithril-bootstrap.types';
import {
  getPendingMithrilBootstrapDecision,
  getMithrilBootstrapStatus,
  onMithrilBootstrapDecision,
  onMithrilBootstrapStatus,
  waitForMithrilBootstrapDecision,
  mithrilBootstrapStatusChannel,
  setMithrilBootstrapStatus,
} from '../ipc/mithrilBootstrapChannel';
import { MithrilBootstrapService } from '../mithril/MithrilBootstrapService';

const getDiskCheckReport = async (
  path: string,
  timeout: number = DISK_SPACE_CHECK_TIMEOUT
): Promise<CheckDiskSpaceResponse> => {
  const initialReport: CheckDiskSpaceResponse = {
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
      checkDiskSpace(path)
        .then(({ free, size }) => {
          logger.info('[DISK-SPACE-DEBUG] Disk space check completed', {
            free,
            size,
          });
          resolve({
            ...initialReport,
            diskSpaceAvailableRaw: free,
            diskSpaceAvailable: prettysize(free),
            diskTotalSpace: size,
          });
        })
        .catch((error) => {
          logger.error(
            '[DISK-SPACE-DEBUG] Error getting diskCheckReport',
            error
          );
          resolve({ ...initialReport, isError: true });
        });
    }), // Timeout promise
    new Promise((resolve) => {
      setTimeout(() => {
        resolve({ ...initialReport, isError: true });
      }, timeout);
    }),
  ]);
};

export const handleDiskSpace = (
  mainWindow: BrowserWindow,
  cardanoNode: CardanoNode
): ((...args: Array<any>) => any) => {
  let diskSpaceCheckInterval;
  let diskSpaceCheckIntervalLength = DISK_SPACE_CHECK_LONG_INTERVAL; // Default check interval

  let isNotEnoughDiskSpace = false; // Default check state
  let mithrilDecisionInFlight = false;
  let mithrilFailureDecisionInFlight = false;
  let mithrilFailureDeclineInFlight = false;
  let mithrilStartupCheckDone = false;
  let mithrilDecisionPrompted = false;
  let mithrilDecision: 'accept' | 'decline' | null = null;
  let mithrilBootstrapCompleted = false;
  let mithrilStartInFlight = false;
  const mithrilLockFilePath = path.join(
    stateDirectoryPath,
    'Logs',
    'mithril-bootstrap.lock'
  );
  const envWipeChain =
    process.env.DAEDALUS_WIPE_CHAIN === 'true' ? true : undefined;
  const argvWipeChain = process.argv.slice(1).includes('--wipe-chain')
    ? true
    : undefined;
  const wipeChainFlag =
    envWipeChain ?? argvWipeChain ?? launcherConfig.wipeChain ?? false;
  const mithrilBootstrapService = new MithrilBootstrapService();
  const emitMithrilDecisionStatus = async () => {
    const update: MithrilBootstrapStatusUpdate = {
      status: 'decision',
      progress: 0,
      currentStep: 'Choose a sync method',
      snapshot: null,
      error: null,
    };
    setMithrilBootstrapStatus(update);
    await mithrilBootstrapStatusChannel.send(update, mainWindow.webContents);
  };

  const emitMithrilIdleStatus = async () => {
    const update: MithrilBootstrapStatusUpdate = {
      status: 'idle',
      progress: 0,
      currentStep: undefined,
      snapshot: null,
      error: null,
    };
    setMithrilBootstrapStatus(update);
    await mithrilBootstrapStatusChannel.send(update, mainWindow.webContents);
  };

  const emitMithrilStartFailure = async (error: unknown) => {
    const message =
      error instanceof Error
        ? error.message
        : 'Cardano node failed to start after Mithril bootstrap. Wipe the chain data and try again.';
    const update: MithrilBootstrapStatusUpdate = {
      status: 'failed',
      progress: 0,
      currentStep: 'Cardano node failed to start',
      snapshot: null,
      error: {
        message,
        stage: 'node-start',
      },
    };
    setMithrilBootstrapStatus(update);
    await mithrilBootstrapStatusChannel.send(update, mainWindow.webContents);
  };

  const handleMithrilFailureDecline = async (source: string) => {
    if (mithrilFailureDeclineInFlight) return false;
    const pendingDecision = getPendingMithrilBootstrapDecision();
    if (pendingDecision !== 'decline') return false;
    mithrilFailureDeclineInFlight = true;
    try {
      await emitMithrilIdleStatus();
      await mithrilBootstrapService.wipeChainAndSnapshots(
        `User declined after bootstrap failure (${source}). Wiped chain directory and Mithril snapshots.`
      );
      await cardanoNode.start();
      mithrilDecision = null;
      mithrilDecisionPrompted = false;
      return true;
    } finally {
      mithrilFailureDeclineInFlight = false;
    }
  };

  const ensureMithrilStartupGate = async (): Promise<boolean> => {
    if (mithrilStartupCheckDone) return true;
    mithrilStartupCheckDone = true;

    try {
      if (wipeChainFlag) {
        if (
          cardanoNode.state !== CardanoNodeStates.STOPPING &&
          cardanoNode.state !== CardanoNodeStates.STOPPED
        ) {
          try {
            await cardanoNode.stop();
          } catch (error) {
            logger.warn('[MITHRIL] Failed to stop cardano-node for wipe', {
              error,
            });
          }
        }
        await mithrilBootstrapService.wipeChainAndSnapshots(
          'wipe-chain flag set. Wiped chain directory and Mithril snapshots.'
        );
      }
      const lockExists = await fs.pathExists(mithrilLockFilePath);
      if (lockExists) {
        await mithrilBootstrapService.wipeChainAndSnapshots(
          'Incomplete bootstrap detected. Wiped chain directory and Mithril snapshots.'
        );
        mithrilDecisionPrompted = false;
        mithrilDecision = null;
      }
    } catch (error) {
      logger.warn('[MITHRIL] Failed to handle bootstrap lock', { error });
    }

    return true;
  };

  onMithrilBootstrapStatus(async (status) => {
    if (status.status !== 'completed' || mithrilStartInFlight) return;
    if (cardanoNode.state !== CardanoNodeStates.STOPPED) return;
    mithrilStartInFlight = true;
    mithrilBootstrapCompleted = true;
    try {
      await cardanoNode.start();
      await emitMithrilIdleStatus();
      mithrilDecision = null;
      mithrilDecisionPrompted = false;
    } catch (error) {
      logger.error('[MITHRIL] Failed to start cardano-node after bootstrap', {
        error,
      });
      mithrilBootstrapCompleted = false;
      await emitMithrilStartFailure(error);
    } finally {
      mithrilStartInFlight = false;
    }
  });

  onMithrilBootstrapStatus((status) => {
    if (status.status !== 'failed') return;
    if (mithrilFailureDecisionInFlight) return;
    mithrilFailureDecisionInFlight = true;
    waitForMithrilBootstrapDecision()
      .then(async (decision) => {
        if (decision !== 'decline') return;
        await handleMithrilFailureDecline('status-listener');
      })
      .catch((error) => {
        logger.error('[MITHRIL] Decision wait failed after failure', { error });
      })
      .finally(() => {
        mithrilFailureDecisionInFlight = false;
      });
  });

  onMithrilBootstrapDecision((decision) => {
    if (decision !== 'decline') return;
    if (getMithrilBootstrapStatus().status !== 'failed') return;
    handleMithrilFailureDecline('decision-listener').catch((error) => {
      logger.error('[MITHRIL] Decline handling failed after decision', {
        error,
      });
    });
  });

  const isChainEmpty = async (): Promise<boolean> => {
    const chainDir = path.join(stateDirectoryPath, 'chain');
    const exists = await fs.pathExists(chainDir);
    if (!exists) return true;
    const entries = await fs.readdir(chainDir);
    return entries.length === 0;
  };

  const handleCheckDiskSpace = async (
    hadNotEnoughSpaceLeft: boolean,
    forceDiskSpaceRequired?: number
  ): Promise<CheckDiskSpaceResponse> => {
    const diskSpaceRequired = forceDiskSpaceRequired || DISK_SPACE_REQUIRED;
    const response = await getDiskCheckReport(stateDirectoryPath);
    await ensureMithrilStartupGate();
    const latestDecision = getPendingMithrilBootstrapDecision();
    if (latestDecision && latestDecision !== mithrilDecision) {
      mithrilDecision = latestDecision;
    }

    if (response.isError) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.info(
        '[DISK-SPACE-DEBUG] We could not check disk space, but we will try to start cardano-node anyway'
      );
      resetInterval(DISK_SPACE_CHECK_DONT_BOTHER_ME_INTERVAL);
    } else {
      const diskSpaceMissing = Math.max(
        diskSpaceRequired - response.diskSpaceAvailableRaw,
        0
      );
      const diskSpaceRecommended =
        (response.diskTotalSpaceRaw * DISK_SPACE_RECOMMENDED_PERCENTAGE) / 100;
      const diskSpaceRequiredMargin =
        diskSpaceRequired -
        (diskSpaceRequired * DISK_SPACE_REQUIRED_MARGIN_PERCENTAGE) / 100;

      if (response.diskSpaceAvailableRaw <= diskSpaceRequiredMargin) {
        if (!isNotEnoughDiskSpace) {
          // State change: transitioning from enough to not-enough disk space
          setDiskSpaceIntervalChecking(DISK_SPACE_CHECK_SHORT_INTERVAL);
          isNotEnoughDiskSpace = true;
        }
      } else if (response.diskSpaceAvailableRaw >= diskSpaceRequired) {
        const newDiskSpaceCheckIntervalLength =
          response.diskSpaceAvailableRaw >= diskSpaceRequired * 2
            ? DISK_SPACE_CHECK_LONG_INTERVAL
            : DISK_SPACE_CHECK_MEDIUM_INTERVAL;

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
      response.diskSpaceRequired = prettysize(diskSpaceRequired);
      response.diskSpaceMissing = prettysize(diskSpaceMissing);
      response.diskSpaceRecommended = prettysize(diskSpaceRecommended);
      response.hadNotEnoughSpaceLeft = hadNotEnoughSpaceLeft;
    }

    const NO_SPACE_AND_CARDANO_NODE_CAN_BE_STOPPED =
      isNotEnoughDiskSpace &&
      cardanoNode.state !== CardanoNodeStates.STOPPING &&
      cardanoNode.state !== CardanoNodeStates.STOPPED;
    const CARDANO_NODE_CAN_BE_STARTED_FOR_THE_FIRST_TIME =
      !isNotEnoughDiskSpace && cardanoNode.state === CardanoNodeStates.STOPPED;
    const CARDANO_NODE_CAN_BE_STARTED_AFTER_FREEING_SPACE =
      !isNotEnoughDiskSpace &&
      cardanoNode.state !== CardanoNodeStates.STOPPED &&
      cardanoNode.state !== CardanoNodeStates.STOPPING &&
      hadNotEnoughSpaceLeft;

    try {
      switch (true) {
        case NO_SPACE_AND_CARDANO_NODE_CAN_BE_STOPPED:
          try {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logger.info('[DISK-SPACE-DEBUG] Stopping cardano node');
            await cardanoNode.stop();
          } catch (error) {
            logger.error('[DISK-SPACE-DEBUG] Cannot stop cardano node', error);
          }

          break;

        case CARDANO_NODE_CAN_BE_STARTED_FOR_THE_FIRST_TIME:
          try {
            const chainEmpty = await isChainEmpty();
            if (chainEmpty) {
              const mithrilStatus = getMithrilBootstrapStatus();
              if (mithrilStatus.status === 'completed') {
                mithrilBootstrapCompleted = true;
              }

              if (mithrilStatus.status === 'failed') {
                await handleMithrilFailureDecline('polling-chain-empty');
                response.hadNotEnoughSpaceLeft = false;
                break;
              }

              if (mithrilBootstrapCompleted) {
                await emitMithrilIdleStatus();
                await cardanoNode.start();
                mithrilDecision = null;
                mithrilDecisionPrompted = false;
                break;
              }
              if (!mithrilDecisionPrompted) {
                logger.info('[MITHRIL] Emitting decision status');
                await emitMithrilDecisionStatus();
                mithrilDecisionPrompted = true;
              }
              if (mithrilDecision === 'decline') {
                await emitMithrilIdleStatus();
                await mithrilBootstrapService.wipeChainAndSnapshots(
                  'User declined Mithril bootstrap. Wiped chain directory and Mithril snapshots.'
                );
                await cardanoNode.start();
                break;
              }

              if (mithrilDecision === 'accept') {
                break;
              }

              if (!mithrilDecisionInFlight) {
                mithrilDecisionInFlight = true;
                waitForMithrilBootstrapDecision()
                  .then(async (decision) => {
                    mithrilDecision = decision;
                    mithrilDecisionPrompted = false;
                    if (decision === 'decline') {
                      await emitMithrilIdleStatus();
                      await mithrilBootstrapService.wipeChainAndSnapshots(
                        'User declined Mithril bootstrap. Wiped chain directory and Mithril snapshots.'
                      );
                      await cardanoNode.start();
                    }
                  })
                  .catch((error) => {
                    logger.error('[MITHRIL] Decision wait failed', { error });
                  })
                  .finally(() => {
                    mithrilDecisionInFlight = false;
                  });
              }
              response.hadNotEnoughSpaceLeft = false;
              break;
            }

            const mithrilStatus = getMithrilBootstrapStatus();
            if (mithrilStatus.status === 'failed') {
              await handleMithrilFailureDecline('polling-chain-present');
              response.hadNotEnoughSpaceLeft = false;
              break;
            }

            await cardanoNode.start();
          } catch (error) {
            logger.error('[MITHRIL] Failed to handle bootstrap decision', {
              error,
            });
            await cardanoNode.start();
          }
          break;

        case CARDANO_NODE_CAN_BE_STARTED_AFTER_FREEING_SPACE:
          try {
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            logger.info(
              '[DISK-SPACE-DEBUG] restart cardano node after freeing up disk space'
            );
            if (cardanoNode._startupTries > 0) await cardanoNode.restart();
            else await cardanoNode.start();
            response.hadNotEnoughSpaceLeft = false;
          } catch (error) {
            logger.error(
              '[DISK-SPACE-DEBUG] Daedalus tried to restart, but failed',
              error
            );
          }

          break;

        default:
      }
    } catch (error) {
      logger.error('[DISK-SPACE-DEBUG] Unknown error', error);
      resetInterval(DISK_SPACE_CHECK_MEDIUM_INTERVAL);
    }

    await getDiskSpaceStatusChannel.send(response, mainWindow.webContents);
    return response;
  };

  const resetInterval = (interval: number) => {
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
  getDiskSpaceStatusChannel.onReceive(async () => {
    const diskReport = await getDiskCheckReport(stateDirectoryPath);
    await getDiskSpaceStatusChannel.send(diskReport, mainWindow.webContents);
    return diskReport;
  });
  return handleCheckDiskSpace;
};
