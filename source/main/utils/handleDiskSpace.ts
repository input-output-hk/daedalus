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
import {
  isMithrilBootstrapRestoreCompleteStatus,
  MithrilBootstrapStatusUpdate,
} from '../../common/types/mithril-bootstrap.types';
import {
  getPendingMithrilBootstrapDecision,
  getMithrilBootstrapStatus,
  isMithrilDecisionCancelledError,
  onMithrilBootstrapDecision,
  onMithrilBootstrapStatus,
  resetMithrilDecisionState,
  waitForMithrilBootstrapDecision,
  mithrilBootstrapStatusChannel,
  setMithrilBootstrapStatus,
} from '../ipc/mithrilBootstrapChannel';
import {
  chainStorageCoordinator,
  getChainStorageManager,
} from './chainStorageCoordinator';
import type { ManagedChainLayoutResult } from './chainStorageManagerShared';

const getDiskCheckReport = async (
  targetPath: string,
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
  const diskCheckPromise = new Promise<CheckDiskSpaceResponse>((resolve) => {
    checkDiskSpace(targetPath)
      .then(({ free, size }) => {
        logger.info('[DISK-SPACE-DEBUG] Disk space check completed', {
          free,
          size,
        });
        resolve({
          ...initialReport,
          diskSpaceAvailableRaw: free,
          diskSpaceAvailable: prettysize(free),
          diskTotalSpaceRaw: size,
        });
      })
      .catch((error) => {
        logger.error('[DISK-SPACE-DEBUG] Error getting diskCheckReport', {
          error,
        });
        resolve({ ...initialReport, isError: true });
      });
  });

  const timeoutPromise = new Promise<CheckDiskSpaceResponse>((resolve) => {
    setTimeout(() => {
      resolve({ ...initialReport, isError: true });
    }, timeout);
  });

  return Promise.race([diskCheckPromise, timeoutPromise]);
};

export const handleDiskSpace = (
  mainWindow: BrowserWindow,
  cardanoNode: CardanoNode
) => {
  const MANAGED_CHAIN_LAYOUT_ERROR = 'MANAGED_CHAIN_LAYOUT_ERROR';
  const markManagedChainLayoutError = (error: unknown): Error => {
    const normalizedError =
      error instanceof Error ? error : new Error(String(error));

    Object.assign(normalizedError, {
      code: MANAGED_CHAIN_LAYOUT_ERROR,
    });

    return normalizedError;
  };
  const isManagedChainLayoutError = (error: unknown): boolean =>
    (error as NodeJS.ErrnoException | undefined)?.code ===
    MANAGED_CHAIN_LAYOUT_ERROR;

  let diskSpaceCheckInterval;
  let diskSpaceCheckIntervalLength = DISK_SPACE_CHECK_LONG_INTERVAL; // Default check interval

  let isNotEnoughDiskSpace = false; // Default check state
  let mithrilDecisionInFlight = false;
  let mithrilFailureDecisionInFlight = false;
  let mithrilFailureDeclineInFlight = false;
  let mithrilCancelledDeclineInFlight = false;
  let mithrilStartupCheckDone = false;
  let mithrilStartupLayoutResult: ManagedChainLayoutResult | null = null;
  let mithrilDecisionPrompted = false;
  let mithrilDecision: 'accept' | 'decline' | null = null;
  let mithrilBootstrapCompleted = false;
  let mithrilStartInFlight = false;
  let directoryChangeGeneration = 0;
  let activeDiskSpaceCheckPromise: Promise<
    CheckDiskSpaceResponse
  > | null = null;
  let pendingDiskSpaceCheckArgs: {
    hadNotEnoughSpaceLeft?: boolean;
    forceDiskSpaceRequired?: number;
  } | null = null;
  let pendingDiskSpaceCheckWaiters: Array<{
    resolve: (response: CheckDiskSpaceResponse) => void;
    reject: (error: unknown) => void;
  }> = [];
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
  const chainStorageManager = getChainStorageManager();
  const broadcastMithrilStatus = (
    update: MithrilBootstrapStatusUpdate,
    context: string
  ) => {
    setMithrilBootstrapStatus(update);

    Promise.resolve()
      .then(() =>
        mithrilBootstrapStatusChannel.send(update, mainWindow.webContents)
      )
      .catch((error) => {
        logger.warn('[MITHRIL] Failed to broadcast bootstrap status update', {
          context,
          error,
          status: update.status,
        });
      });
  };

  const emitMithrilDecisionStatus = () => {
    const update: MithrilBootstrapStatusUpdate = {
      status: 'decision',
      snapshot: null,
      error: null,
    };
    broadcastMithrilStatus(update, 'emit-decision');
  };

  const emitMithrilIdleStatus = () => {
    const update: MithrilBootstrapStatusUpdate = {
      status: 'idle',
      snapshot: null,
      error: null,
    };
    broadcastMithrilStatus(update, 'emit-idle');
  };

  const emitMithrilStartFailure = (error: unknown) => {
    const message =
      error instanceof Error
        ? error.message
        : 'Cardano node failed to start after Mithril bootstrap. Wipe the chain data and try again.';
    const update: MithrilBootstrapStatusUpdate = {
      status: 'failed',
      snapshot: null,
      error: {
        message,
        stage: 'node-start',
      },
    };
    broadcastMithrilStatus(update, 'emit-start-failure');
  };

  const emitMithrilStartingNodeStatus = () => {
    const currentStatus = getMithrilBootstrapStatus();
    const update: MithrilBootstrapStatusUpdate = {
      ...currentStatus,
      status: 'starting-node',
      error: null,
    };
    broadcastMithrilStatus(update, 'emit-starting-node');
  };

  const canStartNodeAfterMithrilCompletion = () =>
    [
      CardanoNodeStates.STOPPED,
      CardanoNodeStates.CRASHED,
      CardanoNodeStates.ERRORED,
    ].includes(cardanoNode.state);

  const MITHRIL_COMPLETION_DELAY_MS = 6000;

  const startNodeAfterMithrilCompletion = async (): Promise<void> => {
    if (mithrilStartInFlight) return;
    mithrilStartInFlight = true;
    const gen = directoryChangeGeneration;
    try {
      if (gen !== directoryChangeGeneration) {
        logger.info(
          '[MITHRIL] Aborting node start — directory changed during handoff'
        );
        return;
      }
      await emitMithrilStartingNodeStatus();
      await cardanoNode.start();
      if (gen !== directoryChangeGeneration) {
        logger.info(
          '[MITHRIL] Aborting node start — directory changed during handoff'
        );
        return;
      }
      await new Promise<void>((resolve) => {
        setTimeout(resolve, MITHRIL_COMPLETION_DELAY_MS);
      });
      if (gen !== directoryChangeGeneration) {
        logger.info(
          '[MITHRIL] Aborting node start — directory changed during handoff'
        );
        return;
      }
      if (cardanoNode.state !== CardanoNodeStates.RUNNING) {
        throw new Error(
          'Cardano node stopped responding during startup after Mithril bootstrap.'
        );
      }
      await emitMithrilIdleStatus();
      mithrilBootstrapCompleted = false;
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
  };

  const handleMithrilFailureDecline = async (
    source: string,
    currentGeneration: number = directoryChangeGeneration
  ) => {
    if (mithrilFailureDeclineInFlight) return false;
    const pendingDecision = getPendingMithrilBootstrapDecision();
    if (pendingDecision !== 'decline') return false;
    mithrilFailureDeclineInFlight = true;
    try {
      logger.info(
        '[MITHRIL] Starting decline recovery after bootstrap failure',
        {
          source,
          status: getMithrilBootstrapStatus().status,
        }
      );
      await emitMithrilIdleStatus();
      if (currentGeneration !== directoryChangeGeneration) return false;
      await chainStorageCoordinator.wipeChainAndSnapshots(
        `User declined after bootstrap failure (${source}). Wiped chain directory and Mithril snapshots.`,
        cardanoNode.state
      );
      if (currentGeneration !== directoryChangeGeneration) return false;
      await cardanoNode.start();
      logger.info(
        '[MITHRIL] cardano-node start requested after bootstrap failure decline',
        {
          source,
          state: cardanoNode.state,
        }
      );
      mithrilDecision = null;
      mithrilDecisionPrompted = false;
      return true;
    } finally {
      mithrilFailureDeclineInFlight = false;
    }
  };

  const handleMithrilCancelledDecline = async (
    source: string,
    currentGeneration: number = directoryChangeGeneration
  ) => {
    if (mithrilCancelledDeclineInFlight) {
      logger.info(
        '[MITHRIL] Decline recovery after bootstrap cancel already in progress',
        {
          source,
        }
      );
      return false;
    }

    mithrilCancelledDeclineInFlight = true;

    try {
      const pendingDecision = getPendingMithrilBootstrapDecision();
      const status = getMithrilBootstrapStatus().status;

      if (pendingDecision !== 'decline' || status !== 'cancelled') return false;

      logger.info(
        '[MITHRIL] Starting decline recovery after bootstrap cancel',
        {
          source,
          status,
        }
      );

      await emitMithrilIdleStatus();
      if (currentGeneration !== directoryChangeGeneration) return false;
      await chainStorageCoordinator.wipeChainAndSnapshots(
        `User declined after bootstrap cancel (${source}). Wiped chain directory and Mithril snapshots.`,
        cardanoNode.state
      );
      if (currentGeneration !== directoryChangeGeneration) return false;

      logger.info(
        '[MITHRIL] Starting cardano-node after bootstrap cancel decline',
        {
          source,
        }
      );

      await cardanoNode.start();

      logger.info(
        '[MITHRIL] cardano-node start requested after bootstrap cancel decline',
        {
          source,
          state: cardanoNode.state,
        }
      );

      mithrilDecision = null;
      mithrilDecisionPrompted = false;

      return true;
    } finally {
      mithrilCancelledDeclineInFlight = false;
    }
  };

  const ensureMithrilStartupGate = async (
    currentGeneration: number
  ): Promise<ManagedChainLayoutResult | null> => {
    if (mithrilStartupCheckDone && mithrilStartupLayoutResult) {
      return mithrilStartupLayoutResult;
    }

    let layoutResult: ManagedChainLayoutResult;

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
        layoutResult = await chainStorageCoordinator.ensureManagedChainLayout(
          cardanoNode.state
        );
        if (currentGeneration !== directoryChangeGeneration) {
          return null;
        }
        await chainStorageCoordinator.wipeChainAndSnapshots(
          'wipe-chain flag set. Wiped chain directory and Mithril snapshots.',
          cardanoNode.state
        );
        if (currentGeneration !== directoryChangeGeneration) {
          return null;
        }
      } else {
        layoutResult = await chainStorageCoordinator.ensureManagedChainLayout(
          cardanoNode.state
        );
        if (currentGeneration !== directoryChangeGeneration) {
          return null;
        }
      }
      const lockExists = await fs.pathExists(mithrilLockFilePath);
      if (currentGeneration !== directoryChangeGeneration) {
        return null;
      }
      if (lockExists) {
        if (currentGeneration !== directoryChangeGeneration) {
          return null;
        }
        await chainStorageCoordinator.wipeChainAndSnapshots(
          'Incomplete bootstrap detected. Wiped chain directory and Mithril snapshots.',
          cardanoNode.state
        );
        if (currentGeneration !== directoryChangeGeneration) {
          return null;
        }
        mithrilDecisionPrompted = false;
        mithrilDecision = null;
      }
    } catch (error) {
      logger.error('[MITHRIL] Failed to verify managed chain layout', {
        error,
      });
      throw markManagedChainLayoutError(error);
    }

    mithrilStartupCheckDone = true;
    mithrilStartupLayoutResult = layoutResult;
    return layoutResult;
  };

  onMithrilBootstrapStatus(async (status) => {
    if (status.status !== 'completed' || mithrilStartInFlight) return;
    if (!canStartNodeAfterMithrilCompletion()) return;
    mithrilBootstrapCompleted = true;
    await startNodeAfterMithrilCompletion();
  });

  onMithrilBootstrapStatus((status) => {
    if (status.status !== 'failed') return;
    if (mithrilFailureDecisionInFlight) return;
    mithrilFailureDecisionInFlight = true;
    const currentGeneration = directoryChangeGeneration;
    waitForMithrilBootstrapDecision()
      .then(async (decision) => {
        if (decision !== 'decline') return;
        await handleMithrilFailureDecline('status-listener', currentGeneration);
      })
      .catch((error) => {
        if (isMithrilDecisionCancelledError(error)) {
          logger.info(
            '[MITHRIL] Decision wait cancelled after directory change following bootstrap failure',
            null
          );
          return;
        }
        logger.error('[MITHRIL] Decision wait failed after failure', { error });
      })
      .finally(() => {
        mithrilFailureDecisionInFlight = false;
      });
  });

  onMithrilBootstrapDecision((decision) => {
    if (decision !== 'decline') return;
    if (getMithrilBootstrapStatus().status !== 'failed') return;
    const currentGeneration = directoryChangeGeneration;
    handleMithrilFailureDecline('decision-listener', currentGeneration).catch(
      (error) => {
        logger.error('[MITHRIL] Decline handling failed after decision', {
          error,
        });
      }
    );
  });

  onMithrilBootstrapDecision((decision) => {
    if (decision !== 'decline') return;
    if (getMithrilBootstrapStatus().status !== 'cancelled') return;
    const currentGeneration = directoryChangeGeneration;
    handleMithrilCancelledDecline('decision-listener', currentGeneration).catch(
      (error) => {
        logger.error('[MITHRIL] Decline handling failed after cancel', {
          error,
        });
      }
    );
  });

  const runHandleCheckDiskSpace = async (
    hadNotEnoughSpaceLeft?: boolean,
    forceDiskSpaceRequired?: number
  ): Promise<CheckDiskSpaceResponse> => {
    const hadNotEnoughSpaceFlag = hadNotEnoughSpaceLeft ?? false;
    const currentGeneration = directoryChangeGeneration;
    const diskSpaceRequired = forceDiskSpaceRequired || DISK_SPACE_REQUIRED;
    const getStaleResponse = (): CheckDiskSpaceResponse => ({
      isNotEnoughDiskSpace,
      diskSpaceRequired: '',
      diskSpaceMissing: '',
      diskSpaceRecommended: '',
      diskSpaceAvailable: '',
      hadNotEnoughSpaceLeft: hadNotEnoughSpaceFlag,
      diskSpaceAvailableRaw: 0,
      diskTotalSpaceRaw: 0,
      isError: false,
    });
    const startupLayoutResult = await ensureMithrilStartupGate(
      currentGeneration
    );
    if (
      !startupLayoutResult ||
      currentGeneration !== directoryChangeGeneration
    ) {
      return getStaleResponse();
    }
    const response = await getDiskCheckReport(
      startupLayoutResult.managedChainPath
    );
    if (currentGeneration !== directoryChangeGeneration) {
      return getStaleResponse();
    }
    const latestDecision = getPendingMithrilBootstrapDecision();
    if (latestDecision && latestDecision !== mithrilDecision) {
      mithrilDecision = latestDecision;
    }

    if (response.isError) {
      logger.info(
        '[DISK-SPACE-DEBUG] We could not check disk space, but we will try to start cardano-node anyway',
        null
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
      response.hadNotEnoughSpaceLeft = hadNotEnoughSpaceFlag;
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
      hadNotEnoughSpaceFlag;

    try {
      switch (true) {
        case NO_SPACE_AND_CARDANO_NODE_CAN_BE_STOPPED:
          try {
            logger.info('[DISK-SPACE-DEBUG] Stopping cardano node', null);
            await cardanoNode.stop();
          } catch (error) {
            logger.error('[DISK-SPACE-DEBUG] Cannot stop cardano node', error);
          }

          break;

        case CARDANO_NODE_CAN_BE_STARTED_FOR_THE_FIRST_TIME:
          try {
            const chainEmpty = await chainStorageCoordinator.isManagedChainEmpty();
            if (currentGeneration !== directoryChangeGeneration) {
              return getStaleResponse();
            }
            if (chainEmpty) {
              const mithrilStatus = getMithrilBootstrapStatus();
              if (
                isMithrilBootstrapRestoreCompleteStatus(mithrilStatus.status)
              ) {
                mithrilBootstrapCompleted = true;
              }

              if (mithrilStatus.status === 'failed') {
                await handleMithrilFailureDecline(
                  'polling-chain-empty',
                  currentGeneration
                );
                response.hadNotEnoughSpaceLeft = false;
                break;
              }

              if (mithrilStatus.status === 'cancelled') {
                await handleMithrilCancelledDecline('polling-chain-empty');
                response.hadNotEnoughSpaceLeft = false;
                break;
              }

              if (mithrilBootstrapCompleted) {
                await startNodeAfterMithrilCompletion();
                break;
              }
              if (mithrilDecision === 'accept') {
                break;
              }
              if (!mithrilDecisionPrompted) {
                logger.info('[MITHRIL] Emitting decision status');
                await emitMithrilDecisionStatus();
                mithrilDecisionPrompted = true;
              }
              if (mithrilDecision === 'decline') {
                logger.info('[MITHRIL] Processing immediate decline decision');
                await emitMithrilIdleStatus();
                if (currentGeneration !== directoryChangeGeneration) {
                  return getStaleResponse();
                }
                await chainStorageCoordinator.wipeChainAndSnapshots(
                  'User declined Mithril bootstrap. Wiped chain directory and Mithril snapshots.',
                  cardanoNode.state
                );
                if (currentGeneration !== directoryChangeGeneration) {
                  return getStaleResponse();
                }
                logger.info(
                  '[MITHRIL] Starting cardano-node after bootstrap decline'
                );
                await cardanoNode.start();
                break;
              }
              if (!mithrilDecisionInFlight) {
                mithrilDecisionInFlight = true;
                waitForMithrilBootstrapDecision()
                  .then(async (decision) => {
                    if (currentGeneration !== directoryChangeGeneration) return;
                    logger.info(
                      '[MITHRIL] Bootstrap decision waiter resolved',
                      {
                        decision,
                      }
                    );
                    mithrilDecision = decision;
                    mithrilDecisionPrompted = decision !== 'accept';
                    if (decision === 'decline') {
                      await emitMithrilIdleStatus();
                      if (currentGeneration !== directoryChangeGeneration) {
                        return;
                      }
                      await chainStorageCoordinator.wipeChainAndSnapshots(
                        'User declined Mithril bootstrap. Wiped chain directory and Mithril snapshots.',
                        cardanoNode.state
                      );
                      if (currentGeneration !== directoryChangeGeneration) {
                        return;
                      }
                      logger.info(
                        '[MITHRIL] Starting cardano-node after waited bootstrap decline'
                      );
                      await cardanoNode.start();
                    }
                  })
                  .catch((error) => {
                    if (isMithrilDecisionCancelledError(error)) {
                      logger.info(
                        '[MITHRIL] Decision wait cancelled after directory change',
                        null
                      );
                      return;
                    }
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
              await handleMithrilFailureDecline(
                'polling-chain-present',
                currentGeneration
              );
              response.hadNotEnoughSpaceLeft = false;
              break;
            }

            if (mithrilStatus.status === 'cancelled') {
              await handleMithrilCancelledDecline(
                'polling-chain-present',
                currentGeneration
              );
              response.hadNotEnoughSpaceLeft = false;
              break;
            }

            await emitMithrilIdleStatus();
            if (currentGeneration !== directoryChangeGeneration) {
              return getStaleResponse();
            }
            mithrilDecisionInFlight = false;
            mithrilDecision = null;
            mithrilDecisionPrompted = false;
            resetMithrilDecisionState({ suppressStatusBroadcast: true });
            await cardanoNode.start();
          } catch (error) {
            logger.error('[MITHRIL] Failed to handle bootstrap decision', {
              error,
            });
            if (isManagedChainLayoutError(error)) {
              throw error;
            }

            if (currentGeneration !== directoryChangeGeneration) {
              return getStaleResponse();
            }

            try {
              await cardanoNode.start();
            } catch (startError) {
              logger.error(
                '[MITHRIL] Fallback cardano-node start failed after bootstrap decision error',
                {
                  error: startError,
                }
              );
            }
          }
          break;

        case CARDANO_NODE_CAN_BE_STARTED_AFTER_FREEING_SPACE:
          try {
            logger.info(
              '[DISK-SPACE-DEBUG] restart cardano node after freeing up disk space',
              null
            );
            if (cardanoNode._startupTries > 0) await cardanoNode.restart();
            else {
              await cardanoNode.start();
              if (currentGeneration !== directoryChangeGeneration) {
                return getStaleResponse();
              }
            }
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
      if (isManagedChainLayoutError(error)) {
        throw error;
      }
      logger.error('[DISK-SPACE-DEBUG] Unknown error', error);
      resetInterval(DISK_SPACE_CHECK_MEDIUM_INTERVAL);
    }

    await getDiskSpaceStatusChannel.send(response, mainWindow.webContents);
    return response;
  };

  const flushPendingDiskSpaceCheck = () => {
    if (activeDiskSpaceCheckPromise || !pendingDiskSpaceCheckArgs) return;

    const nextArgs = pendingDiskSpaceCheckArgs;
    const nextWaiters = pendingDiskSpaceCheckWaiters;

    pendingDiskSpaceCheckArgs = null;
    pendingDiskSpaceCheckWaiters = [];

    const nextPromise = startDiskSpaceCheckRun(nextArgs);

    nextPromise.then(
      (response) => {
        nextWaiters.forEach(({ resolve }) => resolve(response));
      },
      (error) => {
        nextWaiters.forEach(({ reject }) => reject(error));
      }
    );
  };

  const startDiskSpaceCheckRun = ({
    hadNotEnoughSpaceLeft,
    forceDiskSpaceRequired,
  }: {
    hadNotEnoughSpaceLeft?: boolean;
    forceDiskSpaceRequired?: number;
  }): Promise<CheckDiskSpaceResponse> => {
    const diskSpaceCheckPromise = runHandleCheckDiskSpace(
      hadNotEnoughSpaceLeft,
      forceDiskSpaceRequired
    );

    activeDiskSpaceCheckPromise = diskSpaceCheckPromise;

    diskSpaceCheckPromise.then(
      () => {
        if (activeDiskSpaceCheckPromise === diskSpaceCheckPromise) {
          activeDiskSpaceCheckPromise = null;
        }
        flushPendingDiskSpaceCheck();
      },
      () => {
        if (activeDiskSpaceCheckPromise === diskSpaceCheckPromise) {
          activeDiskSpaceCheckPromise = null;
        }
        flushPendingDiskSpaceCheck();
      }
    );

    return diskSpaceCheckPromise;
  };

  const mergePendingDiskSpaceCheckArgs = (nextArgs: {
    hadNotEnoughSpaceLeft?: boolean;
    forceDiskSpaceRequired?: number;
  }) => {
    if (!pendingDiskSpaceCheckArgs) {
      pendingDiskSpaceCheckArgs = nextArgs;
      return;
    }

    pendingDiskSpaceCheckArgs = {
      hadNotEnoughSpaceLeft:
        pendingDiskSpaceCheckArgs.hadNotEnoughSpaceLeft === false ||
        nextArgs.hadNotEnoughSpaceLeft === false
          ? false
          : nextArgs.hadNotEnoughSpaceLeft ??
            pendingDiskSpaceCheckArgs.hadNotEnoughSpaceLeft,
      forceDiskSpaceRequired:
        nextArgs.forceDiskSpaceRequired ??
        pendingDiskSpaceCheckArgs.forceDiskSpaceRequired,
    };
  };

  const launchHandleCheckDiskSpace = (
    hadNotEnoughSpaceLeft?: boolean,
    forceDiskSpaceRequired?: number
  ): Promise<CheckDiskSpaceResponse> => {
    const args = {
      hadNotEnoughSpaceLeft,
      forceDiskSpaceRequired,
    };

    if (!activeDiskSpaceCheckPromise) {
      return startDiskSpaceCheckRun(args);
    }

    mergePendingDiskSpaceCheckArgs(args);

    return new Promise<CheckDiskSpaceResponse>((resolve, reject) => {
      pendingDiskSpaceCheckWaiters.push({ resolve, reject });
    });
  };

  const resetOnDirectoryChange = () => {
    mithrilDecisionInFlight = false;
    mithrilFailureDecisionInFlight = false;
    mithrilFailureDeclineInFlight = false;
    mithrilCancelledDeclineInFlight = false;
    mithrilStartupCheckDone = false;
    mithrilStartupLayoutResult = null;
    mithrilDecisionPrompted = false;
    mithrilDecision = null;
    mithrilBootstrapCompleted = false;
    mithrilStartInFlight = false;
    resetMithrilDecisionState({ suppressStatusBroadcast: true });
    directoryChangeGeneration += 1;
    launchHandleCheckDiskSpace(false).catch((error) => {
      if (isMithrilDecisionCancelledError(error)) {
        logger.info(
          '[MITHRIL] Immediate disk-space recheck cancelled after directory change',
          null
        );
        return;
      }

      logger.error(
        '[MITHRIL] Immediate disk-space recheck after directory change failed',
        {
          error,
        }
      );
    });
  };

  const handleCheckDiskSpace = (
    hadNotEnoughSpaceLeft?: boolean,
    forceDiskSpaceRequired?: number
  ): Promise<CheckDiskSpaceResponse> =>
    launchHandleCheckDiskSpace(hadNotEnoughSpaceLeft, forceDiskSpaceRequired);

  chainStorageCoordinator.onDirectoryChanged(resetOnDirectoryChange);

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
    diskSpaceCheckInterval = setInterval(() => {
      handleCheckDiskSpace(hadNotEnoughSpaceLeft)
        .then((response) => {
          hadNotEnoughSpaceLeft = response?.hadNotEnoughSpaceLeft;
        })
        .catch((error) => {
          if (isMithrilDecisionCancelledError(error)) {
            logger.info(
              '[MITHRIL] Background disk-space poll cancelled after directory change',
              null
            );
            return;
          }

          logger.error('[MITHRIL] Background disk-space poll failed', {
            error,
          });
        });
    }, interval);
    diskSpaceCheckIntervalLength = interval;
  };

  // Start default interval
  setDiskSpaceIntervalChecking(diskSpaceCheckIntervalLength);
  getDiskSpaceStatusChannel.onReceive(async () => {
    const diskSpacePath = await chainStorageManager.resolveDiskSpaceCheckPath();
    const diskReport = await getDiskCheckReport(diskSpacePath);
    await getDiskSpaceStatusChannel.send(diskReport, mainWindow.webContents);
    return diskReport;
  });
  return handleCheckDiskSpace;
};
