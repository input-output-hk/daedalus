// @flow
import { pauseActiveDownloads } from '../ipc/downloadManagerChannel';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import { logger } from './logging';
import { safeExitWithCode } from './safeExitWithCode';
import { CardanoNode } from '../cardano/CardanoNode';

export const safeExit = async (
  cardanoNode: ?CardanoNode,
  relaunch: boolean = false
): Promise<void> => {
  pauseActiveDownloads();
  if (!cardanoNode || cardanoNode.state === CardanoNodeStates.STOPPED) {
    logger.info('Daedalus:safeExit: exiting Daedalus with code 0', { code: 0 });
    safeExitWithCode(0, relaunch);
    return;
  }

  if (cardanoNode.state === CardanoNodeStates.STOPPING) {
    logger.info('Daedalus:safeExit: waiting for cardano-node to stop...');
    cardanoNode.exitOnStop();
  }

  try {
    const pid = cardanoNode.pid || 'null';
    logger.info(`Daedalus:safeExit: stopping cardano-node with PID: ${pid}`, {
      pid,
    });
    await cardanoNode.stop();
    logger.info('Daedalus:safeExit: exiting Daedalus with code 0', { code: 0 });
    safeExitWithCode(0, relaunch);
  } catch (error) {
    logger.error('Daedalus:safeExit: cardano-node did not exit correctly', {
      error,
    });
    safeExitWithCode(0, relaunch);
  }
};
