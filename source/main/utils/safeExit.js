// @flow
import { pauseActiveDownloads } from '../ipc/downloadManagerChannel';
import { CardanoNodeStates } from '../../common/types/cardano-node.types';
import { logger } from './logging';
import { safeExitDaedalusWithCode } from './safeExitDaedalusWithCode';
import { CardanoNode } from '../cardano/CardanoNode';

export const safeExit = async (
  cardanoNode: ?CardanoNode,
  exitCode = 0
): Promise<void> => {
  pauseActiveDownloads();

  if (!cardanoNode || cardanoNode.state === CardanoNodeStates.STOPPED) {
    logger.info('Daedalus:safeExit: exiting Daedalus with code 0', { code: 0 });
    safeExitDaedalusWithCode(exitCode);
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
    safeExitDaedalusWithCode(exitCode);
  } catch (error) {
    logger.error('Daedalus:safeExit: cardano-node did not exit correctly', {
      error,
    });
    safeExitDaedalusWithCode(exitCode);
  }
};
