import { logger } from '../../../utils/logging';

type LedgerOperation = 'getVersion' | 'getExtendedPublicKey' | 'getSerial';

// Error instances lose their custom fields across Electron IPC and their
// non-enumerable message in our JSON logger. Send only diagnostic scalars.
export const runLedgerOperation = async <T>(
  operation: LedgerOperation,
  action: () => Promise<T>
): Promise<T> => {
  logger.info('[HW-DEBUG] Ledger operation started', { operation });
  try {
    const result = await action();
    logger.info('[HW-DEBUG] Ledger operation completed', { operation });
    return result;
  } catch (error) {
    let statusCode: number | undefined;
    if (typeof error?.statusCode === 'number') {
      statusCode = error.statusCode;
    } else if (typeof error?.code === 'number') {
      statusCode = error.code;
    }
    const details = {
      operation,
      name: typeof error?.name === 'string' ? error.name : 'Error',
      message:
        typeof error?.message === 'string' && error.message
          ? error.message
          : 'Ledger operation failed',
      ...(typeof error?.code === 'string' || typeof error?.code === 'number'
        ? { code: error.code }
        : {}),
      ...(statusCode !== undefined ? { statusCode } : {}),
    };
    logger.error('[HW-DEBUG] Ledger operation failed', details);
    throw details;
  }
};
