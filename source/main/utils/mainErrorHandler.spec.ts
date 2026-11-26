import mainErrorHandler from './mainErrorHandler';
import { logger } from './logging';

const mockApp = { on: jest.fn() };

jest.mock('electron', () => ({
  app: {
    on: (...args: Array<unknown>) => mockApp.on(...args),
  },
}));

jest.mock('./logging', () => ({
  logger: {
    info: jest.fn(),
    error: jest.fn(),
  },
}));

const getHandler = (event: string) => {
  const call = (process.on as jest.Mock).mock.calls.find(
    ([name]) => name === event
  );
  return call[1] as (error: unknown) => void;
};

describe('mainErrorHandler', () => {
  let processOn: jest.SpyInstance;
  let consoleError: jest.SpyInstance;

  beforeEach(() => {
    processOn = jest.spyOn(process, 'on').mockImplementation(() => process);
    consoleError = jest.spyOn(console, 'error').mockImplementation(() => {});
  });

  afterEach(() => {
    processOn.mockRestore();
    consoleError.mockRestore();
  });

  it('logs an uncaught exception and calls onError', () => {
    const onError = jest.fn();
    mainErrorHandler(onError);

    getHandler('uncaughtException')(new TypeError('failure'));

    expect(logger.error).toHaveBeenCalledWith(
      'uncaughtException',
      expect.anything()
    );
    expect(onError).toHaveBeenCalledTimes(1);
    expect(consoleError).not.toHaveBeenCalled();
  });

  it('does not throw when the logger throws and still calls onError', () => {
    (logger.error as jest.Mock).mockImplementationOnce(() => {
      throw new TypeError('Cannot read properties of undefined');
    });
    const onError = jest.fn();
    mainErrorHandler(onError);

    expect(() =>
      getHandler('uncaughtException')(new Error('failure'))
    ).not.toThrow();
    expect(onError).toHaveBeenCalledTimes(1);
    expect(consoleError).toHaveBeenCalledTimes(1);
  });
});
