import {
  requestDownloadChannel,
  requestResumeDownloadChannel,
  deleteDownloadedFile,
  getDownloadLocalDataChannel,
  clearDownloadLocalDataChannel,
  checkFileExistsChannel,
  reuseExistingDownloadChannel,
} from '../ipc/downloadManagerChannel';

jest.mock('../ipc/downloadManagerChannel', () => ({
  requestDownloadChannel: { request: jest.fn(), onReceive: jest.fn() },
  requestResumeDownloadChannel: { request: jest.fn() },
  deleteDownloadedFile: { request: jest.fn() },
  getDownloadLocalDataChannel: { request: jest.fn() },
  clearDownloadLocalDataChannel: { request: jest.fn() },
  checkFileExistsChannel: { request: jest.fn() },
  reuseExistingDownloadChannel: { request: jest.fn() },
}));
jest.mock('../utils/logging', () => ({
  logger: { info: jest.fn(), error: jest.fn(), warn: jest.fn() },
}));
jest.mock('../ipc/manageAppUpdateChannel', () => ({
  manageAppUpdateChannel: { request: jest.fn(), onReceive: jest.fn() },
}));

const request = (channel: unknown) =>
  (channel as { request: jest.Mock }).request;
const requestDownload = request(requestDownloadChannel);
const resumeDownload = request(requestResumeDownloadChannel);
const deleteFile = request(deleteDownloadedFile);
const getLocalData = request(getDownloadLocalDataChannel);
const clearLocalData = request(clearDownloadLocalDataChannel);
const checkFileExists = request(checkFileExistsChannel);
const reuseDownload = request(reuseExistingDownloadChannel);

const hash = 'a'.repeat(64);
const fileUrl = 'https://example.test/Daedalus-11.7.0-mainnet-x64.exe';
const update = {
  softwareUpdate: {
    win32: { version: '11.7.0', hash, url: fileUrl },
  },
};
const existing = {
  info: { fileUrl, originalFilename: 'Daedalus-11.7.0-mainnet-x64.exe' },
  data: { state: 'FINISHED', progress: 100 },
};

const createStore = (newsFeed: Record<string, unknown>) => {
  const AppUpdateStore = require('./AppUpdateStore').default;
  const resolved = (value?: unknown) => jest.fn(() => Promise.resolve(value));
  const api = {
    localStorage: {
      getAppAutomaticUpdateFailed: resolved(false),
      setAppAutomaticUpdateFailed: resolved(),
      unsetAppAutomaticUpdateFailed: resolved(),
      getAppUpdateCompleted: resolved(false),
      setAppUpdateCompleted: resolved(),
      unsetAppUpdateCompleted: resolved(),
    },
  };
  const store = new AppUpdateStore(api, {}, null);
  store.configure({
    newsFeed,
    profile: { currentLocale: 'en-US' },
  });
  return store;
};

describe('AppUpdateStore', () => {
  beforeAll(() => {
    global.environment = {
      ...global.environment,
      version: '11.6.1',
      platform: 'win32',
      isAppleSilicon: false,
    };
  });

  beforeEach(() => {
    jest.clearAllMocks();
    getLocalData.mockResolvedValue({});
    clearLocalData.mockResolvedValue(undefined);
    reuseDownload.mockResolvedValue(null);
    checkFileExists.mockResolvedValue(true);
  });

  describe('_checkNewAppUpdate', () => {
    it('offers the install without a download when the local installer matches the newsfeed hash', async () => {
      reuseDownload.mockResolvedValue(existing);
      const store = createStore({});

      await store._checkNewAppUpdate(update);

      expect(reuseDownload).toHaveBeenCalledWith(
        expect.objectContaining({ fileUrl, hash })
      );
      expect(requestDownload).not.toHaveBeenCalled();
      expect(store.isUpdateDownloaded).toBe(true);
      expect(store.downloadInfo).toEqual(existing.info);
      expect(store.downloadData).toEqual(existing.data);
      expect(store.displayAppUpdateOverlay).toBe(true);
    });

    it('requests a download when the local installer does not match the newsfeed hash', async () => {
      reuseDownload.mockResolvedValue(null);
      const store = createStore({});

      await store._checkNewAppUpdate(update);

      expect(requestDownload).toHaveBeenCalledWith(
        expect.objectContaining({ fileUrl })
      );
      expect(store.isUpdateDownloaded).toBe(false);
    });

    it('requests a download when there is no local installer', async () => {
      const store = createStore({});

      await store._checkNewAppUpdate(update);

      expect(reuseDownload).toHaveBeenCalledTimes(1);
      expect(requestDownload).toHaveBeenCalledTimes(1);
      expect(store.isUpdateDownloaded).toBe(false);
    });

    it('requests a download when the local installer cannot be checked', async () => {
      reuseDownload.mockRejectedValue(new Error('IPC failure'));
      const store = createStore({});

      await store._checkNewAppUpdate(update);

      expect(requestDownload).toHaveBeenCalledTimes(1);
      expect(store.isUpdateDownloaded).toBe(false);
    });

    it('checks the local installer after clearing a record for another file', async () => {
      getLocalData.mockResolvedValue({
        info: { fileUrl: 'https://example.test/old.exe' },
        data: { state: 'FINISHED', progress: 100 },
      });
      reuseDownload.mockResolvedValue(existing);
      const store = createStore({});

      await store._checkNewAppUpdate(update);

      expect(clearLocalData).toHaveBeenCalledTimes(1);
      expect(clearLocalData.mock.invocationCallOrder[0]).toBeLessThan(
        reuseDownload.mock.invocationCallOrder[0]
      );
      expect(requestDownload).not.toHaveBeenCalled();
      expect(store.isUpdateDownloaded).toBe(true);
    });

    it('resumes a pending download without checking for a local installer', async () => {
      getLocalData.mockResolvedValue({
        info: { fileUrl },
        data: { state: 'PAUSED', progress: 40 },
      });
      const store = createStore({});

      await store._checkNewAppUpdate(update);

      expect(resumeDownload).toHaveBeenCalledTimes(1);
      expect(reuseDownload).not.toHaveBeenCalled();
      expect(requestDownload).not.toHaveBeenCalled();
    });
  });

  describe('_watchForNewsfeedUpdates', () => {
    it('keeps the installer when the newsfeed could not be fetched or verified', async () => {
      const store = createStore({
        newsFeedData: { update: undefined },
        isLoadingNews: true,
      });

      await store._watchForNewsfeedUpdates();

      expect(deleteFile).not.toHaveBeenCalled();
      expect(clearLocalData).not.toHaveBeenCalled();
    });

    it('clears the installer when a verified newsfeed has no update', async () => {
      const store = createStore({
        newsFeedData: { update: undefined },
        isLoadingNews: false,
      });

      await store._watchForNewsfeedUpdates();

      expect(deleteFile).toHaveBeenCalledTimes(1);
      expect(clearLocalData).toHaveBeenCalledTimes(1);
    });
  });
});
