/* eslint-disable @typescript-eslint/no-explicit-any, prefer-destructuring, jest/no-standalone-expect, jest/valid-expect */

import path from 'path';
import { expect } from 'chai';
import { After, Given, Then, When } from 'cucumber';
import type {
  ChainStorageValidation,
  MithrilBootstrapErrorStage,
  MithrilBootstrapStatus,
  MithrilBootstrapStatusUpdate,
  MithrilProgressItem,
  MithrilSnapshotItem,
} from '../../../../source/common/types/mithril-bootstrap.types';
import { DEFAULT_TIMEOUT } from '../../../common/e2e/steps/config';

const DEFAULT_CHAIN_PATH = '/tmp/daedalus/mithril-default-chain';
const CUSTOM_CHAIN_PATH = '/tmp/daedalus/mithril-custom-chain';
// Spectron's file dialog stub returns an existing filesystem path; the actual path content is irrelevant for chain-storage selection tests.
const SELECTED_CHAIN_PATH = path.resolve(
  __dirname,
  '../../../wallets/e2e/documents/default-wallet.key'
);

const SELECTORS = {
  OVERLAY: '.MithrilBootstrap_component',
  CHAIN_STORAGE_ROOT: '.ChainStorageLocationPicker_root',
  CHAIN_STORAGE_INPUT: '.ChainStorageLocationPicker_storageInput',
  CHAIN_STORAGE_SELECT_BUTTON:
    '.ChainStorageLocationPicker_selectDirectoryButton',
  CHAIN_STORAGE_RESET_BUTTON: '.ChainStorageLocationPicker_inlineAction',
  CHAIN_STORAGE_CONTINUE_BUTTON: '.ChainStorageLocationPicker_primaryAction',
  CHAIN_STORAGE_VALIDATION_MESSAGE:
    '.ChainStorageLocationPicker_validationMessage',
  DECISION_ROOT: '.MithrilDecisionView_root',
  DECISION_LOCATION_VALUE: '.MithrilDecisionView_locationValue',
  DECISION_ACCEPT_BUTTON: '.MithrilDecisionView_primaryAction',
  DECISION_DECLINE_BUTTON: '.MithrilDecisionView_secondaryAction',
  PROGRESS_ROOT: '.MithrilProgressView_root',
  PROGRESS_CANCEL_BUTTON: '.MithrilProgressView_secondaryAction',
  ERROR_ROOT: '.MithrilErrorView_root',
  HEADING_CHAIN_STORAGE: '#mithril-chain-storage-title',
  HEADING_DECISION: '#mithril-bootstrap-title',
  HEADING_PROGRESS: '#mithril-progress-title',
  HEADING_ERROR: '#mithril-error-title',
};

const IPC_CHANNELS = {
  MITHRIL_STATUS_BROADCAST: 'MITHRIL_BOOTSTRAP_STATUS_CHANNEL-broadcast',
  MITHRIL_STATUS_RESPONSE: 'MITHRIL_BOOTSTRAP_STATUS_CHANNEL-response',
};

const BASE_SNAPSHOT: MithrilSnapshotItem = {
  digest: '4f1f9e7e2f2dd9e5d70dcd2ca7b3148ef3cb33f0c6480b9b58e1c2f8d75c00aa',
  createdAt: '2026-03-05T12:00:00.000Z',
  size: 256000000,
  cardanoNodeVersion: '10.1.4',
  network: 'mainnet',
};

const ERROR_TITLE_IDS: Record<MithrilBootstrapErrorStage, string> = {
  download: 'loading.mithrilBootstrap.error.download.title',
  verify: 'loading.mithrilBootstrap.error.verify.title',
  convert: 'loading.mithrilBootstrap.error.convert.title',
  'node-start': 'loading.mithrilBootstrap.error.nodeStart.title',
};

const STEP_LABEL_IDS: Record<
  'preparing' | 'downloading' | 'finalizing',
  string
> = {
  preparing: 'loading.mithrilBootstrap.step.preparing',
  downloading: 'loading.mithrilBootstrap.step.downloading',
  finalizing: 'loading.mithrilBootstrap.step.finalizing',
};

const INSUFFICIENT_SPACE_MESSAGE_ID =
  'chainStorage.locationPicker.validation.insufficientSpace';

type SeedView = 'chain-storage' | 'decision' | 'progress' | 'error';

type SeedOptions = {
  view: SeedView;
  status: MithrilBootstrapStatus;
  storageLocationConfirmed: boolean;
  customChainPath: string | null;
  isChainStorageLoading: boolean;
  errorStage?: MithrilBootstrapErrorStage;
  validationByPath: Record<string, ChainStorageValidation>;
  defaultChainPath: string;
  defaultChainStorageValidation: ChainStorageValidation;
  snapshot: MithrilSnapshotItem;
  progressItems: MithrilProgressItem[];
  filesDownloaded?: number;
  filesTotal?: number;
  elapsedSeconds?: number;
};

const createDefaultValidation = (): ChainStorageValidation => ({
  isValid: true,
  path: null,
  resolvedPath: DEFAULT_CHAIN_PATH,
  availableSpaceBytes: BASE_SNAPSHOT.size * 4,
  requiredSpaceBytes: BASE_SNAPSHOT.size,
});

const createProgressItems = (
  status: MithrilBootstrapStatus
): MithrilProgressItem[] => {
  if (status === 'downloading') {
    return [
      {
        id: 'step-3',
        label: 'downloading-snapshot',
        state: 'active',
      },
    ];
  }

  if (status === 'finalizing') {
    return [
      {
        id: 'step-3',
        label: 'downloading-snapshot',
        state: 'completed',
      },
      {
        id: 'install-snapshot',
        label: 'install-snapshot',
        state: 'active',
      },
    ];
  }

  return [];
};

const createSeedOptions = (
  view: SeedView,
  overrides: Partial<SeedOptions> = {}
): SeedOptions => {
  const defaultChainStorageValidation = createDefaultValidation();
  const base: SeedOptions = {
    view,
    status: 'decision',
    storageLocationConfirmed: view !== 'chain-storage',
    customChainPath: null,
    isChainStorageLoading: false,
    errorStage: undefined,
    validationByPath: {},
    defaultChainPath: DEFAULT_CHAIN_PATH,
    defaultChainStorageValidation,
    snapshot: BASE_SNAPSHOT,
    progressItems: [],
    filesDownloaded: 0,
    filesTotal: undefined,
    elapsedSeconds: undefined,
  };

  if (view === 'progress') {
    base.status = 'preparing';
    base.progressItems = [];
    base.filesDownloaded = 0;
    base.filesTotal = 12;
    base.elapsedSeconds = 5;
  }

  if (view === 'error') {
    base.status = 'failed';
    base.errorStage = 'download';
  }

  return {
    ...base,
    ...overrides,
    validationByPath: {
      ...base.validationByPath,
      ...(overrides.validationByPath || {}),
    },
  };
};

const seedMithrilBootstrap = async function (seedOptions: SeedOptions) {
  await this.client.execute(
    (seed: SeedOptions, channels) => {
      const { runInAction } = require('mobx');
      const runtimeWindow = window as any;
      const networkStatus = daedalus.stores.networkStatus;
      const mithrilBootstrap = daedalus.stores.mithrilBootstrap;

      if (runtimeWindow.__mithrilBootstrapE2E) {
        return;
      }

      const defaultValidation = seed.defaultChainStorageValidation;
      const currentValidation = seed.customChainPath
        ? {
            isValid: true,
            path: seed.customChainPath,
            resolvedPath: seed.customChainPath,
            availableSpaceBytes: defaultValidation.availableSpaceBytes,
            requiredSpaceBytes: defaultValidation.requiredSpaceBytes,
          }
        : defaultValidation;

      runtimeWindow.__mithrilBootstrapE2E = {
        originals: {
          polling: {
            status: Boolean(networkStatus._networkStatusPollingInterval),
            clock: Boolean(networkStatus._networkClockPollingInterval),
            parameters: Boolean(
              networkStatus._networkParametersPollingInterval
            ),
          },
          networkStatus: {
            isNodeResponding: networkStatus.isNodeResponding,
            isNodeSyncing: networkStatus.isNodeSyncing,
            isNodeInSync: networkStatus.isNodeInSync,
            isNodeTimeCorrect: networkStatus.isNodeTimeCorrect,
            isSystemTimeIgnored: networkStatus.isSystemTimeIgnored,
            isNotEnoughDiskSpace: networkStatus.isNotEnoughDiskSpace,
          },
          mithrilBootstrap: {
            status: mithrilBootstrap.status,
            snapshot: mithrilBootstrap.snapshot,
            filesDownloaded: mithrilBootstrap.filesDownloaded,
            filesTotal: mithrilBootstrap.filesTotal,
            elapsedSeconds: mithrilBootstrap.elapsedSeconds,
            error: mithrilBootstrap.error,
            snapshots: mithrilBootstrap.snapshots,
            isFetchingSnapshots: mithrilBootstrap.isFetchingSnapshots,
            customChainPath: mithrilBootstrap.customChainPath,
            defaultChainPath: mithrilBootstrap.defaultChainPath,
            defaultChainStorageValidation:
              mithrilBootstrap.defaultChainStorageValidation,
            chainStorageValidation: mithrilBootstrap.chainStorageValidation,
            isChainStorageLoading: mithrilBootstrap.isChainStorageLoading,
            storageLocationConfirmed: mithrilBootstrap.storageLocationConfirmed,
            ancillaryBytesDownloaded: mithrilBootstrap.ancillaryBytesDownloaded,
            ancillaryBytesTotal: mithrilBootstrap.ancillaryBytesTotal,
            progressItems: mithrilBootstrap.progressItems,
            bootstrapStartedAt: mithrilBootstrap.bootstrapStartedAt,
          },
          methods: {
            syncStatus: mithrilBootstrap.syncStatus,
            loadSnapshots: mithrilBootstrap.loadSnapshots,
            loadChainStorageConfig: mithrilBootstrap.loadChainStorageConfig,
            setDecision: mithrilBootstrap.setDecision,
            startBootstrap: mithrilBootstrap.startBootstrap,
            cancelBootstrap: mithrilBootstrap.cancelBootstrap,
            setChainStorageDirectory: mithrilBootstrap.setChainStorageDirectory,
            resetChainStorageDirectory:
              mithrilBootstrap.resetChainStorageDirectory,
            validateChainStorageDirectory:
              mithrilBootstrap.validateChainStorageDirectory,
          },
        },
        defaultValidation,
        validationByPath: seed.validationByPath,
        snapshot: seed.snapshot,
      };

      networkStatus._clearNetworkStatusPollingInterval();
      networkStatus._clearNetworkClockPollingInterval();
      networkStatus._clearNetworkParametersPollingInterval();

      mithrilBootstrap.syncStatus = async () => Promise.resolve();
      mithrilBootstrap.loadSnapshots = async () => Promise.resolve();
      mithrilBootstrap.loadChainStorageConfig = async () => Promise.resolve();

      mithrilBootstrap.setDecision = async (decision: string) => {
        if (decision === 'decline') {
          runInAction(() => {
            mithrilBootstrap.status = 'idle';
            mithrilBootstrap.storageLocationConfirmed = false;
            mithrilBootstrap.error = null;
          });
        }

        return Promise.resolve();
      };

      mithrilBootstrap.startBootstrap = async (digest?: string) => {
        const snapshot =
          mithrilBootstrap.snapshots.find(
            (candidate: MithrilSnapshotItem) => candidate.digest === digest
          ) ||
          mithrilBootstrap.snapshot ||
          runtimeWindow.__mithrilBootstrapE2E.snapshot;

        return mithrilBootstrap._updateStatus({
          status: 'preparing',
          snapshot,
          error: null,
          filesDownloaded: 0,
          filesTotal: 12,
          elapsedSeconds: 5,
          progressItems: [],
        });
      };

      mithrilBootstrap.cancelBootstrap = async () =>
        mithrilBootstrap._updateStatus({
          status: 'cancelled',
          snapshot: mithrilBootstrap.snapshot,
          error: null,
          progressItems: [],
        });

      mithrilBootstrap.validateChainStorageDirectory = async (
        nextPath: string
      ) => {
        const runtime = runtimeWindow.__mithrilBootstrapE2E;
        return (
          runtime.validationByPath[nextPath] || {
            isValid: true,
            path: nextPath,
            resolvedPath: nextPath,
            availableSpaceBytes: runtime.defaultValidation.availableSpaceBytes,
            requiredSpaceBytes: runtime.defaultValidation.requiredSpaceBytes,
          }
        );
      };

      mithrilBootstrap.setChainStorageDirectory = async (
        nextPath: string | null
      ) => {
        const runtime = runtimeWindow.__mithrilBootstrapE2E;
        const validation =
          nextPath == null
            ? runtime.defaultValidation
            : runtime.validationByPath[nextPath] || {
                isValid: true,
                path: nextPath,
                resolvedPath: nextPath,
                availableSpaceBytes:
                  runtime.defaultValidation.availableSpaceBytes,
                requiredSpaceBytes:
                  runtime.defaultValidation.requiredSpaceBytes,
              };

        if (validation.isValid) {
          runInAction(() => {
            mithrilBootstrap.customChainPath = validation.path ?? null;
            mithrilBootstrap.chainStorageValidation = validation;
            if (validation.path == null) {
              mithrilBootstrap.defaultChainStorageValidation =
                runtime.defaultValidation;
            }
          });
        }

        return validation;
      };

      mithrilBootstrap.resetChainStorageDirectory = async () =>
        mithrilBootstrap.setChainStorageDirectory(null);

      if (global.ipcRenderer?.removeAllListeners) {
        global.ipcRenderer.removeAllListeners(
          channels.MITHRIL_STATUS_BROADCAST
        );
      }

      runInAction(() => {
        networkStatus.isNodeResponding = false;
        networkStatus.isNodeSyncing = false;
        networkStatus.isNodeInSync = false;
        networkStatus.isNotEnoughDiskSpace = false;

        mithrilBootstrap.status = seed.status;
        mithrilBootstrap.snapshot = seed.snapshot;
        mithrilBootstrap.filesDownloaded = seed.filesDownloaded;
        mithrilBootstrap.filesTotal = seed.filesTotal;
        mithrilBootstrap.elapsedSeconds = seed.elapsedSeconds;
        mithrilBootstrap.error =
          seed.view === 'error'
            ? {
                stage: seed.errorStage,
                code: `mithril-${seed.errorStage}`,
                message: `Seeded ${seed.errorStage} failure`,
                logPath: '/tmp/mithril-bootstrap.log',
              }
            : null;
        mithrilBootstrap.snapshots = [seed.snapshot];
        mithrilBootstrap.isFetchingSnapshots = false;
        mithrilBootstrap.customChainPath = seed.customChainPath;
        mithrilBootstrap.defaultChainPath = seed.defaultChainPath;
        mithrilBootstrap.defaultChainStorageValidation = defaultValidation;
        mithrilBootstrap.chainStorageValidation = currentValidation;
        mithrilBootstrap.isChainStorageLoading = seed.isChainStorageLoading;
        mithrilBootstrap.storageLocationConfirmed =
          seed.storageLocationConfirmed;
        mithrilBootstrap.ancillaryBytesDownloaded = undefined;
        mithrilBootstrap.ancillaryBytesTotal = undefined;
        mithrilBootstrap.progressItems = seed.progressItems;
        mithrilBootstrap.bootstrapStartedAt =
          seed.view === 'progress' ? Date.now() - 5000 : null;
      });
    },
    seedOptions,
    IPC_CHANNELS
  );
};

const cleanupMithrilBootstrap = async function () {
  await this.client.executeAsync((channels, done) => {
    const { runInAction } = require('mobx');
    const runtimeWindow = window as any;
    const runtime = runtimeWindow.__mithrilBootstrapE2E;

    if (!runtime) {
      done();
      return;
    }

    const networkStatus = daedalus.stores.networkStatus;
    const mithrilBootstrap = daedalus.stores.mithrilBootstrap;
    const { originals } = runtime;

    if (originals.polling.status) {
      networkStatus._setNetworkStatusPollingInterval();
    } else {
      networkStatus._clearNetworkStatusPollingInterval();
    }

    if (originals.polling.clock) {
      networkStatus._setNetworkClockPollingInterval();
    } else {
      networkStatus._clearNetworkClockPollingInterval();
    }

    if (originals.polling.parameters) {
      networkStatus._setNetworkParametersPollingInterval();
    } else {
      networkStatus._clearNetworkParametersPollingInterval();
    }

    mithrilBootstrap.syncStatus = originals.methods.syncStatus;
    mithrilBootstrap.loadSnapshots = originals.methods.loadSnapshots;
    mithrilBootstrap.loadChainStorageConfig =
      originals.methods.loadChainStorageConfig;
    mithrilBootstrap.setDecision = originals.methods.setDecision;
    mithrilBootstrap.startBootstrap = originals.methods.startBootstrap;
    mithrilBootstrap.cancelBootstrap = originals.methods.cancelBootstrap;
    mithrilBootstrap.setChainStorageDirectory =
      originals.methods.setChainStorageDirectory;
    mithrilBootstrap.resetChainStorageDirectory =
      originals.methods.resetChainStorageDirectory;
    mithrilBootstrap.validateChainStorageDirectory =
      originals.methods.validateChainStorageDirectory;

    if (global.ipcRenderer?.removeAllListeners) {
      global.ipcRenderer.removeAllListeners(channels.MITHRIL_STATUS_BROADCAST);
    }

    if (global.ipcRenderer?.on) {
      global.ipcRenderer.on(
        channels.MITHRIL_STATUS_BROADCAST,
        async (event: any, message: MithrilBootstrapStatusUpdate) => {
          try {
            const response = await mithrilBootstrap._updateStatus(message);
            event.sender.send(channels.MITHRIL_STATUS_RESPONSE, true, response);
          } catch (error) {
            event.sender.send(channels.MITHRIL_STATUS_RESPONSE, false, error);
          }
        }
      );
    }

    runInAction(() => {
      networkStatus.isNodeResponding = originals.networkStatus.isNodeResponding;
      networkStatus.isNodeSyncing = originals.networkStatus.isNodeSyncing;
      networkStatus.isNodeInSync = originals.networkStatus.isNodeInSync;
      networkStatus.isNodeTimeCorrect =
        originals.networkStatus.isNodeTimeCorrect;
      networkStatus.isSystemTimeIgnored =
        originals.networkStatus.isSystemTimeIgnored;
      networkStatus.isNotEnoughDiskSpace =
        originals.networkStatus.isNotEnoughDiskSpace;

      mithrilBootstrap.status = originals.mithrilBootstrap.status;
      mithrilBootstrap.snapshot = originals.mithrilBootstrap.snapshot;
      mithrilBootstrap.filesDownloaded =
        originals.mithrilBootstrap.filesDownloaded;
      mithrilBootstrap.filesTotal = originals.mithrilBootstrap.filesTotal;
      mithrilBootstrap.elapsedSeconds =
        originals.mithrilBootstrap.elapsedSeconds;
      mithrilBootstrap.error = originals.mithrilBootstrap.error;
      mithrilBootstrap.snapshots = originals.mithrilBootstrap.snapshots;
      mithrilBootstrap.isFetchingSnapshots =
        originals.mithrilBootstrap.isFetchingSnapshots;
      mithrilBootstrap.customChainPath =
        originals.mithrilBootstrap.customChainPath;
      mithrilBootstrap.defaultChainPath =
        originals.mithrilBootstrap.defaultChainPath;
      mithrilBootstrap.defaultChainStorageValidation =
        originals.mithrilBootstrap.defaultChainStorageValidation;
      mithrilBootstrap.chainStorageValidation =
        originals.mithrilBootstrap.chainStorageValidation;
      mithrilBootstrap.isChainStorageLoading =
        originals.mithrilBootstrap.isChainStorageLoading;
      mithrilBootstrap.storageLocationConfirmed =
        originals.mithrilBootstrap.storageLocationConfirmed;
      mithrilBootstrap.ancillaryBytesDownloaded =
        originals.mithrilBootstrap.ancillaryBytesDownloaded;
      mithrilBootstrap.ancillaryBytesTotal =
        originals.mithrilBootstrap.ancillaryBytesTotal;
      mithrilBootstrap.progressItems = originals.mithrilBootstrap.progressItems;
      mithrilBootstrap.bootstrapStartedAt =
        originals.mithrilBootstrap.bootstrapStartedAt;
    });

    delete runtimeWindow.__mithrilBootstrapE2E;

    const waitUntilSynced = () => {
      if (daedalus.stores.networkStatus.isSynced) {
        done();
        return;
      }

      setTimeout(waitUntilSynced, 50);
    };

    waitUntilSynced();
  }, IPC_CHANNELS);
};

const advanceMithrilStatus = async function (status: MithrilBootstrapStatus) {
  const update: MithrilBootstrapStatusUpdate = {
    status,
    snapshot: BASE_SNAPSHOT,
    error: null,
    filesDownloaded: status === 'downloading' ? 4 : 12,
    filesTotal: 12,
    elapsedSeconds: status === 'downloading' ? 10 : 15,
    progressItems: createProgressItems(status),
  };

  await this.client.executeAsync(
    (nextUpdate: MithrilBootstrapStatusUpdate, done) => {
      daedalus.stores.mithrilBootstrap
        ._updateStatus(nextUpdate)
        .then(() => done())
        .catch((error) => done(error));
    },
    update
  );
};

const waitForStepToBeActive = async function (
  step: 'preparing' | 'downloading' | 'finalizing'
) {
  const label = await this.intl(STEP_LABEL_IDS[step]);
  const selector = `//*[contains(@class, "MithrilStepIndicator_stepActive")]//*[contains(@class, "MithrilStepIndicator_label") and normalize-space()="${label}"]`;

  await this.client.waitForVisible(selector);
};

After(
  {
    tags: '@mithrilBootstrap',
    timeout: DEFAULT_TIMEOUT,
  },
  async function () {
    await cleanupMithrilBootstrap.call(this);
  }
);

Given(/^Mithril bootstrap is seeded on the decision view$/, async function () {
  await seedMithrilBootstrap.call(this, createSeedOptions('decision'));
  await this.client.waitForVisible(SELECTORS.HEADING_DECISION);
});

Given(
  /^Mithril bootstrap is seeded on the chain storage view$/,
  async function () {
    await seedMithrilBootstrap.call(this, createSeedOptions('chain-storage'));
    await this.client.waitForVisible(SELECTORS.HEADING_CHAIN_STORAGE);
  }
);

Given(
  /^Mithril bootstrap is seeded on the chain storage view with custom storage "([^"]*)"$/,
  async function (customChainPath: string) {
    await seedMithrilBootstrap.call(
      this,
      createSeedOptions('chain-storage', {
        customChainPath,
      })
    );
    await this.client.waitForVisible(SELECTORS.HEADING_CHAIN_STORAGE);
  }
);

Given(
  /^Mithril bootstrap is seeded on the chain storage view with busy storage controls$/,
  async function () {
    await seedMithrilBootstrap.call(
      this,
      createSeedOptions('chain-storage', {
        customChainPath: CUSTOM_CHAIN_PATH,
        isChainStorageLoading: true,
      })
    );
    await this.client.waitForVisible(SELECTORS.HEADING_CHAIN_STORAGE);
  }
);

Given(
  /^Mithril bootstrap is seeded on the progress view for "([^"]*)"$/,
  async function (status: 'preparing' | 'downloading' | 'finalizing') {
    let filesDownloaded = 0;

    if (status === 'downloading') {
      filesDownloaded = 4;
    } else if (status === 'finalizing') {
      filesDownloaded = 12;
    }

    await seedMithrilBootstrap.call(
      this,
      createSeedOptions('progress', {
        status,
        progressItems: createProgressItems(status),
        filesDownloaded,
        filesTotal: 12,
        elapsedSeconds: 5,
      })
    );
    await this.client.waitForVisible(SELECTORS.HEADING_PROGRESS);
  }
);

Given(
  /^Mithril bootstrap is seeded on the "([^"]*)" error view$/,
  async function (stage: MithrilBootstrapErrorStage) {
    await seedMithrilBootstrap.call(
      this,
      createSeedOptions('error', {
        errorStage: stage,
      })
    );
    await this.client.waitForVisible(SELECTORS.HEADING_ERROR);
  }
);

Given(
  /^the Mithril chain storage validation fails for the selected path because of insufficient space$/,
  async function () {
    await this.client.execute((pathToInvalidate: string) => {
      const runtimeWindow = window as any;

      if (!runtimeWindow.__mithrilBootstrapE2E) {
        return;
      }

      runtimeWindow.__mithrilBootstrapE2E.validationByPath[pathToInvalidate] = {
        isValid: false,
        path: pathToInvalidate,
        resolvedPath: pathToInvalidate,
        availableSpaceBytes: 32000000,
        requiredSpaceBytes: 256000000,
        reason: 'insufficient-space',
        message: 'insufficient-space',
      };
    }, SELECTED_CHAIN_PATH);
  }
);

When(/^I accept Mithril bootstrap$/, async function () {
  await this.waitAndClick(SELECTORS.DECISION_ACCEPT_BUTTON);
});

When(/^I decline Mithril bootstrap$/, async function () {
  await this.waitAndClick(SELECTORS.DECISION_DECLINE_BUTTON);
});

When(/^I cancel Mithril bootstrap$/, async function () {
  await this.waitAndClick(SELECTORS.PROGRESS_CANCEL_BUTTON);
});

When(
  /^Mithril bootstrap advances to the "([^"]*)" stage$/,
  async function (status: 'downloading' | 'finalizing') {
    await advanceMithrilStatus.call(this, status);
  }
);

When(/^I choose a new Mithril chain storage location$/, async function () {
  await this.waitAndClick(SELECTORS.CHAIN_STORAGE_SELECT_BUTTON);
  await this.client.waitUntil(async () => {
    const { value } = await this.client.execute(
      (selectedPath: string, selectors) => {
        const input = document.querySelector(
          selectors.CHAIN_STORAGE_INPUT
        ) as HTMLInputElement | null;
        const hasValidationMessage =
          document.querySelector(selectors.CHAIN_STORAGE_VALIDATION_MESSAGE) !=
          null;

        return hasValidationMessage || input?.value === selectedPath;
      },
      SELECTED_CHAIN_PATH,
      SELECTORS
    );

    return value === true;
  }, DEFAULT_TIMEOUT);
});

When(/^I continue from the Mithril chain storage view$/, async function () {
  await this.waitAndClick(SELECTORS.CHAIN_STORAGE_CONTINUE_BUTTON);
});

When(
  /^I reset the Mithril chain storage location to default$/,
  async function () {
    await this.waitAndClick(SELECTORS.CHAIN_STORAGE_RESET_BUTTON);
    await this.client.waitUntil(async () => {
      const value = await this.client.getValue(SELECTORS.CHAIN_STORAGE_INPUT);
      return value === DEFAULT_CHAIN_PATH;
    }, DEFAULT_TIMEOUT);
  }
);

Then(/^I should see the Mithril progress view$/, async function () {
  await this.client.waitForVisible(SELECTORS.PROGRESS_ROOT);
  await this.client.waitForVisible(SELECTORS.HEADING_PROGRESS);
});

Then(/^I should see the Mithril chain storage view$/, async function () {
  await this.client.waitForVisible(SELECTORS.CHAIN_STORAGE_ROOT);
  await this.client.waitForVisible(SELECTORS.HEADING_CHAIN_STORAGE);
});

Then(/^the Mithril bootstrap overlay should be hidden$/, async function () {
  await this.client.waitForVisible(SELECTORS.OVERLAY, null, true);
});

Then(
  /^the "([^"]*)" Mithril step should be active$/,
  async function (step: 'preparing' | 'downloading' | 'finalizing') {
    await waitForStepToBeActive.call(this, step);
  }
);

Then(
  /^I should see the "([^"]*)" Mithril error heading$/,
  async function (stage: MithrilBootstrapErrorStage) {
    const expectedHeading = await this.intl(ERROR_TITLE_IDS[stage]);
    await this.client.waitForVisible(SELECTORS.ERROR_ROOT);
    const actualHeading = await this.waitAndGetText(SELECTORS.HEADING_ERROR);
    expect(actualHeading).to.equal(expectedHeading);
  }
);

Then(
  /^the Mithril decision view should show the selected chain storage path$/,
  async function () {
    await this.client.waitForVisible(SELECTORS.DECISION_ROOT);
    await this.client.waitForVisible(SELECTORS.HEADING_DECISION);
    const locationValue = await this.waitAndGetText(
      SELECTORS.DECISION_LOCATION_VALUE
    );
    expect(locationValue).to.equal(SELECTED_CHAIN_PATH);
  }
);

Then(
  /^the Mithril decision view should show the default chain storage path$/,
  async function () {
    await this.client.waitForVisible(SELECTORS.DECISION_ROOT);
    await this.client.waitForVisible(SELECTORS.HEADING_DECISION);
    const locationValue = await this.waitAndGetText(
      SELECTORS.DECISION_LOCATION_VALUE
    );
    expect(locationValue).to.equal(DEFAULT_CHAIN_PATH);
  }
);

Then(
  /^I should see the Mithril insufficient space validation message$/,
  async function () {
    const expectedMessage = await this.intl(INSUFFICIENT_SPACE_MESSAGE_ID);
    await this.client.waitForVisible(
      SELECTORS.CHAIN_STORAGE_VALIDATION_MESSAGE
    );
    const actualMessage = await this.waitAndGetText(
      SELECTORS.CHAIN_STORAGE_VALIDATION_MESSAGE
    );
    expect(actualMessage).to.equal(expectedMessage);
  }
);

Then(
  /^the Mithril chain storage select button should be disabled$/,
  async function () {
    await this.client.waitForVisible(SELECTORS.CHAIN_STORAGE_SELECT_BUTTON);
    const isEnabled = await this.client.isEnabled(
      SELECTORS.CHAIN_STORAGE_SELECT_BUTTON
    );
    expect(isEnabled).to.equal(false);
  }
);

Then(
  /^the Mithril chain storage continue button should be disabled$/,
  async function () {
    await this.client.waitForVisible(SELECTORS.CHAIN_STORAGE_CONTINUE_BUTTON);
    const isEnabled = await this.client.isEnabled(
      SELECTORS.CHAIN_STORAGE_CONTINUE_BUTTON
    );
    expect(isEnabled).to.equal(false);
  }
);

Then(
  /^the Mithril chain storage reset button should be disabled$/,
  async function () {
    await this.client.waitForVisible(SELECTORS.CHAIN_STORAGE_RESET_BUTTON);
    const isEnabled = await this.client.isEnabled(
      SELECTORS.CHAIN_STORAGE_RESET_BUTTON
    );
    expect(isEnabled).to.equal(false);
  }
);
