import os from 'os';
import { get, includes, uniq } from 'lodash';
import packageJson from '../../package.json';
import type { Environment } from '../common/types/environment.types';
import { DEVELOPMENT, OS_NAMES } from '../common/types/environment.types';
import {
  evaluateNetwork,
  checkIsAlonzoPurple,
  checkIsVasilDev,
  checkIsPreprod,
  checkIsPreview,
  checkIsShelleyQA,
  checkIsDev,
  checkIsTest,
  checkIsProduction,
  checkIsMainnet,
  checkIsStaging,
  checkIsTestnet,
  checkIsSelfnode,
  checkIsDevelopment,
  checkIsMacOS,
  checkIsWindows,
  checkIsLinux,
} from '../common/utils/environmentCheckers';

const { version } = packageJson;
// Daedalus requires minimum 16 gigabytes of RAM, but some devices having 16 GB
// actually have a slightly smaller RAM size (eg. 15.99 GB), therefore we used 15 GB threshold
//
// TODO figure out better place for it - can't import from config.js as it would be a circular dep
// https://input-output.atlassian.net/browse/DDW-928
export const RECOMMENDED_RAM_IN_BYTES = 15 * 1024 * 1024 * 1024;

/* ==================================================================
=                           Evaluations                             =
================================================================== */
// environment variables
const CURRENT_NODE_ENV = process.env.NODE_ENV || DEVELOPMENT;
const NETWORK = evaluateNetwork(process.env.NETWORK);
const isDev = checkIsDev(CURRENT_NODE_ENV);
const isTest = checkIsTest(CURRENT_NODE_ENV);
const isProduction = checkIsProduction(CURRENT_NODE_ENV);
const isMainnet = checkIsMainnet(NETWORK);
const isStaging = checkIsStaging(NETWORK);
const isTestnet = checkIsTestnet(NETWORK);
const isAlonzoPurple = checkIsAlonzoPurple(NETWORK);
const isVasilDev = checkIsVasilDev(NETWORK);
const isPreprod = checkIsPreprod(NETWORK);
const isPreview = checkIsPreview(NETWORK);
const isShelleyQA = checkIsShelleyQA(NETWORK);
const isSelfnode = checkIsSelfnode(NETWORK);
const isDevelopment = checkIsDevelopment(NETWORK);
const analyticsFeatureEnabled = true;
const keepLocalClusterRunning = process.env.KEEP_LOCAL_CLUSTER_RUNNING;
const API_VERSION = process.env.API_VERSION || 'dev';
const NODE_VERSION = '1.35.3'; // TODO: pick up this value from process.env

const mainProcessID = get(process, 'ppid', '-');
const rendererProcessID = process.pid;
const PLATFORM = os.platform();
const PLATFORM_VERSION = os.release();
const OS = OS_NAMES[PLATFORM] || PLATFORM;
const cpu = os.cpus();
const ram = os.totalmem();
const hasMetHardwareRequirements = ram >= RECOMMENDED_RAM_IN_BYTES;
const isBlankScreenFixActive = includes(process.argv.slice(1), '--safe-mode');
const BUILD = process.env.BUILD_NUMBER || 'dev';
const BUILD_NUMBER = uniq([API_VERSION, BUILD]).join('.');
const INSTALLER_VERSION = uniq([API_VERSION, BUILD]).join('.');
const isMacOS = checkIsMacOS(PLATFORM);
const isWindows = checkIsWindows(PLATFORM);
const isLinux = checkIsLinux(PLATFORM);

/* ==================================================================
=                       Compose environment                         =
================================================================== */
// @ts-ignore ts-migrate(2322) FIXME: Type '{ network: string; apiVersion: string; nodeV... Remove this comment to see the full error message
export const environment: Environment = Object.assign(
  {},
  {
    network: NETWORK,
    apiVersion: API_VERSION,
    nodeVersion: NODE_VERSION,
    current: CURRENT_NODE_ENV,
    isDev,
    isTest,
    isProduction,
    isMainnet,
    isStaging,
    isTestnet,
    isAlonzoPurple,
    isVasilDev,
    isPreprod,
    isPreview,
    isShelleyQA,
    isSelfnode,
    isDevelopment,
    build: BUILD,
    buildNumber: BUILD_NUMBER,
    platform: PLATFORM,
    platformVersion: PLATFORM_VERSION,
    mainProcessID,
    rendererProcessID,
    os: OS,
    cpu,
    ram,
    installerVersion: INSTALLER_VERSION,
    version,
    isWindows,
    isMacOS,
    isLinux,
    isBlankScreenFixActive,
    keepLocalClusterRunning,
    hasMetHardwareRequirements,
    analyticsFeatureEnabled,
  },
  process.env
);
