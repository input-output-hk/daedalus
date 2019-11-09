// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get, includes, upperFirst } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import CopyToClipboard from 'react-copy-to-clipboard';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import SVGInline from 'react-svg-inline';
// import {
//   ALLOWED_TIME_DIFFERENCE,
//   MAX_ALLOWED_STALL_DURATION,
// } from '../../config/timingConfig';
// import { UNSYNCED_BLOCKS_ALLOWED } from '../../config/numbersConfig';
// import { getNetworkEkgUrl } from '../../utils/network';
import DialogCloseButton from '../widgets/DialogCloseButton';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import iconCopy from '../../assets/images/clipboard-ic.inline.svg';
import externalLinkIcon from '../../assets/images/link-ic.inline.svg';
import LocalizableError from '../../i18n/LocalizableError';
import { CardanoNodeStates } from '../../../../common/types/cardano-node.types';
import styles from './DaedalusDiagnostics.scss';
import type { CardanoNodeState } from '../../../../common/types/cardano-node.types';
import type { SystemInfo } from '../../types/systemInfoTypes';
import type { CoreSystemInfo } from '../../types/coreSystemInfoTypes';
import type { TipInfo } from '../../api/network/types';

const messages = defineMessages({
  systemInfo: {
    id: 'daedalus.diagnostics.dialog.system.info',
    defaultMessage: '!!!SYSTEM INFO',
    description: 'System info',
  },
  platform: {
    id: 'daedalus.diagnostics.dialog.platform',
    defaultMessage: '!!!Platform',
    description: 'Platform',
  },
  platformVersion: {
    id: 'daedalus.diagnostics.dialog.platform.version',
    defaultMessage: '!!!Platform Version',
    description: 'Platform Version',
  },
  cpu: {
    id: 'daedalus.diagnostics.dialog.cpu',
    defaultMessage: '!!!CPU',
    description: 'CPU',
  },
  ram: {
    id: 'daedalus.diagnostics.dialog.ram',
    defaultMessage: '!!!RAM',
    description: 'RAM',
  },
  availableDiskSpace: {
    id: 'daedalus.diagnostics.dialog.availableDiskSpace',
    defaultMessage: '!!!Available disk space',
    description: 'Available disk space',
  },
  unknownDiskSpace: {
    id: 'daedalus.diagnostics.dialog.unknownDiskSpace',
    defaultMessage: '!!!Unknown',
    description: 'Unknown amount of disk space',
  },
  unknownDiskSpaceSupportUrl: {
    id: 'daedalus.diagnostics.dialog.unknownDiskSpaceSupportUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc',
    description: '"Support" link URL while disk space is unknown',
  },
  coreInfo: {
    id: 'daedalus.diagnostics.dialog.coreInfo',
    defaultMessage: '!!!CORE INFO',
    description: 'CORE INFO',
  },
  daedalusVersion: {
    id: 'daedalus.diagnostics.dialog.daedalusVersion',
    defaultMessage: '!!!Daedalus Version',
    description: 'Daedalus Version',
  },
  daedalusMainProcessID: {
    id: 'daedalus.diagnostics.dialog.daedalusMainProcessID',
    defaultMessage: '!!!Daedalus Main Process ID',
    description: 'Daedalus Main Process ID',
  },
  daedalusProcessID: {
    id: 'daedalus.diagnostics.dialog.daedalusProcessID',
    defaultMessage: '!!!Daedalus Renderer Process ID',
    description: 'Daedalus Renderer Process ID',
  },
  blankScreenFix: {
    id: 'daedalus.diagnostics.dialog.blankScreenFix',
    defaultMessage: "!!!'Blank Screen Fix' active",
    description: "'Blank Screen Fix' active",
  },
  cardanoVersion: {
    id: 'daedalus.diagnostics.dialog.cardanoVersion',
    defaultMessage: '!!!Cardano Version',
    description: 'Cardano Version',
  },
  cardanoProcessID: {
    id: 'daedalus.diagnostics.dialog.cardanoProcessID',
    defaultMessage: '!!!Cardano Process ID',
    description: 'Cardano Process ID',
  },
  cardanoApiPort: {
    id: 'daedalus.diagnostics.dialog.cardanoApiPort',
    defaultMessage: '!!!Cardano API Port',
    description: 'Cardano API Port',
  },
  cardanoNetwork: {
    id: 'daedalus.diagnostics.dialog.cardanoNetwork',
    defaultMessage: '!!!Cardano Network',
    description: 'Cardano Network',
  },
  stateDirectoryPath: {
    id: 'daedalus.diagnostics.dialog.stateDirectory',
    defaultMessage: '!!!Daedalus State Directory',
    description: 'Daedalus State Directory',
  },
  stateDirectoryPathOpenBtn: {
    id: 'daedalus.diagnostics.dialog.stateDirectoryPathOpenBtn',
    defaultMessage: '!!!Open',
    description: 'Open',
  },
  connectionError: {
    id: 'daedalus.diagnostics.dialog.connectionError',
    defaultMessage: '!!!CONNECTION ERROR',
    description: 'CONNECTION ERROR',
  },
  daedalusStatus: {
    id: 'daedalus.diagnostics.dialog.daedalusStatus',
    defaultMessage: '!!!DAEDALUS STATUS',
    description: 'DAEDALUS STATUS',
  },
  connected: {
    id: 'daedalus.diagnostics.dialog.connected',
    defaultMessage: '!!!Connected',
    description: 'Connected',
  },
  synced: {
    id: 'daedalus.diagnostics.dialog.synced',
    defaultMessage: '!!!Synced',
    description: 'Synced',
  },
  syncPercentage: {
    id: 'daedalus.diagnostics.dialog.syncPercentage',
    defaultMessage: '!!!Sync Percentage',
    description: 'Sync Percentage',
  },
  remainingUnsyncedBlocks: {
    id: 'daedalus.diagnostics.dialog.remainingUnsyncedBlocks',
    defaultMessage: '!!!Remaining Unsynced Blocks',
    description: 'Remaining Unsynced Blocks',
  },
  latestLocalBlockAge: {
    id: 'daedalus.diagnostics.dialog.latestLocalBlockAge',
    defaultMessage: '!!!Latest Local Block Age',
    description: 'Latest Local Block Age',
  },
  latestNetworkBlockAge: {
    id: 'daedalus.diagnostics.dialog.latestNetworkBlockAge',
    defaultMessage: '!!!Latest Network Block Age',
    description: 'Latest Network Block Age',
  },
  localTimeDifference: {
    id: 'daedalus.diagnostics.dialog.localTimeDifference',
    defaultMessage: '!!!Local Time Difference',
    description: 'Local Time Difference',
  },
  systemTimeCorrect: {
    id: 'daedalus.diagnostics.dialog.systemTimeCorrect',
    defaultMessage: '!!!System Time Correct',
    description: 'System Time Correct',
  },
  systemTimeIgnored: {
    id: 'daedalus.diagnostics.dialog.systemTimeIgnored',
    defaultMessage: '!!!System Time Ignored',
    description: 'System Time Ignored',
  },
  checkingNodeTime: {
    id: 'daedalus.diagnostics.dialog.checkingNodeTime',
    defaultMessage: '!!!Checking Node Time',
    description: 'Checking Node Time',
  },
  cardanoNodeStatus: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeStatus',
    defaultMessage: '!!!CARDANO NODE STATUS',
    description: 'CARDANO NODE STATUS',
  },
  cardanoNodeStatusRestarting: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeStatusRestarting',
    defaultMessage: '!!!Restarting Cardano Node...',
    description: 'Restarting Cardano Node...',
  },
  cardanoNodeStatusRestart: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeStatusRestart',
    defaultMessage: '!!!Restart Cardano Node',
    description: 'Restart Cardano Node',
  },
  cardanoNodeDiagnostics: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeDiagnostics',
    defaultMessage: '!!!Cardano Node Diagnostics',
    description: 'Cardano Node Diagnostics',
  },
  realtimeStatisticsMonitor: {
    id: 'daedalus.diagnostics.dialog.realtimeStatisticsMonitor',
    defaultMessage: '!!!Realtime statistics monitor',
    description: 'Realtime statistics monitor',
  },
  cardanoNodeState: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeState',
    defaultMessage: '!!!Cardano Node State',
    description: 'Cardano Node State',
  },
  nodeHasBeenUpdated: {
    id: 'daedalus.diagnostics.dialog.nodeHasBeenUpdated',
    defaultMessage: '!!!Updated',
    description: 'Updated',
  },
  nodeHasCrashed: {
    id: 'daedalus.diagnostics.dialog.nodeHasCrashed',
    defaultMessage: '!!!Crashed',
    description: 'Crashed',
  },
  nodeHasErrored: {
    id: 'daedalus.diagnostics.dialog.nodeHasErrored',
    defaultMessage: '!!!Errored',
    description: 'Errored',
  },
  nodeHasStopped: {
    id: 'daedalus.diagnostics.dialog.nodeHasStopped',
    defaultMessage: '!!!Stopped',
    description: 'Stopped',
  },
  nodeIsExiting: {
    id: 'daedalus.diagnostics.dialog.nodeIsExiting',
    defaultMessage: '!!!Exiting',
    description: 'Exiting',
  },
  nodeIsRunning: {
    id: 'daedalus.diagnostics.dialog.nodeIsRunning',
    defaultMessage: '!!!Running',
    description: 'Running',
  },
  nodeIsStarting: {
    id: 'daedalus.diagnostics.dialog.nodeIsStarting',
    defaultMessage: '!!!Starting',
    description: 'Starting',
  },
  nodeIsStopping: {
    id: 'daedalus.diagnostics.dialog.nodeIsStopping',
    defaultMessage: '!!!Stopping',
    description: 'Stopping',
  },
  nodeIsUnrecoverable: {
    id: 'daedalus.diagnostics.dialog.nodeIsUnrecoverable',
    defaultMessage: '!!!Unrecoverable',
    description: 'Unrecoverable',
  },
  nodeIsUpdating: {
    id: 'daedalus.diagnostics.dialog.nodeIsUpdating',
    defaultMessage: '!!!Updating',
    description: 'Updating',
  },
  cardanoNodeResponding: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeResponding',
    defaultMessage: '!!!Node Responding',
    description: 'Node Responding',
  },
  cardanoNodeSubscribed: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeSubscribed',
    defaultMessage: '!!!Node Subscribed',
    description: 'Node Subscribed',
  },
  cardanoNodeTimeCorrect: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeTimeCorrect',
    defaultMessage: '!!!Node Time Correct',
    description: 'Node Time Correct',
  },
  cardanoNodeSyncing: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeSyncing',
    defaultMessage: '!!!Node Syncing',
    description: 'Node Syncing',
  },
  cardanoNodeInSync: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeInSync',
    defaultMessage: '!!!Node In Sync',
    description: 'Node In Sync',
  },
  localTimeDifferenceChecking: {
    id: 'daedalus.diagnostics.dialog.localTimeDifferenceChecking',
    defaultMessage: '!!!Checking...',
    description: 'Checking...',
  },
  localTimeDifferenceCheckTime: {
    id: 'daedalus.diagnostics.dialog.localTimeDifferenceCheckTime',
    defaultMessage: '!!!Check time',
    description: 'Check time',
  },
  statusOn: {
    id: 'daedalus.diagnostics.dialog.statusOn',
    defaultMessage: '!!!YES',
    description: 'YES',
  },
  statusOff: {
    id: 'daedalus.diagnostics.dialog.statusOff',
    defaultMessage: '!!!NO',
    description: 'NO',
  },
  serviceUnreachable: {
    id: 'daedalus.diagnostics.dialog.serviceUnreachable',
    defaultMessage: '!!!NTP Service unreachable',
    description: 'NTP Service unreachable',
  },
  message: {
    id: 'daedalus.diagnostics.dialog.message',
    defaultMessage: '!!!message',
    description: 'message',
  },
  code: {
    id: 'daedalus.diagnostics.dialog.code',
    defaultMessage: '!!!code',
    description: 'code',
  },
  lastNetworkBlock: {
    id: 'daedalus.diagnostics.dialog.lastNetworkBlock',
    defaultMessage: '!!!Last network block',
    description: 'Last network block',
  },
  lastSynchronizedBlock: {
    id: 'daedalus.diagnostics.dialog.lastSynchronizedBlock',
    defaultMessage: '!!!Last synchronized block',
    description: 'Last synchronized block',
  },
  epoch: {
    id: 'daedalus.diagnostics.dialog.epoch',
    defaultMessage: '!!!epoch',
    description: 'epoch',
  },
  slot: {
    id: 'daedalus.diagnostics.dialog.slot',
    defaultMessage: '!!!slot',
    description: 'slot',
  },
});

type Props = {
  systemInfo: SystemInfo,
  coreInfo: CoreSystemInfo,
  cardanoNodeState: ?CardanoNodeState,
  // isDev: boolean,
  // isMainnet: boolean,
  // isStaging: boolean,
  // isTestnet: boolean,
  isNodeResponding: boolean,
  // isNodeSubscribed: boolean,
  isNodeSyncing: boolean,
  isNodeInSync: boolean,
  // isNodeTimeCorrect: boolean,
  nodeConnectionError: ?LocalizableError,
  isConnected: boolean,
  isSynced: boolean,
  syncPercentage: number,
  // localTimeDifference: ?number,
  // isSystemTimeIgnored: boolean,
  // isSystemTimeCorrect: boolean,
  // isForceCheckingNodeTime: boolean,
  // latestLocalBlockTimestamp: number,
  // latestNetworkBlockTimestamp: number,
  localTip: ?TipInfo,
  networkTip: ?TipInfo,
  // localBlockHeight: number,
  // networkBlockHeight: number,
  currentLocale: string,
  onForceCheckLocalTimeDifference: Function,
  onOpenStateDirectory: Function,
  onOpenExternalLink: Function,
  onRestartNode: Function,
  onClose: Function,
  onCopyStateDirectoryPath: Function,
};

type State = {
  isNodeRestarting: boolean,
};

@observer
export default class DaedalusDiagnostics extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: Props) {
    super(props);
    // let { localBlockHeight, networkBlockHeight } = props;
    // localBlockHeight = localBlockHeight || null;
    // networkBlockHeight = networkBlockHeight || null;
    this.state = {
      isNodeRestarting: false,
    };
  }

  componentWillReceiveProps(nextProps: Props) {
    const { cardanoNodeState } = this.props;
    const { cardanoNodeState: nextCardanoNodeState } = nextProps;
    const { isNodeRestarting } = this.state;
    const finalCardanoNodeStates = [
      CardanoNodeStates.RUNNING,
      CardanoNodeStates.STOPPED,
      CardanoNodeStates.UPDATED,
      CardanoNodeStates.CRASHED,
      CardanoNodeStates.ERRORED,
      CardanoNodeStates.UNRECOVERABLE,
    ];
    if (
      isNodeRestarting &&
      cardanoNodeState === CardanoNodeStates.STARTING &&
      includes(finalCardanoNodeStates, nextCardanoNodeState)
    ) {
      this.setState({ isNodeRestarting: false });
    }
  }

  render() {
    const { intl } = this.context;

    const {
      systemInfo,
      coreInfo,
      cardanoNodeState,
      isNodeResponding,
      // isNodeSubscribed,
      isNodeSyncing,
      isNodeInSync,
      // isNodeTimeCorrect,
      isConnected,
      isSynced,
      syncPercentage,
      // localTimeDifference,
      // isSystemTimeCorrect,
      // isForceCheckingNodeTime,
      localTip,
      networkTip,
      // localBlockHeight,
      // networkBlockHeight,
      // latestLocalBlockTimestamp,
      // latestNetworkBlockTimestamp,
      onOpenStateDirectory,
      onClose,
      onCopyStateDirectoryPath,
      nodeConnectionError,
      // isSystemTimeIgnored,
      onOpenExternalLink,
      // isDev,
      // isTestnet,
      // isStaging,
      // isMainnet,
      currentLocale,
    } = this.props;

    const {
      platform,
      platformVersion,
      cpu,
      ram,
      availableDiskSpace,
    } = systemInfo;

    const {
      daedalusVersion,
      daedalusProcessID,
      daedalusMainProcessID,
      isBlankScreenFixActive,
      cardanoVersion,
      cardanoProcessID,
      cardanoAPIPort,
      cardanoNetwork,
      daedalusStateDirectoryPath,
    } = coreInfo;

    const { isNodeRestarting } = this.state;
    // const isNTPServiceReachable = localTimeDifference != null;
    const connectionError = get(nodeConnectionError, 'values', '{}');
    const { message, code } = connectionError;

    // const localTimeDifferenceClasses = classNames([
    //   !isNTPServiceReachable ||
    //   (localTimeDifference && localTimeDifference > ALLOWED_TIME_DIFFERENCE)
    //     ? styles.red
    //     : styles.green,
    // ]);

    // const remainingUnsyncedBlocks = networkBlockHeight - localBlockHeight;
    // const remainingUnsyncedBlocksClasses = classNames([
    //   remainingUnsyncedBlocks < 0 ||
    //   remainingUnsyncedBlocks > UNSYNCED_BLOCKS_ALLOWED
    //     ? styles.red
    //     : styles.green,
    // ]);

    // const latestLocalBlockAge = moment(Date.now()).diff(
    //   moment(latestLocalBlockTimestamp)
    // );
    // const isLocalBlockHeightStalling =
    //   latestLocalBlockAge > MAX_ALLOWED_STALL_DURATION;
    // const latestLocalBlockAgeClasses = classNames([
    //   latestLocalBlockTimestamp > 0 && !isLocalBlockHeightStalling
    //     ? styles.green
    //     : styles.red,
    // ]);

    // const latestNetworkBlockAge = moment(Date.now()).diff(
    //   moment(latestNetworkBlockTimestamp)
    // );
    // const isNetworkBlockHeightStalling =
    //   latestNetworkBlockAge > MAX_ALLOWED_STALL_DURATION;
    // const latestNetworkBlockAgeClasses = classNames([
    //   latestNetworkBlockTimestamp > 0 && !isNetworkBlockHeightStalling
    //     ? styles.green
    //     : styles.red,
    // ]);

    // Cardano Node EKG server is not enabled for the Mainnet and Testnet builds!
    // const showCardanoNodeEkgLink =
    //   isMainnet || isTestnet
    //     ? false
    //     : getNetworkEkgUrl({
    //         isDev,
    //         isStaging,
    //         isTestnet,
    //       });

    const stateDirectoryPathStyles = classNames([
      styles.stateDirectoryPath,
      styles[`locale-${currentLocale}`],
    ]);

    const unknownDiskSpaceSupportUrl = intl.formatMessage(
      messages.unknownDiskSpaceSupportUrl
    );

    return (
      <div className={styles.component}>
        <DialogCloseButton
          className={styles.closeButton}
          icon={closeCrossThin}
          onClose={onClose}
        />

        <div className={styles.tables}>
          <table className={styles.table}>
            <tbody>
              <tr>
                <th className={styles.sectionTitle} colSpan={2}>
                  <span>{intl.formatMessage(messages.systemInfo)}</span>
                  <hr />
                </th>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.platform)}:</th>
                <td>{platform}</td>
              </tr>
              <tr className={styles.platformVersion}>
                <th>{intl.formatMessage(messages.platformVersion)}:</th>
                <td className={styles.platform}>{platformVersion}</td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.cpu)}:</th>
                <Tooltip skin={TooltipSkin} tip={cpu}>
                  <td>{cpu}</td>
                </Tooltip>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.ram)}:</th>
                <td>{ram}</td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.availableDiskSpace)}:</th>
                <td>
                  {availableDiskSpace || (
                    <button
                      className={styles.unknownDiskSpaceBtn}
                      onClick={() =>
                        onOpenExternalLink(unknownDiskSpaceSupportUrl)
                      }
                    >
                      {intl.formatMessage(messages.unknownDiskSpace)}
                      <SVGInline
                        svg={externalLinkIcon}
                        className={styles.externalLinkIcon}
                      />
                    </button>
                  )}
                </td>
              </tr>
            </tbody>
            <tbody>
              <tr>
                <th className={styles.sectionTitle} colSpan={2}>
                  <span>{intl.formatMessage(messages.coreInfo)}</span>
                  <hr />
                </th>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.daedalusVersion)}:</th>
                <td>{daedalusVersion}</td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.daedalusMainProcessID)}:</th>
                <td>{daedalusMainProcessID}</td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.daedalusProcessID)}:</th>
                <td>{daedalusProcessID}</td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.blankScreenFix)}:</th>
                <td className={styles.blankScreenFix}>
                  {isBlankScreenFixActive
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.cardanoVersion)}:</th>
                <td>{cardanoVersion}</td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.cardanoProcessID)}:</th>
                <td>{cardanoProcessID}</td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.cardanoApiPort)}:</th>
                <td>{cardanoAPIPort || '-'}</td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.cardanoNetwork)}:</th>
                <td>{cardanoNetwork}</td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.stateDirectoryPath)}:</th>
                <td className={styles.stateDirectory}>
                  <button
                    className={styles.stateDirectoryOpenBtn}
                    onClick={() =>
                      onOpenStateDirectory(daedalusStateDirectoryPath)
                    }
                  >
                    {intl.formatMessage(messages.stateDirectoryPathOpenBtn)}
                  </button>
                  <CopyToClipboard
                    text={daedalusStateDirectoryPath}
                    onCopy={onCopyStateDirectoryPath}
                  >
                    <div className={stateDirectoryPathStyles}>
                      <Tooltip
                        skin={TooltipSkin}
                        tip={
                          <div className={styles.tooltipLabelWrapper}>
                            <div>{daedalusStateDirectoryPath}</div>
                          </div>
                        }
                      >
                        <p>{daedalusStateDirectoryPath}</p>
                        <SVGInline svg={iconCopy} />
                      </Tooltip>
                    </div>
                  </CopyToClipboard>
                </td>
              </tr>
            </tbody>
            {isConnected && nodeConnectionError ? (
              <tbody>
                <tr>
                  <th className={styles.sectionTitle} colSpan={2}>
                    <span>{intl.formatMessage(messages.connectionError)}</span>
                    <hr />
                  </th>
                </tr>
                <tr>
                  <th>
                    <div className={styles.error}>
                      {intl.formatMessage(messages.message)}: {message || '-'}
                      <br />
                      {intl.formatMessage(messages.code)}: {code || '-'}
                    </div>
                  </th>
                </tr>
              </tbody>
            ) : null}
          </table>

          <table className={styles.table}>
            <tbody>
              <tr>
                <th className={styles.sectionTitle} colSpan={2}>
                  <span>{intl.formatMessage(messages.daedalusStatus)}</span>
                  <hr />
                </th>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.connected)}:</th>
                <td className={this.getClassName(isConnected)}>
                  {isConnected
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.synced)}:</th>
                <td className={this.getClassName(isSynced)}>
                  {isSynced
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.syncPercentage)}:</th>
                <td>{syncPercentage.toFixed(2)}%</td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.lastNetworkBlock)}:</th>
                <td className={styles.blockchainHeightInfo}>
                  <span>{intl.formatMessage(messages.epoch)}:</span>{' '}
                  {get(networkTip, 'epoch', '-')}
                  <span>{intl.formatMessage(messages.slot)}:</span>{' '}
                  {get(networkTip, 'slot', '-')}
                </td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.lastSynchronizedBlock)}:</th>
                <td className={styles.blockchainHeightInfo}>
                  <span>{intl.formatMessage(messages.epoch)}:</span>{' '}
                  {get(localTip, 'epoch', '-')}
                  <span>{intl.formatMessage(messages.slot)}:</span>{' '}
                  {get(localTip, 'slot', '-')}
                </td>
              </tr>
              {/*
                <tr>
                  <th>{intl.formatMessage(messages.remainingUnsyncedBlocks)}:</th>
                  <td className={remainingUnsyncedBlocksClasses}>
                    {remainingUnsyncedBlocks >= 0 ? remainingUnsyncedBlocks : '-'}
                  </td>
                </tr>
                <tr>
                  <th>{intl.formatMessage(messages.latestLocalBlockAge)}:</th>
                  <td className={latestLocalBlockAgeClasses}>
                    {latestLocalBlockTimestamp > 0
                      ? `${latestLocalBlockAge} ms`
                      : '-'}
                  </td>
                </tr>
                <tr>
                  <th>{intl.formatMessage(messages.latestNetworkBlockAge)}:</th>
                  <td className={latestNetworkBlockAgeClasses}>
                    {latestNetworkBlockTimestamp > 0
                      ? `${latestNetworkBlockAge} ms`
                      : '-'}
                  </td>
                </tr>
                <tr>
                  <th>{intl.formatMessage(messages.localTimeDifference)}:</th>
                  <td className={styles.localTimeDifferenceItem}>
                    <button
                      onClick={() => this.checkTime()}
                      disabled={isForceCheckingNodeTime || !isConnected}
                    >
                      {isForceCheckingNodeTime
                        ? intl.formatMessage(messages.localTimeDifferenceChecking)
                        : intl.formatMessage(
                            messages.localTimeDifferenceCheckTime
                          )}
                    </button>
                    <span className={localTimeDifferenceClasses}>
                      {isNTPServiceReachable
                        ? `${localTimeDifference || 0} μs`
                        : intl.formatMessage(messages.serviceUnreachable)}
                    </span>
                  </td>
                </tr>
                <tr>
                  <th>{intl.formatMessage(messages.systemTimeCorrect)}:</th>
                  <td className={this.getClassName(isSystemTimeCorrect)}>
                    {isSystemTimeCorrect
                      ? intl.formatMessage(messages.statusOn)
                      : intl.formatMessage(messages.statusOff)}
                  </td>
                </tr>
                <tr>
                  <th>{intl.formatMessage(messages.systemTimeIgnored)}:</th>
                  <td className={this.getClassName(!isSystemTimeIgnored)}>
                    {isSystemTimeIgnored
                      ? intl.formatMessage(messages.statusOn)
                      : intl.formatMessage(messages.statusOff)}
                  </td>
                </tr>
                <tr>
                  <th>{intl.formatMessage(messages.checkingNodeTime)}:</th>
                  <td>
                    {isForceCheckingNodeTime
                      ? intl.formatMessage(messages.statusOn)
                      : intl.formatMessage(messages.statusOff)}
                  </td>
                </tr>
              */}
            </tbody>
            <tbody>
              <tr>
                <th className={styles.sectionTitle} colSpan={2}>
                  <span>{intl.formatMessage(messages.cardanoNodeStatus)}</span>
                  <button
                    className={styles.statusBtn}
                    onClick={() => this.restartNode()}
                    disabled={isNodeRestarting}
                  >
                    {isNodeRestarting
                      ? intl.formatMessage(messages.cardanoNodeStatusRestarting)
                      : intl.formatMessage(messages.cardanoNodeStatusRestart)}
                  </button>
                  <hr />
                </th>
              </tr>
              {/*
                {showCardanoNodeEkgLink ? (
                  <tr>
                    <th>
                      {intl.formatMessage(messages.cardanoNodeDiagnostics)}:
                    </th>
                    <td>
                      <button
                        className={styles.realTimeStatusBtn}
                        onClick={() => onOpenExternalLink(showCardanoNodeEkgLink)}
                      >
                        {intl.formatMessage(messages.realtimeStatisticsMonitor)}
                        <SVGInline
                          svg={externalLinkIcon}
                          className={styles.externalLinkIcon}
                        />
                      </button>
                    </td>
                  </tr>
                ) : null}
              */}
              <tr>
                <th>{intl.formatMessage(messages.cardanoNodeState)}:</th>
                <td>
                  {upperFirst(
                    cardanoNodeState != null
                      ? intl.formatMessage(
                          this.getLocalisationForCardanoNodeState()
                        )
                      : 'unknown'
                  )}
                </td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.cardanoNodeResponding)}:</th>
                <td className={this.getClassName(isNodeResponding)}>
                  {isNodeResponding
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              {/*
                <tr>
                  <th>{intl.formatMessage(messages.cardanoNodeSubscribed)}:</th>
                  <td className={this.getClassName(isNodeSubscribed)}>
                    {isNodeSubscribed
                      ? intl.formatMessage(messages.statusOn)
                      : intl.formatMessage(messages.statusOff)}
                  </td>
                </tr>
                <tr>
                  <th>{intl.formatMessage(messages.cardanoNodeTimeCorrect)}:</th>
                  <td className={this.getClassName(isNodeTimeCorrect)}>
                    {isNodeTimeCorrect
                      ? intl.formatMessage(messages.statusOn)
                      : intl.formatMessage(messages.statusOff)}
                  </td>
                </tr>
              */}
              <tr>
                <th>{intl.formatMessage(messages.cardanoNodeSyncing)}:</th>
                <td className={this.getClassName(isNodeSyncing)}>
                  {isNodeSyncing
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
              <tr>
                <th>{intl.formatMessage(messages.cardanoNodeInSync)}:</th>
                <td className={this.getClassName(isNodeInSync)}>
                  {isNodeInSync
                    ? intl.formatMessage(messages.statusOn)
                    : intl.formatMessage(messages.statusOff)}
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    );
  }

  getLocalisationForCardanoNodeState = () => {
    const { cardanoNodeState } = this.props;
    let localisationKey;
    switch (cardanoNodeState) {
      case CardanoNodeStates.STARTING:
        localisationKey = messages.nodeIsStarting;
        break;
      case CardanoNodeStates.EXITING:
        localisationKey = messages.nodeIsExiting;
        break;
      case CardanoNodeStates.STOPPING:
        localisationKey = messages.nodeIsStopping;
        break;
      case CardanoNodeStates.STOPPED:
        localisationKey = messages.nodeHasStopped;
        break;
      case CardanoNodeStates.UPDATING:
        localisationKey = messages.nodeIsUpdating;
        break;
      case CardanoNodeStates.UPDATED:
        localisationKey = messages.nodeHasBeenUpdated;
        break;
      case CardanoNodeStates.CRASHED:
        localisationKey = messages.nodeHasCrashed;
        break;
      case CardanoNodeStates.ERRORED:
        localisationKey = messages.nodeHasErrored;
        break;
      case CardanoNodeStates.UNRECOVERABLE:
        localisationKey = messages.nodeIsUnrecoverable;
        break;
      default:
        localisationKey = messages.nodeIsRunning;
        break;
    }
    return localisationKey;
  };

  restoreDialogCloseOnEscKey = () => {
    // This method is to be used on buttons which get disabled after click
    // as without it the ReactModal is not closing if you press the ESC key
    // even after the button is later re-enabled
    document.getElementsByClassName('ReactModal__Content')[0].focus();
  };

  checkTime = () => {
    this.props.onForceCheckLocalTimeDifference();
    this.restoreDialogCloseOnEscKey();
  };

  restartNode = () => {
    this.setState({ isNodeRestarting: true });
    this.props.onRestartNode.trigger();
    this.restoreDialogCloseOnEscKey();
  };

  getClassName = (isTrue: boolean) =>
    classNames([isTrue ? styles.green : styles.red]);
}
