// @flow
import React, { Component, Fragment } from 'react';
import type { Node } from 'react';
import classNames from 'classnames';
import { observer } from 'mobx-react';
import { get, includes, upperFirst } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import CopyToClipboard from 'react-copy-to-clipboard';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import SVGInline from 'react-svg-inline';
import { BigNumber } from 'bignumber.js';
import { ALLOWED_TIME_DIFFERENCE } from '../../config/timingConfig';
import globalMessages from '../../i18n/global-messages';
import DialogCloseButton from '../widgets/DialogCloseButton';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import iconCopy from '../../assets/images/clipboard-ic.inline.svg';
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
    defaultMessage: '!!!Platform version',
    description: 'Platform version',
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
    defaultMessage: '!!!Daedalus version',
    description: 'Daedalus version',
  },
  daedalusMainProcessID: {
    id: 'daedalus.diagnostics.dialog.daedalusMainProcessID',
    defaultMessage: '!!!Daedalus main process ID',
    description: 'Daedalus main process ID',
  },
  daedalusProcessID: {
    id: 'daedalus.diagnostics.dialog.daedalusProcessID',
    defaultMessage: '!!!Daedalus renderer process ID',
    description: 'Daedalus renderer process ID',
  },
  blankScreenFix: {
    id: 'daedalus.diagnostics.dialog.blankScreenFix',
    defaultMessage: "!!!Daedalus 'Blank Screen Fix' active",
    description: "Daedalus 'Blank Screen Fix' active",
  },
  cardanoVersion: {
    id: 'daedalus.diagnostics.dialog.cardanoVersion',
    defaultMessage: '!!!Cardano node version',
    description: 'Cardano node version',
  },
  cardanoProcessID: {
    id: 'daedalus.diagnostics.dialog.cardanoProcessID',
    defaultMessage: '!!!Cardano node process ID',
    description: 'Cardano node process ID',
  },
  cardanoApiPort: {
    id: 'daedalus.diagnostics.dialog.cardanoApiPort',
    defaultMessage: '!!!Cardano node API port',
    description: 'Cardano node API port',
  },
  cardanoNetwork: {
    id: 'daedalus.diagnostics.dialog.cardanoNetwork',
    defaultMessage: '!!!Cardano network',
    description: 'Cardano network',
  },
  stateDirectoryPath: {
    id: 'daedalus.diagnostics.dialog.stateDirectory',
    defaultMessage: '!!!Daedalus state directory',
    description: 'Daedalus state directory',
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
    defaultMessage: '!!!Sync percentage',
    description: 'Sync percentage',
  },
  remainingUnsyncedBlocks: {
    id: 'daedalus.diagnostics.dialog.remainingUnsyncedBlocks',
    defaultMessage: '!!!Remaining unsynced blocks',
    description: 'Remaining unsynced blocks',
  },
  latestLocalBlockAge: {
    id: 'daedalus.diagnostics.dialog.latestLocalBlockAge',
    defaultMessage: '!!!Latest local block age',
    description: 'Latest local block age',
  },
  latestNetworkBlockAge: {
    id: 'daedalus.diagnostics.dialog.latestNetworkBlockAge',
    defaultMessage: '!!!Latest network block age',
    description: 'Latest network block age',
  },
  localTimeDifference: {
    id: 'daedalus.diagnostics.dialog.localTimeDifference',
    defaultMessage: '!!!Local time difference',
    description: 'Local time difference',
  },
  systemTimeCorrect: {
    id: 'daedalus.diagnostics.dialog.systemTimeCorrect',
    defaultMessage: '!!!System time correct',
    description: 'System time correct',
  },
  systemTimeIgnored: {
    id: 'daedalus.diagnostics.dialog.systemTimeIgnored',
    defaultMessage: '!!!System time ignored',
    description: 'System time ignored',
  },
  checkingNodeTime: {
    id: 'daedalus.diagnostics.dialog.checkingNodeTime',
    defaultMessage: '!!!Checking Cardano node time',
    description: 'Checking Cardano node time',
  },
  cardanoNodeStatus: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeStatus',
    defaultMessage: '!!!CARDANO NODE STATUS',
    description: 'CARDANO NODE STATUS',
  },
  cardanoNodeStatusRestarting: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeStatusRestarting',
    defaultMessage: '!!!Restarting Cardano node...',
    description: 'Restarting Cardano node...',
  },
  cardanoNodeStatusRestart: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeStatusRestart',
    defaultMessage: '!!!Restart Cardano node',
    description: 'Restart Cardano node',
  },
  cardanoNodeDiagnostics: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeDiagnostics',
    defaultMessage: '!!!Cardano node diagnostics',
    description: 'Cardano node diagnostics',
  },
  realtimeStatisticsMonitor: {
    id: 'daedalus.diagnostics.dialog.realtimeStatisticsMonitor',
    defaultMessage: '!!!Realtime statistics monitor',
    description: 'Realtime statistics monitor',
  },
  cardanoNodeState: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeState',
    defaultMessage: '!!!Cardano node state',
    description: 'Cardano node state',
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
    defaultMessage: '!!!Cardano node responding',
    description: 'Cardano node responding',
  },
  cardanoNodeSubscribed: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeSubscribed',
    defaultMessage: '!!!Cardano node subscribed',
    description: 'Cardano node subscribed',
  },
  cardanoNodeTimeCorrect: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeTimeCorrect',
    defaultMessage: '!!!Cardano node time correct',
    description: 'Cardano node time correct',
  },
  cardanoNodeSyncing: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeSyncing',
    defaultMessage: '!!!Cardano node syncing',
    description: 'Cardano node syncing',
  },
  cardanoNodeInSync: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeInSync',
    defaultMessage: '!!!Cardano node in sync',
    description: 'Cardano node in sync',
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
    defaultMessage: '!!!NTP service unreachable',
    description: 'NTP service unreachable',
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
  isNodeTimeCorrect: boolean,
  nodeConnectionError: ?LocalizableError,
  isConnected: boolean,
  isSynced: boolean,
  syncPercentage: number,
  localTimeDifference: ?number,
  isSystemTimeCorrect: boolean,
  isSystemTimeIgnored: boolean,
  // isForceCheckingNodeTime: boolean,
  // latestLocalBlockTimestamp: number,
  // latestNetworkBlockTimestamp: number,
  localTip: ?TipInfo,
  networkTip: ?TipInfo,
  // localBlockHeight: number,
  // networkBlockHeight: number,
  // onForceCheckLocalTimeDifference: Function,
  onOpenStateDirectory: Function,
  onOpenExternalLink: Function,
  onRestartNode: Function,
  onClose: Function,
  onCopyStateDirectoryPath: Function,
  isCheckingSystemTime: boolean,
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

  getSectionRow = (messageId: string, content?: Node) => {
    return (
      <div className={styles.layoutRow}>
        <div className={styles.sectionTitle}>
          <span>{this.context.intl.formatMessage(messages[messageId])}</span>
          {content}
          <hr />
        </div>
      </div>
    );
  };

  getRow = (messageId: string, value: Node | boolean) => {
    const { intl } = this.context;
    const key = intl.formatMessage(messages[messageId]);
    const colon = intl.formatMessage(globalMessages.punctuationColon);
    let content = value;
    let className = classNames([styles[messageId], styles.layoutData]);
    const classNameHeader = classNames([
      styles[messageId],
      styles.layoutHeader,
    ]);
    if (typeof value === 'boolean') {
      content = value
        ? intl.formatMessage(messages.statusOn)
        : intl.formatMessage(messages.statusOff);
      className =
        value || (!value && messageId === 'systemTimeIgnored')
          ? classNames([className, styles.green])
          : classNames([className, styles.red]);
    }
    return (
      <div className={styles.layoutRow}>
        <div className={classNameHeader}>
          {key}
          {colon}
        </div>
        <div className={className}>{content}</div>
      </div>
    );
  };

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
      isNodeTimeCorrect,
      isConnected,
      isSynced,
      syncPercentage,
      localTimeDifference,
      isSystemTimeCorrect,
      isSystemTimeIgnored,
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
      onOpenExternalLink,
      // isDev,
      // isTestnet,
      // isStaging,
      // isMainnet,
      isCheckingSystemTime,
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
      cardanoRawNetwork,
      cardanoNetwork,
      daedalusStateDirectoryPath,
    } = coreInfo;

    const { isNodeRestarting } = this.state;
    const isNTPServiceReachable = localTimeDifference != null;
    const connectionError = get(nodeConnectionError, 'values', '{}');
    const { message, code } = connectionError;

    // const remainingUnsyncedBlocks = networkBlockHeight - localBlockHeight;
    // const remainingUnsyncedBlocksClasses = classNames([
    //   remainingUnsyncedBlocks < 0 ||
    //   remainingUnsyncedBlocks > UNSYNCED_BLOCKS_ALLOWED
    //     ? styles.red
    //     : styles.green,
    //   styles.layoutData
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
    //   styles.layoutData
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
    //   styles.layoutData
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

    const unknownDiskSpaceSupportUrl = intl.formatMessage(
      messages.unknownDiskSpaceSupportUrl
    );

    let cardanoNetworkValue = intl.formatMessage(
      globalMessages[`network_${cardanoNetwork}`]
    );

    if (cardanoRawNetwork && cardanoNetwork !== cardanoRawNetwork) {
      const cardanoRawNetworkValue = intl.formatMessage(
        globalMessages[`network_${cardanoRawNetwork}`]
      );
      cardanoNetworkValue += ` (${cardanoRawNetworkValue})`;
    }

    const localTimeDifferenceClasses = isCheckingSystemTime
      ? styles.layoutData
      : classNames([
          styles.layoutData,
          !isNTPServiceReachable ||
          (localTimeDifference && localTimeDifference > ALLOWED_TIME_DIFFERENCE)
            ? styles.red
            : styles.green,
        ]);

    const { getSectionRow, getRow } = this;

    return (
      <div className={styles.component}>
        <DialogCloseButton
          className={styles.closeButton}
          icon={closeCrossThin}
          onClose={onClose}
        />

        <div className={styles.tables}>
          <div className={styles.table}>
            <div>
              {getSectionRow('cardanoNodeStatus')}
              {getRow('platform', platform)}
              {getRow('platformVersion', platformVersion)}
              {getRow(
                'cpu',
                <Tooltip skin={TooltipSkin} tip={cpu}>
                  {cpu}
                </Tooltip>
              )}
              {getRow('ram', ram)}
              {getRow(
                'availableDiskSpace',
                availableDiskSpace || (
                  <Link
                    onClick={() =>
                      onOpenExternalLink(unknownDiskSpaceSupportUrl)
                    }
                    label={intl.formatMessage(messages.unknownDiskSpace)}
                    skin={LinkSkin}
                  />
                )
              )}
            </div>
            <div>
              {getSectionRow('coreInfo')}
              {getRow('daedalusVersion', daedalusVersion)}
              {getRow('daedalusMainProcessID', daedalusMainProcessID)}
              {getRow('daedalusProcessID', daedalusProcessID)}
              {getRow(
                'blankScreenFix',
                isBlankScreenFixActive
                  ? intl.formatMessage(messages.statusOn)
                  : intl.formatMessage(messages.statusOff)
              )}
              {getRow(
                'stateDirectoryPath',
                <Fragment>
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
                    <div className={styles.stateDirectoryPath}>
                      <Tooltip
                        skin={TooltipSkin}
                        tip={
                          <div className={styles.tooltipLabelWrapper}>
                            <div>{daedalusStateDirectoryPath}</div>
                          </div>
                        }
                      >
                        <div className={styles.daedalusStateDirectoryPath}>
                          {daedalusStateDirectoryPath}
                        </div>
                        <SVGInline svg={iconCopy} />
                      </Tooltip>
                    </div>
                  </CopyToClipboard>
                </Fragment>
              )}
              {getRow('cardanoVersion', cardanoVersion)}
              {getRow('cardanoProcessID', cardanoProcessID)}
              {getRow('cardanoApiPort', cardanoAPIPort || '-')}
              {getRow('cardanoNetwork', cardanoNetworkValue)}
            </div>
            {isConnected && nodeConnectionError ? (
              <div>
                {getSectionRow('connectionError')}
                <div className={styles.layoutRow}>
                  <div className={styles.layoutHeader}>
                    <div className={styles.error}>
                      {intl.formatMessage(messages.message)}: {message || '-'}
                      <br />
                      {intl.formatMessage(messages.code)}: {code || '-'}
                    </div>
                  </div>
                </div>
              </div>
            ) : null}
          </div>

          <div className={styles.table}>
            <div>
              {getSectionRow('daedalusStatus')}
              {getRow('connected', isConnected)}
              {getRow('synced', isSynced)}
              {getRow(
                'syncPercentage',
                `${new BigNumber(
                  parseFloat(syncPercentage).toFixed(2)
                ).toFormat(2)}%`
              )}
              {getRow(
                'lastNetworkBlock',
                <Fragment>
                  <span>{intl.formatMessage(messages.epoch)}:</span>{' '}
                  {get(networkTip, 'epoch', '-')}
                  <span>{intl.formatMessage(messages.slot)}:</span>{' '}
                  {get(networkTip, 'slot', '-')}
                </Fragment>
              )}
              {getRow(
                'lastSynchronizedBlock',
                <Fragment>
                  <span>{intl.formatMessage(messages.epoch)}:</span>{' '}
                  {get(localTip, 'epoch', '-')}
                  <span>{intl.formatMessage(messages.slot)}:</span>{' '}
                  {get(localTip, 'slot', '-')}
                </Fragment>
              )}
              {/*
                <div className={styles.layoutRow}>
                  <div className={styles.layoutHeader}>{intl.formatMessage(messages.remainingUnsyncedBlocks)}:</div>
                  <div className={remainingUnsyncedBlocksClasses}>
                    {remainingUnsyncedBlocks >= 0 ? remainingUnsyncedBlocks : '-'}
                  </div>
                </div>
                <div className={styles.layoutRow}>
                  <div className={styles.layoutHeader}>{intl.formatMessage(messages.latestLocalBlockAge)}:</div>
                  <div className={latestLocalBlockAgeClasses}>
                    {latestLocalBlockTimestamp > 0
                      ? `${latestLocalBlockAge} ms`
                      : '-'}
                  </div>
                </div>
                <div className={styles.layoutRow}>
                  <div className={styles.layoutHeader}>{intl.formatMessage(messages.latestNetworkBlockAge)}:</div>
                  <div className={latestNetworkBlockAgeClasses}>
                    {latestNetworkBlockTimestamp > 0
                      ? `${latestNetworkBlockAge} ms`
                      : '-'}
                  </div>
                </div>
              */}
              <div className={styles.layoutRow}>
                <div className={styles.layoutHeader}>
                  {intl.formatMessage(messages.localTimeDifference)}:
                </div>
                <div className={localTimeDifferenceClasses}>
                  {/*
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
                  */}
                  {isCheckingSystemTime ? (
                    <span className={localTimeDifferenceClasses}>-</span>
                  ) : (
                    <span className={localTimeDifferenceClasses}>
                      {isNTPServiceReachable
                        ? `${localTimeDifference || 0} μs`
                        : intl.formatMessage(messages.serviceUnreachable)}
                    </span>
                  )}
                </div>
              </div>
              {getRow('systemTimeCorrect', isSystemTimeCorrect)}
              {getRow('systemTimeIgnored', isSystemTimeIgnored)}
              {/*
                <div className={styles.layoutRow}>
                  <div className={styles.layoutHeader}>{intl.formatMessage(messages.checkingNodeTime)}:</div>
                  <div className={styles.layoutData}>
                    {isForceCheckingNodeTime
                      ? intl.formatMessage(messages.statusOn)
                      : intl.formatMessage(messages.statusOff)}
                  </div>
                </div>
              */}
            </div>
            <div>
              {getSectionRow(
                'cardanoNodeStatus',
                <button
                  className={styles.cardanoNodeStatusBtn}
                  onClick={() => this.restartNode()}
                  disabled={isNodeRestarting}
                >
                  {isNodeRestarting
                    ? intl.formatMessage(messages.cardanoNodeStatusRestarting)
                    : intl.formatMessage(messages.cardanoNodeStatusRestart)}
                </button>
              )}
              {/*
                {showCardanoNodeEkgLink ? (
                  <div className={styles.layoutRow}>
                    <div className={styles.layoutHeader}>
                      {intl.formatMessage(messages.cardanoNodeDiagnostics)}:
                    </div>
                    <div className={styles.layoutData}>
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
                    </div>
                  </div>
                ) : null}
              */}
              {getRow(
                'cardanoNodeState',
                upperFirst(
                  cardanoNodeState != null
                    ? intl.formatMessage(
                        this.getLocalisationForCardanoNodeState()
                      )
                    : 'unknown'
                )
              )}
              {getRow('cardanoNodeResponding', isNodeResponding)}
              {/* getRow('cardanoNodeSubscribed', isNodeSubscribed) */}
              {getRow('cardanoNodeTimeCorrect', isNodeTimeCorrect)}
              {getRow('cardanoNodeSyncing', isNodeSyncing)}
              {getRow('cardanoNodeInSync', isNodeInSync)}
            </div>
          </div>
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
    // this.props.onForceCheckLocalTimeDifference();
    this.restoreDialogCloseOnEscKey();
  };

  restartNode = () => {
    this.setState({ isNodeRestarting: true });
    this.props.onRestartNode.trigger();
    this.restoreDialogCloseOnEscKey();
  };
}
