import React, { Component, Fragment } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import classNames from 'classnames';
import { observer } from 'mobx-react';
import { get, includes, upperFirst } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import CopyToClipboard from 'react-copy-to-clipboard';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import SVGInline from 'react-svg-inline';
import { ALLOWED_TIME_DIFFERENCE } from '../../config/timingConfig';
import globalMessages from '../../i18n/global-messages';
import DialogCloseButton from '../widgets/DialogCloseButton';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/close-cros... Remove this comment to see the full error message
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/clipboard-... Remove this comment to see the full error message
import iconCopy from '../../assets/images/clipboard-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/sand-clock... Remove this comment to see the full error message
import sandClockIcon from '../../assets/images/sand-clock-xs.inline.svg';
import LocalizableError from '../../i18n/LocalizableError';
import {
  formattedNumber,
  formattedCpuModel,
  formattedSize,
} from '../../utils/formatters';
import { CardanoNodeStates } from '../../../../common/types/cardano-node.types';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DaedalusDiagnostics.scss' or... Remove this comment to see the full error message
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
  daedalusBuildNumber: {
    id: 'daedalus.diagnostics.dialog.daedalusBuildNumber',
    defaultMessage: '!!!Daedalus build number',
    description: 'Daedalus build number',
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
  cardanoNodeVersion: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeVersion',
    defaultMessage: '!!!Cardano node version',
    description: 'Cardano node version',
  },
  cardanoNodePID: {
    id: 'daedalus.diagnostics.dialog.cardanoNodePID',
    defaultMessage: '!!!Cardano node process ID',
    description: 'Cardano node process ID',
  },
  cardanoNodeApiPort: {
    id: 'daedalus.diagnostics.dialog.cardanoNodeApiPort',
    defaultMessage: '!!!Cardano node port',
    description: 'Cardano node port',
  },
  cardanoWalletPID: {
    id: 'daedalus.diagnostics.dialog.cardanoWalletPID',
    defaultMessage: '!!!Cardano wallet process ID',
    description: 'Cardano wallet process ID',
  },
  cardanoWalletVersion: {
    id: 'daedalus.diagnostics.dialog.cardanoWalletVersion',
    defaultMessage: '!!!Cardano wallet version',
    description: 'Cardano wallet version',
  },
  cardanoWalletApiPort: {
    id: 'daedalus.diagnostics.dialog.cardanoWalletApiPort',
    defaultMessage: '!!!Cardano wallet port',
    description: 'Cardano wallet port',
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
    defaultMessage: '!!!Checking system time',
    description: 'Checking system time',
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
  systemInfo: SystemInfo;
  coreInfo: CoreSystemInfo;
  cardanoNodeState: CardanoNodeState | null | undefined;
  isNodeResponding: boolean;
  // isNodeSubscribed: boolean,
  isNodeSyncing: boolean;
  isNodeInSync: boolean;
  isNodeTimeCorrect: boolean;
  nodeConnectionError: LocalizableError | null | undefined;
  isConnected: boolean;
  isSynced: boolean;
  syncPercentage: number;
  localTimeDifference: number | null | undefined;
  isSystemTimeCorrect: boolean;
  isSystemTimeIgnored: boolean;
  isCheckingSystemTime: boolean;
  isForceCheckingSystemTime: boolean;
  localTip: TipInfo | null | undefined;
  networkTip: TipInfo | null | undefined;
  onOpenStateDirectory: (...args: Array<any>) => any;
  onOpenExternalLink: (...args: Array<any>) => any;
  onRestartNode: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  onCopyStateDirectoryPath: (...args: Array<any>) => any;
  onForceCheckNetworkClock: (...args: Array<any>) => any;
};
type State = {
  isNodeRestarting: boolean;
};
const FINAL_CARDANO_NODE_STATES = [
  CardanoNodeStates.RUNNING,
  CardanoNodeStates.UPDATED,
  CardanoNodeStates.CRASHED,
  CardanoNodeStates.ERRORED,
  CardanoNodeStates.UNRECOVERABLE,
];

@observer
class DaedalusDiagnostics extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: Props) {
    super(props);
    this.state = {
      isNodeRestarting: false,
    };
  }

  componentDidUpdate(prevProps: Props) {
    const { cardanoNodeState: prevCardanoNodeState } = prevProps;
    const { cardanoNodeState } = this.props;

    if (
      cardanoNodeState !== prevCardanoNodeState &&
      includes(FINAL_CARDANO_NODE_STATES, cardanoNodeState)
    ) {
      this.setState({
        isNodeRestarting: false,
      }); // eslint-disable-line
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
    const classNameRow = classNames([styles.layoutRow, messageId]);

    if (typeof value === 'boolean') {
      content = value
        ? intl.formatMessage(messages.statusOn)
        : intl.formatMessage(messages.statusOff);
      className =
        (value && messageId !== 'systemTimeIgnored') ||
        (!value && messageId === 'systemTimeIgnored')
          ? classNames([className, styles.green])
          : classNames([className, styles.red]);
    }

    return (
      <div className={classNameRow}>
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
      localTip,
      networkTip,
      onOpenStateDirectory,
      onClose,
      onCopyStateDirectoryPath,
      nodeConnectionError,
      onOpenExternalLink,
      isCheckingSystemTime,
      isForceCheckingSystemTime,
    } = this.props;
    const {
      platform,
      platformVersion,
      cpu: cpuInOriginalFormat,
      ram,
      availableDiskSpace: availableDiskSpaceInOriginalFormat,
    } = systemInfo;
    const cpu = formattedCpuModel(cpuInOriginalFormat);
    const availableDiskSpace = formattedSize(
      availableDiskSpaceInOriginalFormat
    );
    const {
      daedalusVersion,
      daedalusBuildNumber,
      daedalusProcessID,
      daedalusMainProcessID,
      isBlankScreenFixActive,
      cardanoNodeVersion,
      cardanoNodePID,
      cardanoWalletVersion,
      cardanoWalletPID,
      cardanoWalletApiPort,
      cardanoNetwork,
      daedalusStateDirectoryPath,
    } = coreInfo;
    const { isNodeRestarting } = this.state;
    const isNTPServiceReachable = localTimeDifference != null;
    const connectionError = get(nodeConnectionError, 'values', '{}');
    const { message, code } = connectionError;
    const unknownDiskSpaceSupportUrl = intl.formatMessage(
      messages.unknownDiskSpaceSupportUrl
    );
    const cardanoNetworkValue = intl.formatMessage(
      globalMessages[`network_${cardanoNetwork}`]
    );
    const localTimeDifferenceClasses = isCheckingSystemTime
      ? classNames([styles.layoutData, styles.localTimeDifference])
      : classNames([
          styles.layoutData,
          styles.localTimeDifference,
          !isNTPServiceReachable ||
          (localTimeDifference &&
            Math.abs(localTimeDifference) > ALLOWED_TIME_DIFFERENCE)
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
              {getRow('cpu', <PopOver content={cpu}>{cpu}</PopOver>)}
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
              {getRow('daedalusBuildNumber', daedalusBuildNumber)}
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
                      <PopOver
                        maxWidth={400}
                        content={
                          <div className={styles.tooltipLabelWrapper}>
                            <div>{daedalusStateDirectoryPath}</div>
                          </div>
                        }
                      >
                        <div className={styles.daedalusStateDirectoryPath}>
                          {daedalusStateDirectoryPath}
                        </div>
                        <SVGInline svg={iconCopy} />
                      </PopOver>
                    </div>
                  </CopyToClipboard>
                </Fragment>
              )}
              {getRow('cardanoNodeVersion', cardanoNodeVersion)}
              {getRow('cardanoNodePID', cardanoNodePID || '-')}
              {/* getRow('cardanoNodeApiPort', '-') */}
              {getRow('cardanoWalletVersion', cardanoWalletVersion)}
              {getRow('cardanoWalletPID', cardanoWalletPID || '-')}
              {getRow('cardanoWalletApiPort', cardanoWalletApiPort || '-')}
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
              {getRow('cardanoNetwork', cardanoNetworkValue)}
              {getRow('connected', isConnected)}
              {getRow('synced', isSynced)}
              {getRow(
                'syncPercentage',
                `${formattedNumber(syncPercentage, 2)}%`
              )}
              {getRow(
                'lastNetworkBlock',
                <Fragment>
                  <span>{intl.formatMessage(messages.epoch)}:</span>{' '}
                  {networkTip && networkTip.epoch ? (
                    formattedNumber(networkTip.epoch)
                  ) : (
                    <SVGInline
                      svg={sandClockIcon}
                      className={styles.networkTipSandClock}
                    />
                  )}
                  <span>{intl.formatMessage(messages.slot)}:</span>{' '}
                  {networkTip && networkTip.slot ? (
                    formattedNumber(networkTip.slot)
                  ) : (
                    <SVGInline
                      svg={sandClockIcon}
                      className={styles.networkTipSandClock}
                    />
                  )}
                </Fragment>
              )}
              {getRow(
                'lastSynchronizedBlock',
                <Fragment>
                  <span>{intl.formatMessage(messages.epoch)}:</span>{' '}
                  {localTip && localTip.epoch ? (
                    formattedNumber(localTip.epoch)
                  ) : (
                    <SVGInline
                      svg={sandClockIcon}
                      className={styles.networkTipSandClock}
                    />
                  )}
                  <span>{intl.formatMessage(messages.slot)}:</span>{' '}
                  {localTip && localTip.slot ? (
                    formattedNumber(localTip.slot)
                  ) : (
                    <SVGInline
                      svg={sandClockIcon}
                      className={styles.networkTipSandClock}
                    />
                  )}
                </Fragment>
              )}
              <div className={styles.layoutRow}>
                <div className={styles.layoutHeader}>
                  {intl.formatMessage(messages.localTimeDifference)}
                  {intl.formatMessage(globalMessages.punctuationColon)}
                </div>
                <div className={localTimeDifferenceClasses}>
                  {
                    <button
                      onClick={() => this.checkTime()}
                      disabled={isForceCheckingSystemTime || !isNodeResponding}
                    >
                      {isForceCheckingSystemTime
                        ? intl.formatMessage(
                            messages.localTimeDifferenceChecking
                          )
                        : intl.formatMessage(
                            messages.localTimeDifferenceCheckTime
                          )}
                    </button>
                  }
                  {isCheckingSystemTime ? (
                    <span className={localTimeDifferenceClasses}>
                      <SVGInline
                        svg={sandClockIcon}
                        className={styles.networkTipSandClock}
                      />
                    </span>
                  ) : (
                    <span className={localTimeDifferenceClasses}>
                      {isNTPServiceReachable
                        ? `${formattedNumber(localTimeDifference || 0)} μs`
                        : intl.formatMessage(messages.serviceUnreachable)}
                    </span>
                  )}
                </div>
              </div>
              {getRow('systemTimeCorrect', isSystemTimeCorrect)}
              {getRow('systemTimeIgnored', isSystemTimeIgnored)}
              {
                <div className={styles.layoutRow}>
                  <div className={styles.layoutHeader}>
                    {intl.formatMessage(messages.checkingNodeTime)}
                    {intl.formatMessage(globalMessages.punctuationColon)}
                  </div>
                  <div className={styles.layoutData}>
                    {isCheckingSystemTime
                      ? intl.formatMessage(messages.statusOn)
                      : intl.formatMessage(messages.statusOff)}
                  </div>
                </div>
              }
            </div>
            <div>
              {getSectionRow(
                'cardanoNodeStatus',
                <button
                  className={styles.cardanoNodeStatusBtn}
                  onClick={() => this.restartNode()}
                  disabled={
                    !includes(FINAL_CARDANO_NODE_STATES, cardanoNodeState)
                  }
                >
                  {isNodeRestarting
                    ? intl.formatMessage(messages.cardanoNodeStatusRestarting)
                    : intl.formatMessage(messages.cardanoNodeStatusRestart)}
                </button>
              )}
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
    // @ts-ignore ts-migrate(2339) FIXME: Property 'focus' does not exist on type 'Element'.
    document.getElementsByClassName('ReactModal__Content')[0].focus();
  };
  checkTime = () => {
    this.props.onForceCheckNetworkClock();
    this.restoreDialogCloseOnEscKey();
  };
  restartNode = () => {
    this.setState({
      isNodeRestarting: true,
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property 'trigger' does not exist on type '(...arg... Remove this comment to see the full error message
    this.props.onRestartNode.trigger();
    this.restoreDialogCloseOnEscKey();
  };
}

export default DaedalusDiagnostics;
