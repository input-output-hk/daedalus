// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import ReactMarkdown from 'react-markdown';
import News from '../../domains/News';
import styles from './AppUpdateOverlay.scss';
import DialogCloseButton from '../widgets/DialogCloseButton';
import ProgressBarLarge from '../widgets/ProgressBarLarge';
import externalLinkIcon from '../../assets/images/link-ic.inline.svg';

const messages = defineMessages({
  title: {
    id: 'appUpdate.overlay.title',
    defaultMessage: '!!!Software update available!',
    description: '"title" for the App Update Overlay',
  },
  subtitle: {
    id: 'appUpdate.overlay.subtitle',
    defaultMessage:
      '!!!You are currently running Daedalus version {currentAppVersion}.<br />Daedalus version {availableAppVersion} is now available to download.',
    description: '"subtitle" for the App Update Overlay',
  },
  checkboxLabel: {
    id: 'appUpdate.overlay.checkboxLabel',
    defaultMessage:
      '!!!I understand that I need to complete the installation before starting Daedalus.',
    description: '"checkboxLabel" for the App Update Overlay',
  },
  buttonLabelOpenInstaller: {
    id: 'appUpdate.overlay.buttonLabelOpenInstaller',
    defaultMessage: '!!!Quit Daedalus and start the installation',
    description: '"buttonLabelOpenInstaller" for the App Update Overlay',
  },
  buttonLabelOpenDirectory: {
    id: 'appUpdate.overlay.buttonLabelOpenDirectory',
    defaultMessage: '!!!Quit Daedalus and open installer',
    description: '"buttonLabelOpenDirectory" for the App Update Overlay',
  },
  downloadProgressLabel: {
    id: 'appUpdate.overlay.downloadProgressLabel',
    defaultMessage: '!!!Download in progress',
    description: '"downloadProgressLabel" for the App Update Overlay',
  },
  downloadTimeLeft: {
    id: 'appUpdate.overlay.downloadTimeLeft',
    defaultMessage: '!!!{downloadTimeLeft} left',
    description: '"downloadTimeLeft" for the App Update Overlay',
  },
  downloadProgressData: {
    id: 'appUpdate.overlay.downloadProgressData',
    defaultMessage: '!!!({totalDownloaded} of {totalDownloadSize} downloaded)',
    description: '"downloadProgressData" for the App Update Overlay',
  },
  manualUpdateDescription: {
    id: 'appUpdate.overlay.manualUpdate.description',
    defaultMessage:
      '!!!We were unable to launch the update installer automatically. Please manually update Daedalus to its latest version.',
    description: '"manualUpdateDescription" for the App Update Overlay',
  },
  manualUpdateLinkLabel: {
    id: 'appUpdate.overlay.manualUpdate.link.label',
    defaultMessage: '!!!Download and install it manually',
    description: '"manualUpdateLinkLabel" for the App Update Overlay',
  },
  manualUpdateButtonLabel: {
    id: 'appUpdate.overlay.manualUpdate.button.label',
    defaultMessage: '!!!Follow instructions and manually update',
    description: '"manualUpdateButtonLabel" for the App Update Overlay',
  },
  manualUpdateButtonUrl: {
    id: 'appUpdate.overlay.manualUpdate.button.url',
    defaultMessage: '!!!https://daedaluswallet.io/en/download/',
    description: '"manualUpdateButtonUrl" for the App Update Overlay',
  },
});

type Props = {
  update: News.News,
  onClose: Function,
  downloadTimeLeft: string,
  totalDownloaded: string,
  totalDownloadSize: string,
  availableAppVersion: string,
  currentAppVersion: string,
  downloadProgress: number,
  isUpdateDownloaded: boolean,
  isAutomaticUpdateFailed: boolean,
  displayManualUpdateLink: boolean,
  onInstallUpdate: Function,
  onExternalLinkClick: Function,
  isLinux: boolean,
};

type State = {
  areTermsOfUseAccepted: boolean,
};

@observer
export default class AppUpdateOverlay extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    areTermsOfUseAccepted: false,
  };

  toggleAcceptance = () => {
    this.setState(prevState => ({
      areTermsOfUseAccepted: !prevState.areTermsOfUseAccepted,
    }));
  };

  progressActions = () => {
    const { intl } = this.context;
    const {
      downloadTimeLeft,
      totalDownloaded,
      totalDownloadSize,
      downloadProgress,
    } = this.props;
    return (
      <div className={styles.downloadProgress}>
        <div className={styles.downloadProgressContent}>
          <p className={styles.downloadProgressLabel}>
            {intl.formatMessage(messages.downloadProgressLabel)}
          </p>
          <p className={styles.downloadProgressData}>
            <b>
              {intl.formatMessage(messages.downloadTimeLeft, {
                downloadTimeLeft,
              })}
            </b>{' '}
            {intl.formatMessage(messages.downloadProgressData, {
              totalDownloaded,
              totalDownloadSize,
            })}
          </p>
        </div>
        <ProgressBarLarge progress={downloadProgress} />
      </div>
    );
  };

  openInstallerAction = () => {
    const { intl } = this.context;
    const {
      onInstallUpdate,
      displayManualUpdateLink,
      onExternalLinkClick,
      isLinux,
    } = this.props;
    const { areTermsOfUseAccepted } = this.state;
    const buttonStyles = classnames([
      styles.button,
      !areTermsOfUseAccepted ? styles.disabled : null,
    ]);
    const buttonLabel = isLinux
      ? messages.buttonLabelOpenDirectory
      : messages.buttonLabelOpenInstaller;
    return (
      <div className={styles.actions}>
        <Checkbox
          label={intl.formatMessage(messages.checkboxLabel)}
          onChange={this.toggleAcceptance}
          className={styles.checkbox}
          checked={areTermsOfUseAccepted}
          skin={CheckboxSkin}
          themeOverrides={styles.checkbox}
        />
        <Button
          className={buttonStyles}
          onClick={onInstallUpdate}
          skin={ButtonSkin}
          label={intl.formatMessage(buttonLabel)}
          disabled={!areTermsOfUseAccepted}
        />
        {displayManualUpdateLink && (
          <Link
            className={styles.manualUpdateLink}
            onClick={() =>
              onExternalLinkClick(
                intl.formatMessage(messages.manualUpdateButtonUrl)
              )
            }
            label={intl.formatMessage(messages.manualUpdateLinkLabel)}
            skin={LinkSkin}
          />
        )}
      </div>
    );
  };

  manualUpdateAction = () => {
    const { intl } = this.context;
    const { onExternalLinkClick } = this.props;
    return (
      <div className={styles.actions}>
        <div className={styles.manualUpdateDescription}>
          {intl.formatMessage(messages.manualUpdateDescription)}
        </div>
        <Button
          className={styles.button}
          onClick={() =>
            onExternalLinkClick(
              intl.formatMessage(messages.manualUpdateButtonUrl)
            )
          }
          skin={ButtonSkin}
          label={
            <span>
              <SVGInline
                svg={externalLinkIcon}
                className={styles.externalLinkIcon}
              />
              {intl.formatMessage(messages.manualUpdateButtonLabel)}
            </span>
          }
        />
      </div>
    );
  };

  render() {
    const { intl } = this.context;
    const {
      update,
      onClose,
      isUpdateDownloaded,
      availableAppVersion,
      currentAppVersion,
      isAutomaticUpdateFailed,
    } = this.props;
    const { content } = update;
    let actions;
    if (isAutomaticUpdateFailed) actions = this.manualUpdateAction();
    else if (!isUpdateDownloaded) actions = this.progressActions();
    else actions = this.openInstallerAction();

    return (
      <div
        className={styles.component}
        role="presentation"
        onClick={!isUpdateDownloaded ? onClose : () => {}}
      >
        {!isUpdateDownloaded && !isAutomaticUpdateFailed && (
          <DialogCloseButton onClose={onClose} className={styles.closeButton} />
        )}
        <h1 className={styles.title}>{intl.formatMessage(messages.title)}</h1>
        <span className={styles.subtitle}>
          <FormattedHTMLMessage
            {...messages.subtitle}
            values={{
              availableAppVersion,
              currentAppVersion,
            }}
          />
        </span>
        <div className={styles.content}>
          <ReactMarkdown escapeHtml={false} source={content} />
        </div>
        {actions}
      </div>
    );
  }
}
