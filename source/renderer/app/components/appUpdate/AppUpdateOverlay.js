// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { get } from 'lodash';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { ButtonSpinnerSkin } from 'react-polymorph/lib/skins/simple/ButtonSpinnerSkin';
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
  buttonLaunchInstallerLabel: {
    id: 'appUpdate.overlay.button.launchInstaller.label',
    defaultMessage: '!!!Quit Daedalus and start the installation',
    description: '"buttonLaunchInstallerLabel" for the App Update Overlay',
  },
  buttonInstallUpdateLabel: {
    id: 'appUpdate.overlay.button.installUpdate.label',
    defaultMessage: '!!!Install the update and restart Daedalus',
    description: '"buttonInstallUpdateLabel" for the App Update Overlay',
  },
  postponeInstallLinkLabel: {
    id: 'appUpdate.overlay.postponeInstall.link.label',
    defaultMessage: '!!!Postpone the update',
    description: '"manualUpdateLinkLabel" for the App Update Overlay',
  },
  installingUpdateLabel: {
    id: 'appUpdate.overlay.installingUpdate.link.label',
    defaultMessage: '!!!Installing update...',
    description: '"installingUpdateLabel" for the App Update Overlay',
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
  manualUpdateDescriptionError: {
    id: 'appUpdate.overlay.manualUpdate.description.error',
    defaultMessage:
      '!!!We were unable to launch the update installer automatically.',
    description: '"manualUpdateDescriptionError" for the App Update Overlay',
  },
  manualUpdateDescriptionErrorLinux: {
    id: 'appUpdate.overlay.manualUpdate.description.errorLinux',
    defaultMessage: '!!!We were unable to install the update.',
    description:
      '"manualUpdateDescriptionErrorLinux" for the App Update Overlay',
  },
  manualUpdateDescriptionAction: {
    id: 'appUpdate.overlay.manualUpdate.description.action',
    defaultMessage: '!!!Please manually update Daedalus to its latest version.',
    description: '"manualUpdateDescriptionAction" for the App Update Overlay',
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
  isWaitingToQuitDaedalus: boolean,
  onInstallUpdate: Function,
  onExternalLinkClick: Function,
  onPostponeUpdate: Function,
  instalationProgress: number,
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

  contentClickHandler(event: SyntheticMouseEvent<HTMLElement>) {
    const linkUrl = get(event, ['target', 'href']);
    if (linkUrl) {
      event.preventDefault();
      event.stopPropagation();
      this.props.onExternalLinkClick(linkUrl);
    }
  }

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
      onPostponeUpdate,
      isWaitingToQuitDaedalus,
      isLinux,
      instalationProgress,
    } = this.props;
    const { areTermsOfUseAccepted } = this.state;
    const isCheckboxDisabled = isWaitingToQuitDaedalus;
    const checkboxStyles = classnames([
      styles.checkbox,
      isCheckboxDisabled ? styles.disabled : null,
    ]);
    const isButtonDisabled = !areTermsOfUseAccepted || isWaitingToQuitDaedalus;
    const buttonStyles = classnames([
      styles.button,
      isButtonDisabled ? styles.disabled : null,
      isWaitingToQuitDaedalus ? styles.installing : null,
    ]);
    const buttonLabel = isLinux
      ? messages.buttonInstallUpdateLabel
      : messages.buttonLaunchInstallerLabel;
    const postponeLinkStyles = classnames([
      styles.postponeLink,
      !isLinux && isWaitingToQuitDaedalus ? styles.disabled : null,
      isLinux && isWaitingToQuitDaedalus ? styles.noLink : null,
    ]);
    const postponeLabel =
      isLinux && isWaitingToQuitDaedalus
        ? messages.installingUpdateLabel
        : messages.postponeInstallLinkLabel;
    const postponeAction = !isWaitingToQuitDaedalus
      ? onPostponeUpdate
      : () => {};
    const actionsStyles = classnames([
      styles.actions,
      isLinux && isWaitingToQuitDaedalus ? styles.progressBar : null,
    ]);
    return (
      <div className={actionsStyles}>
        {!(isLinux && isWaitingToQuitDaedalus) && (
          <>
            <Checkbox
              label={intl.formatMessage(messages.checkboxLabel)}
              onChange={this.toggleAcceptance}
              className={checkboxStyles}
              checked={areTermsOfUseAccepted || isWaitingToQuitDaedalus}
              skin={CheckboxSkin}
              themeOverrides={styles.checkbox}
              disabled={isCheckboxDisabled}
            />
            <Button
              className={buttonStyles}
              onClick={onInstallUpdate}
              skin={ButtonSpinnerSkin}
              loading={isWaitingToQuitDaedalus}
              label={intl.formatMessage(buttonLabel)}
              disabled={isButtonDisabled}
            />
            <Link
              className={postponeLinkStyles}
              onClick={postponeAction}
              label={intl.formatMessage(postponeLabel)}
              skin={LinkSkin}
              hasIconAfter={false}
            />
          </>
        )}
        {isLinux && isWaitingToQuitDaedalus && (
          <>
            <div className={styles.downloadProgressContent}>
              <p className={styles.downloadProgressLabel}>
                {intl.formatMessage(messages.installingUpdateLabel)}
              </p>
            </div>
            <ProgressBarLarge progress={instalationProgress} />
          </>
        )}
      </div>
    );
  };

  manualUpdateAction = () => {
    const { intl } = this.context;
    const { onExternalLinkClick, onPostponeUpdate, isLinux } = this.props;
    const errorMessage = isLinux
      ? messages.manualUpdateDescriptionErrorLinux
      : messages.manualUpdateDescriptionError;
    return (
      <div className={styles.actions}>
        <div className={styles.manualUpdateDescription}>
          {intl.formatMessage(errorMessage)}
          {intl.formatMessage(messages.manualUpdateDescriptionAction)}
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
        <Link
          className={styles.postponeLink}
          onClick={onPostponeUpdate}
          label={intl.formatMessage(messages.postponeInstallLinkLabel)}
          skin={LinkSkin}
          hasIconAfter={false}
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

        <div
          className={styles.content}
          role="presentation"
          onClick={this.contentClickHandler.bind(this)}
        >
          <ReactMarkdown escapeHtml={false} source={content} />
        </div>
        {actions}
      </div>
    );
  }
}
