// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import ReactMarkdown from 'react-markdown';
import News from '../../domains/News';
import styles from './UpdateOverlay.scss';
import DialogCloseButton from '../widgets/DialogCloseButton';
import ProgressBarLarge from '../widgets/ProgressBarLarge';

const messages = defineMessages({
  title: {
    id: 'news.updateOverlay.title',
    defaultMessage: '!!!Software update available!',
    description: 'title for the Update Overlay',
  },
  subtitle: {
    id: 'news.updateOverlay.subtitle',
    defaultMessage:
      '!!!You are currently running Daedalus version {currentVersion}.<br />Daedalus version {availableVersion} is now available to download.',
    description: 'subtitle for the Update Overlay',
  },
  checkboxLabel: {
    id: 'news.updateOverlay.checkboxLabel',
    defaultMessage:
      '!!!I understand that I need to complete the installation before starting Daedalus.',
    description: 'checkboxLabel for the Update Overlay',
  },
  buttonLabel: {
    id: 'news.updateOverlay.buttonLabel',
    defaultMessage: '!!!Quit Daedalus and start the installation',
    description: 'buttonLabel for the Update Overlay',
  },
  downloadProgressLabel: {
    id: 'news.updateOverlay.downloadProgressLabel',
    defaultMessage: '!!!Download in progress',
    description: 'downloadProgressLabel for the Update Overlay',
  },
  // timeLeft: {
  //   id: 'news.updateOverlay.timeLeft',
  //   defaultMessage: '!!!',
  //   description: 'timeLeft for the Update Overlay',
  // },
  downloadProgressData: {
    id: 'news.updateOverlay.downloadProgressData',
    defaultMessage: '!!!({downloaded} MB of {total} MB downloaded)',
    description: 'downloadProgressData for the Update Overlay',
  },
});

type Props = {
  update: News.News,
  onCloseUpdate: Function,
  downloadProgress: DownloadData,
  isUpdateDownloaded: boolean,
  onInstallUpdate: Function,
};

type State = {
  areTermsOfUseAccepted: boolean,
};

@observer
export default class UpdateOverlay extends Component<Props, State> {
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

  render() {
    const { intl } = this.context;
    const {
      update,
      onCloseUpdate,
      downloadProgress,
      isUpdateDownloaded,
      onInstallUpdate,
    } = this.props;
    const { areTermsOfUseAccepted } = this.state;
    const { content } = update;
    const currentVersion = '1.1.0';
    const availableVersion = '21.1.0';
    return (
      <div
        className={styles.component}
        role="presentation"
        onClick={!isUpdateDownloaded ? onCloseUpdate : () => {}}
      >
        {!isUpdateDownloaded && (
          <DialogCloseButton
            onClose={onCloseUpdate}
            className={styles.closeButton}
          />
        )}
        <h1 className={styles.title}>{intl.formatMessage(messages.title)}</h1>
        <span className={styles.subtitle}>
          <FormattedHTMLMessage
            {...messages.subtitle}
            values={{
              currentVersion,
              availableVersion,
            }}
          />
        </span>
        <div className={styles.content}>
          <ReactMarkdown escapeHtml={false} source={content} />
        </div>
        {!isUpdateDownloaded ? (
          <div className={styles.downloadProgress}>
            <div className={styles.downloadProgressContent}>
              <p className={styles.downloadProgressLabel}>
                {intl.formatMessage(messages.downloadProgressLabel)}
              </p>
              <p className={styles.downloadProgressData}>
                <b>{this.timeLeft}</b>{' '}
                {intl.formatMessage(messages.downloadProgressData, {
                  downloaded,
                  total,
                })}
              </p>
            </div>
            <ProgressBarLarge progress={downloadProgress} />
          </div>
        ) : (
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
              className={styles.button}
              onClick={onInstallUpdate}
              skin={ButtonSkin}
              label={intl.formatMessage(messages.buttonLabel)}
              disabled={!areTermsOfUseAccepted}
            />
          </div>
        )}
      </div>
    );
  }
}
