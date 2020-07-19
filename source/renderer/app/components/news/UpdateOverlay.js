// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import ReactMarkdown from 'react-markdown';
import News from '../../domains/News';
import styles from './UpdateOverlay.scss';
import DialogCloseButton from '../widgets/DialogCloseButton';
import ProgressBarLarge from '../widgets/ProgressBarLarge';
import ButtonLink from '../widgets/ButtonLink';

const messages = defineMessages({
  checkboxLabel: {
    id: 'profile.termsOfUse.checkboxLabel',
    defaultMessage: '!!!I agree with terms of service',
    description: 'Label for the "I agree with terms of service" checkbox.',
  },
  checkboxLabelWithDisclaimer: {
    id: 'profile.termsOfUse.checkboxLabelWithDisclaimer',
    defaultMessage:
      '!!!I understand that the terms of use are only available in English and agree to the terms of use',
    description:
      'Label for the "I agree with terms of service" checkbox when terms of use are not translated.',
  },
  submitLabel: {
    id: 'profile.termsOfUse.submitLabel',
    defaultMessage: '!!!Continue',
    description: 'Label for the "Terms of service" form submit button.',
  },
});

type Props = {
  update: News.News,
  onCloseUpdate: Function,
  downloadProgress?: number,
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
    const { content, title } = update;
    return (
      <div
        className={styles.component}
        role="presentation"
        onClick={!isUpdateDownloaded ? onCloseUpdate : () => {}}
      >
        {!isUpdateDownloaded && <DialogCloseButton onClose={onCloseUpdate} />}
        <h1 className={styles.title}>{title}</h1>
        <span className={styles.subtitle}>
          You are currently running Daedalus v 0.14.7 and v 0.15.1 is available.
        </span>
        <div className={styles.content}>
          <ReactMarkdown escapeHtml={false} source={content} />
        </div>
        {!isUpdateDownloaded ? (
          <div className={styles.downloadProgress}>
            <ProgressBarLarge progress={downloadProgress} />
          </div>
        ) : (
          <div>
            <Checkbox
              label={intl.formatMessage(messages.checkboxLabel)}
              onChange={this.toggleAcceptance}
              checked={areTermsOfUseAccepted}
              skin={CheckboxSkin}
            />
            <ButtonLink
              className={styles.actionBtn}
              onClick={onInstallUpdate}
              skin={ButtonSkin}
              label={'Restart Daedalus and Update'}
              linkProps={{
                hasIconBefore: false,
                className: styles.externalLink,
              }}
            />
          </div>
        )}
      </div>
    );
  }
}
