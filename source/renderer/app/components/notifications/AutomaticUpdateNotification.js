// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import ReactModal from 'react-modal';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './AutomaticUpdateNotification.scss';

const messages = defineMessages({
  title: {
    id: 'automaticUpdate.title',
    defaultMessage: '!!!Software update is available',
    description: 'Title for "Automatic update" overlay',
  },
  descriptionLine1: {
    id: 'automaticUpdate.description1',
    defaultMessage:
      '!!!You are currently running Daedalus <b>v {currentAppVersion}</b> and <b>{nextAppVersion}</b> is available.',
    description: 'First description line of "Automatic update" overlay',
  },
  descriptionLine2: {
    id: 'automaticUpdate.description2',
    defaultMessage:
      '!!!Would you like to install the update? If choose to postpone, the update will be installed automatically on the next Daedalus launch.',
    description: 'Second description line of "Automatic update" overlay',
  },
  acceptButtonLabel: {
    id: 'automaticUpdate.accept.button.label',
    defaultMessage: '!!!Restart Daedalus and Update',
    description:
      'Label for "Restart Daedalus and Update" action button on "Automatic update" overlay',
  },
  postponeButtonLabel: {
    id: 'automaticUpdate.postpone.button.label',
    defaultMessage: '!!!Postpone until Daedalus restart',
    description:
      'Label for "Postpone" action button on "Automatic update" overlay',
  },
  newerVersionlabel: {
    id: 'automaticUpdate.newerVersion.label',
    defaultMessage: '!!!newer version',
    description: 'Label for "newer version" on "Automatic update" overlay',
  },
});

type Props = {
  currentAppVersion: string,
  nextUpdateVersion: ?string,
  onAccept: Function,
  onPostpone: Function,
};

@observer
export default class AutomaticUpdateNotification extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      currentAppVersion,
      nextUpdateVersion,
      onAccept,
      onPostpone,
    } = this.props;
    const { formatMessage } = this.context.intl;

    return (
      <ReactModal
        isOpen
        shouldCloseOnOverlayClick={false}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        onRequestClose={onPostpone}
        ariaHideApp={false}
      >
        <div className={styles.content}>
          <h1>{formatMessage(messages.title)}</h1>
          <div className={styles.description}>
            <p>
              <FormattedHTMLMessage
                {...messages.descriptionLine1}
                values={{
                  currentAppVersion,
                  nextUpdateVersion: nextUpdateVersion
                    ? `v ${nextUpdateVersion}`
                    : formatMessage(messages.newerVersionlabel),
                }}
              />
            </p>
            <p>{formatMessage(messages.descriptionLine2)}</p>
          </div>

          <div className={styles.actionsWrapper}>
            <Button
              className={styles.acceptButton}
              label={
                <p>
                  <span className={styles.btnLabel}>
                    {formatMessage(messages.acceptButtonLabel)}
                  </span>
                </p>
              }
              onClick={onAccept}
              skin={ButtonSkin}
            />

            <span
              className={styles.postponeButton}
              onClick={onPostpone}
              role="presentation"
            >
              {formatMessage(messages.postponeButtonLabel)}
            </span>
          </div>
        </div>
      </ReactModal>
    );
  }
}
