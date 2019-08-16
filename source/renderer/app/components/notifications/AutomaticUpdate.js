// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import ReactModal from 'react-modal';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DialogCloseButton from '../widgets/DialogCloseButton';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import styles from './AutomaticUpdate.scss';

const messages = defineMessages({
  title: {
    id: 'automaticUpdate.title',
    defaultMessage: '!!!Software update is available',
    description: 'Title for "Automatic update" overlay',
  },
  descriptionLine1: {
    id: 'automaticUpdate.description1',
    defaultMessage:
      '!!!You are currently running Daedalus v <b>{currentAppVersion}</b> and <b>newer version</b> is available.',
    description: 'First description line of "Automatic update" overlay',
  },
  descriptionLine2: {
    id: 'automaticUpdate.description2',
    defaultMessage:
      '!!!Would you like to install the update? If you close this overlay or choose to postpone, the update will be installed automatically on the next Daedalus launch.',
    description: 'Second description line of "Automatic update" overlay',
  },
  acceptButtonLabel: {
    id: 'automaticUpdate.accept.button.label',
    defaultMessage: '!!!Restart and Update',
    description:
      'Label for "Restart and Update" action button on "Automatic update" overlay',
  },
  postponeButtonLabel: {
    id: 'automaticUpdate.postpone.button.label',
    defaultMessage: '!!!Postpone',
    description:
      'Label for "Postpone" action button on "Automatic update" overlay',
  },
});

type Props = {
  currentAppVersion: string,
  onAccept: Function,
  onPostpone: Function,
};

@observer
export default class AutomaticUpdate extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { currentAppVersion, onAccept, onPostpone } = this.props;
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
                values={{ currentAppVersion }}
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

          <DialogCloseButton
            className={styles.closeButton}
            icon={closeCrossThin}
            onClose={onPostpone}
          />
        </div>
      </ReactModal>
    );
  }
}
