// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import {
  defineMessages,
  intlShape,
  FormattedHTMLMessage,
} from 'react-intl';
import ReactModal from 'react-modal';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import linkNewWindow from '../../assets/images/link-ic.inline.svg';
import styles from './ManualUpdateOverlay.scss';

const messages = defineMessages({
  title: {
    id: 'manualUpdateOverlay.title',
    defaultMessage: '!!!Software update is available',
    description: 'Title for "Manual update" overlay',
  },
  descriptionLine1: {
    id: 'manualUpdateOverlay.description1',
    defaultMessage: '!!!You are experiencing network connection issues, and you are not running the latest Daedalus version. Automatic updates are unavailable while Daedalus is not connected to Cardano network.',
    description: 'Description line 1 of "Manual update" overlay',
  },
  descriptionLine2: {
    id: 'manualUpdateOverlay.description2',
    defaultMessage: '!!!You are currently running <b>{currentAppVersion}</b> version of Daedalus, and <b>{availableAppVersion}</b> version is available. Please manually update to that version since it may resolve your connecting issues.',
    description: 'Description line 2 of "Manual update" overlay',
  },
  actionButtonLabel: {
    id: 'manualUpdateOverlay.button.label',
    defaultMessage: '!!!Follow instructions and manually update',
    description: 'Label for "Follow instructions and manually update" action button on "Manual update" overlay',
  },
  manualUpdateButtonUrl: {
    id: 'manualUpdateOverlay.button.url',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/articles/360023850634',
    description: 'Follow instructions and manually update link on "Manual update" overlay',
  },
});

type Props = {
  availableAppVersion: ?string,
  currentAppVersion: string,
  onExternalLinkClick: Function,
};

@observer
export default class ManualUpdateOverlay extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { availableAppVersion, currentAppVersion, onExternalLinkClick } = this.props;
    const { formatMessage } = this.context.intl;

    return (
      <ReactModal
        isOpen
        shouldCloseOnOverlayClick={false}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <div className={styles.content}>
          <h1>{formatMessage(messages.title)}</h1>
          <div className={styles.description}>
            <p>{formatMessage(messages.descriptionLine1)}</p>
            <p>
              <FormattedHTMLMessage
                {...messages.descriptionLine2}
                values={{
                  currentAppVersion,
                  availableAppVersion,
                }}
              />
            </p>
          </div>

          <div>
            <Button
              className={styles.actionButton}
              label={(
                <p>
                  <span>{formatMessage(messages.actionButtonLabel)}</span>
                  <SVGInline svg={linkNewWindow} className={styles.linkNewWindow} />
                </p>
              )}
              onClick={() => onExternalLinkClick(formatMessage(messages.manualUpdateButtonUrl))}
              skin={ButtonSkin}
            />
          </div>
        </div>
      </ReactModal>
    );
  }
}
