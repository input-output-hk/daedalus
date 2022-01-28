// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';
import styles from './RTSFlagsRecommendationOverlay.scss';

type Props = {
  onConfirm: () => void,
  onClose: () => void,
};

const messages = defineMessages({
  title: {
    id: 'knownIssues.rtsRecommendationOverlay.title',
    defaultMessage: '!!!RTS flag warning',
    description: 'Title of the RTS flags recommendation overlay',
  },
  content: {
    id: 'knownIssues.rtsRecommendationOverlay.content',
    defaultMessage:
      '!!!WARNING/REMINDER: You are running Daedalus with less memory than recommended. You can try these experimental settings to improve performance.',
    description: 'Content of the RTS flags recommendation overlay',
  },
  enableAndQuitButtonLabel: {
    id: 'knownIssues.rtsRecommendationOverlay.enableAndQuitButtonLabel',
    defaultMessage: '!!!Enable and quit',
    description: 'Enable and quit button label',
  },
  decideLaterButtonLabel: {
    id: 'knownIssues.rtsRecommendationOverlay.decideLaterButtonLabel',
    defaultMessage: '!!!Decide later',
    description: 'Decide later button label',
  },
});

@observer
export default class RTSFlagsRecommendationOverlay extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, onConfirm } = this.props;

    // TODO reduce duplication with AlertsOverlay
    // https://input-output.atlassian.net/browse/DDW-928
    return (
      <div className={styles.component}>
        <DialogCloseButton
          className={styles.closeButton}
          icon={closeCrossThin}
          onClose={onClose}
        />
        <h1 className={styles.title}>{intl.formatMessage(messages.title)}</h1>
        <div className={styles.content}>
          {intl.formatMessage(messages.content)}
        </div>
        <Button
          className={styles.actionBtn}
          onClick={onConfirm}
          label={intl.formatMessage(messages.enableAndQuitButtonLabel)}
          linkProps={{
            hasIconBefore: false,
            hasIconAfter: false,
          }}
        />
        <Link
          className={styles.decideLaterLink}
          onClick={onClose}
          label={intl.formatMessage(messages.decideLaterButtonLabel)}
          hasIconAfter={false}
          skin={LinkSkin}
        />
      </div>
    );
  }
}
