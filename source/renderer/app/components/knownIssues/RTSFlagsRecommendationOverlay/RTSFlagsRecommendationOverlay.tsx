// @ts-nocheck

import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import DialogCloseButton from '../../widgets/DialogCloseButton';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';
import styles from './RTSFlagsRecommendationOverlay.scss';

type Props = {
  onConfirm: () => void;
  onClose: () => void;
};
const messages = defineMessages({
  title: {
    id: 'knownIssues.rtsRecommendationOverlay.title',
    defaultMessage: '!!!Recommended hardware requirements status',
    description: 'Title of the RTS flags recommendation overlay',
  },
  content: {
    id: 'knownIssues.rtsRecommendationOverlay.content',
    defaultMessage:
      '!!!<p>Your system specifications do not meet Daedalus’ recommended hardware requirements.</p><p>You can enable RAM management (RTS Flags), an experimental setting that can reduce memory usage on computers with less than 16GB of RAM.</p><p>You can enable it now by clicking the ‘Enable and quit’ button. Note that you will have to restart Daedalus for this change to take effect. To enable or disable it at any time, go to the Help menu.</p>',
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
class RTSFlagsRecommendationOverlay extends Component<Props> {
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
          <FormattedHTMLMessage {...messages.content} />
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

export default RTSFlagsRecommendationOverlay;
