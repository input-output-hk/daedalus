// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import attentionIcon from '../../../assets/images/attention-big-light.inline.svg';
import styles from './NoDiskSpaceError.scss';

const messages = defineMessages({
  overlayContent: {
    id: 'noDiskSpace.error.overlayContent',
    defaultMessage:
      '!!!<b>Daedalus requires at least {diskSpaceRequired} of hard drive space to operate. Your computer is missing {diskSpaceMissing} of available space. Please delete some files to increase available hard drive space to continue using Daedalus. </b><br /><br />It is recommended to have at least 15% of hard drive space available ({diskSpaceRecommended} in your case) for normal and stable operation of the operating system and installed programs. We strongly recommend that you free up at least that amount of space from your hard drive.',
    description: 'Content of No disk space overlay',
  },
  overlayTitle: {
    id: 'noDiskSpace.error.overlayTitle',
    defaultMessage: '!!!Daedalus requires more hard drive space',
    description: 'Title of No disk space overlay',
  },
});

type Props = {
  diskSpaceRequired: string,
  diskSpaceMissing: string,
  diskSpaceRecommended: string,
};

@observer
export default class NoDiskSpaceError extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      diskSpaceRequired,
      diskSpaceMissing,
      diskSpaceRecommended,
    } = this.props;

    return (
      <div className={styles.component}>
        <SVGInline svg={attentionIcon} className={styles.icon} />
        <div>
          <h1>{intl.formatMessage(messages.overlayTitle)}</h1>
          <p>
            <FormattedHTMLMessage
              {...messages.overlayContent}
              values={{
                diskSpaceRequired,
                diskSpaceMissing,
                diskSpaceRecommended,
              }}
            />
          </p>
        </div>
      </div>
    );
  }
}
