// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import prettysize from 'prettysize';
import attentionIcon from '../../assets/images/attention-big-light.inline.svg';
import styles from './NoDiskSpaceErrorOverlay.scss';

const messages = defineMessages({
  overlayContent: {
    id: 'noDiskSpace.error.overlayContent',
    defaultMessage: '!!!<b>There is not enough disk space left on your device.</b><br />Daedalus requires {diskSpaceRequired} more space to operate. Please free up some disk space and then click the button below to continue.',
    description: 'Content of No disk space overlay'
  },
  overlayTitle: {
    id: 'noDiskSpace.error.overlayTitle',
    defaultMessage: '!!!Not enough disk space',
    description: 'Title of No disk space overlay'
  },
});

type Props = {
  diskSpaceAvailable: number,
  diskSpaceRequired: number,
  diskSpaceMissing: number,
};

@observer
export default class NoDiskSpaceErrorOverlay extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      /* diskSpaceAvailable, */ diskSpaceRequired/* , diskSpaceMissing */
    } = this.props;

    return (
      <div className={styles.component}>

        <SVGInline svg={attentionIcon} className={styles.icon} />

        <div>
          <h1>{intl.formatMessage(messages.overlayTitle)}</h1>

          <p>
            <FormattedHTMLMessage
              {...messages.overlayContent}
              values={{ diskSpaceRequired: prettysize(diskSpaceRequired) }}
            />
          </p>

        </div>

      </div>
    );
  }

}
