// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import prettysize from 'prettysize';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import attentionIcon from '../../assets/images/attention-big-light.inline.svg';
import styles from './NoDiskSpaceErrorOverlay.scss';

const messages = defineMessages({
  overlayTitle: {
    id: 'noDiskSpace.error.overlayTitle',
    defaultMessage: '!!!Daedalus Disk Space Error',
    description: 'Title of No disk space overlay'
  },
  overlayContent: {
    id: 'noDiskSpace.error.overlayContent',
    defaultMessage: '!!!ATTENTION: No disk space left on a device. Node requires {diskSpaceRequired} more space. Free the space and click a button to continue. You need to fix issue, because you are gonna be unable to sync system!',
    description: 'Content of No disk space overlay'
  },
  overlayButtonText: {
    id: 'noDiskSpace.error.overlayButtonText',
    defaultMessage: '!!!Check and continue',
    description: 'Button Text of No disk space overlay'
  },
});

type Props = {
  onCheckDiskSpace: Function,
  isCheckingNoDiskSpace: boolean,
  diskSpaceRequired: number,
};

@observer
export default class NoDiskSpaceErrorOverlay extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      onCheckDiskSpace,
      isCheckingNoDiskSpace,
      diskSpaceRequired
    } = this.props;

    return (
      <div className={styles.component}>

        <SVGInline svg={attentionIcon} className={styles.icon} />

        <div>
          <h1>{intl.formatMessage(messages.overlayTitle)}</h1>

          <p>
            <FormattedMessage
              {...messages.overlayContent}
              values={{ diskSpaceRequired: prettysize(diskSpaceRequired) }}
            />
          </p>

          <Button
            className="disclaimer"
            label={intl.formatMessage(messages.overlayButtonText)}
            onClick={() => onCheckDiskSpace()}
            disabled={isCheckingNoDiskSpace}
            skin={ButtonSkin}
          />
        </div>

      </div>
    );
  }

}
