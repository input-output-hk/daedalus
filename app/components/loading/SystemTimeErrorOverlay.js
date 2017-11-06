// @flow
import React, { Component } from 'react';
import SvgInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Button from 'react-polymorph/lib/components/Button';
import SimpleButtonSkin from 'react-polymorph/lib/skins/simple/ButtonSkin';
import attentionIcon from '../../assets/images/attention-big-light.inline.svg';
import styles from './SystemTimeErrorOverlay.scss';

const messages = defineMessages({
  overlayTitle: {
    id: 'systemTime.error.overlayTitle',
    defaultMessage: '!!!Daedalus Sync Error',
    description: 'Title of Sync error overlay'
  },
  overlayText: {
    id: 'systemTime.error.overlayText',
    defaultMessage: '!!!ATTENTION: Time of your machine is different from global time. You are 2 hours 12 minutes 54 seconds behind. You need to fix issue, because you are gonna be unable to sync system!',
    description: 'Text of Sync error overlay'
  },
  buttonLabel: {
    id: 'systemTime.error.overlayButtonLabel',
    defaultMessage: '!!!See problem solutions',
    description: 'Button label of Sync error overlay'
  },
});

@observer
export default class SystemTimeErrorOverlay extends Component {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;

    return (
      <div className={styles.component}>

        <SvgInline svg={attentionIcon} className={styles.icon} />

        <h1>{intl.formatMessage(messages.overlayTitle)}</h1>

        <p>{intl.formatMessage(messages.overlayText)}</p>

        <Button
          label={intl.formatMessage(messages.buttonLabel)}
          skin={<SimpleButtonSkin />}
        />

      </div>
    );
  }

}
