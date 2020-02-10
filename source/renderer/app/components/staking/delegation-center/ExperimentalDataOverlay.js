// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import testnetBulbIcon from '../../../assets/images/testnet-bulb.inline.svg';

import styles from './ExperimentalDataOverlay.scss';

const messages = defineMessages({
  title: {
    id: 'experimental.data.overlay.title',
    defaultMessage: '!!!Experimental features',
    description: 'Experimental features',
  },
  incentivizedTestnetDescription: {
    id: 'experimental.data.overlay.description',
    defaultMessage:
      '<p>!!!As well as an environment for testing the core staking and delegation features of the Shelley era, the Incentivized Testnet is also a valuable opportunity to test a number of new features in Daedalus.</p><p>Our goal is to test and to iterate, and also to experiment. However, given the nature of the testnet - network inconsistencies, feature incompleteness etc. - this does mean that some of the features we shall be trying out in Daedalus for the Incentivized Testnet may not work reliably. For example, we are not currently able to calculate all parameters precisely, so this data should not be relied upon.</p><p>While we continue to work on resolving those issues, stake pool parameters which may contain inaccurate or unreliable data are marked with this icon <span className="icon"></span> in stake pool details.</p>',
    description: 'Experimental data description',
  },
  actionLabel: {
    id: 'experimental.data.overlay.actionLabel',
    defaultMessage: '!!!I understand',
    description: 'Experimental data action',
  },
});

type Props = {
  onClose: Function,
};

export default class ExperimentalDataOverlay extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose } = this.props;
    const title = intl.formatMessage(messages.title);
    const actionLabel = intl.formatMessage(messages.actionLabel);
    const description = (
      <FormattedHTMLMessage {...messages.incentivizedTestnetDescription} />
    );

    return (
      <div className={styles.component}>
        <div className={styles.content}>
          <SVGInline svg={testnetBulbIcon} className={styles.testnetBulbIcon} />
          <div className={styles.title}>{title}</div>
          <div className={styles.description}>{description}</div>
          <div className={styles.action}>
            <Button
              className={styles.actionButton}
              label={actionLabel}
              onClick={onClose}
              skin={ButtonSkin}
            />
          </div>
        </div>
      </div>
    );
  }
}
