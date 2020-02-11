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
    id: 'experimentalData.overlay.title',
    defaultMessage: '!!!Experimental features',
    description: 'Experimental features',
  },
  description: {
    id: 'experimentalData.overlay.description',
    defaultMessage:
      '<p>!!!The goal of the Incentivized Testnet is to test and improve the stake delegation features of the Shelley era, including new Daedalus features, before their release on the Cardano mainnet. Due to the nature of the testnet, some of these features may be unreliable or subject to change as they are iterated upon by the development team.</p><p>Any new Daedalus features which are considered experimental will be marked with a <span class="experimentalIcon"></span> icon. Take care when using information from experimental features to make delegation decisions, as the data could be inaccurate. The <span class="experimentalIcon"></span> icon will be removed when a feature is considered stable and reliable to use.</p>',
    description: 'Experimental data description',
  },
  actionLabel: {
    id: 'experimentalData.overlay.actionLabel',
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
      <FormattedHTMLMessage {...messages.description} />
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
