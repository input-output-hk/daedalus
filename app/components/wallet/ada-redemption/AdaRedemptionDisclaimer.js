// @flow
import React, { Component } from 'react';
import SvgInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleCheckboxSkin from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import Button from 'react-polymorph/lib/components/Button';
import SimpleButtonSkin from 'react-polymorph/lib/skins/simple/ButtonSkin';
import attentionIcon from '../../../assets/images/attention-big-light.inline.svg';
import styles from './AdaRedemptionDisclaimer.scss';

const messages = defineMessages({
  disclaimerTitle: {
    id: 'wallet.redeem.disclaimerOverlay.title',
    defaultMessage: '!!!Daedalus Redemption Disclamer',
    description: 'Title of "Redemption disclaimer" on Ada redemption page.'
  },
  disclaimerText: {
    id: 'wallet.redeem.disclaimerOverlay.disclaimerText',
    defaultMessage: '!!!ATTENTION: Redeeming on the Cardano Test-net will validate that your certificate or redemption key is correct and will allow you to redeem TEST-ADA for testing purposes only. KEEP your certificate or redemption key safe and secure. You will need to redeem again when Cardano SL launches the mainnet. TEST-ADA holds no value and cannot be exchanged.',
    description: 'Disclaimer text for "Redemption disclaimer" on Ada redemption page.'
  },
  checkboxLabel: {
    id: 'wallet.redeem.disclaimerOverlay.checkboxLabel',
    defaultMessage: '!!!I’ve understood the information above',
    description: 'Label for "I’ve understood the information above" checkbox on Ada redemption page "Redemption disclaimer".'
  },
  submitLabel: {
    id: 'wallet.redeem.disclaimerOverlay.submitLabel',
    defaultMessage: '!!!Continue',
    description: 'Label for "Continue" button on Ada redemption page "Redemption disclaimer".'
  },
});

type Props = {
  onSubmit: Function,
};

type State = {
  isAccepted: boolean,
};

@observer
export default class AdaRedemptionDisclaimer extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isAccepted: false,
  };

  onAcceptToggle = (value: boolean) => {
    this.setState({ isAccepted: value });
  };

  render() {
    const { intl } = this.context;
    const { onSubmit } = this.props;
    const { isAccepted } = this.state;

    const submitButtonStyles = classnames([
      !isAccepted ? styles.disabled : null,
    ]);

    return (
      <div className={styles.component}>

        <SvgInline svg={attentionIcon} className={styles.icon} />

        <h1>{intl.formatMessage(messages.disclaimerTitle)}</h1>

        <p>{intl.formatMessage(messages.disclaimerText)}</p>

        <div className="adaRedemptionDisclaimerCheckbox">
          <Checkbox
            label={intl.formatMessage(messages.checkboxLabel)}
            onChange={this.onAcceptToggle}
            checked={isAccepted}
            skin={<SimpleCheckboxSkin />}
          />
        </div>

        <Button
          className={submitButtonStyles}
          label={intl.formatMessage(messages.submitLabel)}
          onClick={() => isAccepted && onSubmit()}
          disabled={!isAccepted}
          skin={<SimpleButtonSkin />}
        />

      </div>
    );
  }

}
