// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import Button from 'react-polymorph/lib/components/Button';
import SimpleButtonSkin from 'react-polymorph/lib/skins/simple/raw/ButtonSkin';
import { defineMessages, intlShape } from 'react-intl';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleCheckboxSkin from 'react-polymorph/lib/skins/simple/raw/CheckboxSkin';
import LocalizableError from '../../../i18n/LocalizableError';
import TermsOfUseText from './TermsOfUseText';
import styles from './TermsOfUseForm.scss';
import environment from '../../../environment';

const messages = defineMessages({
  checkboxLabel: {
    id: 'profile.termsOfUse.checkboxLabel',
    defaultMessage: '!!!I agree with terms of use',
    description: 'Label for the "I agree with terms of use" checkbox.'
  },
  checkboxLabelWithDisclaimer: {
    id: 'profile.termsOfUse.checkboxLabelWithDisclaimer',
    defaultMessage: '!!!I understand that the terms of use are only available in English and agree to the terms of use',
    description: 'Label for the "I agree with terms of use" checkbox when terms of use are not translated.'
  },
  submitLabel: {
    id: 'profile.termsOfUse.submitLabel',
    defaultMessage: '!!!Continue',
    description: 'Label for the "Terms of use" form submit button.'
  },
});

type Props = {
  localizedTermsOfUse: string,
  onSubmit: Function,
  isSubmitting: boolean,
  error?: ?LocalizableError,
};

type State = {
  areTermsOfUseAccepted: boolean,
};

@observer
export default class TermsOfUseForm extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    areTermsOfUseAccepted: false,
  };

  toggleAcceptance() {
    this.setState({ areTermsOfUseAccepted: !this.state.areTermsOfUseAccepted });
  }

  submit = () => {
    this.props.onSubmit();
  };

  render() {
    const { intl } = this.context;
    const { isSubmitting, error, localizedTermsOfUse } = this.props;
    const { areTermsOfUseAccepted } = this.state;
    const buttonClasses = classnames([
      'primary',
      isSubmitting ? styles.submitButtonSpinning : styles.submitButton,
    ]);

    const checkboxLabel = environment.isEtcApi() ? 'checkboxLabelWithDisclaimer' : 'checkboxLabel';

    return (
      <div className={styles.component}>
        <div className={styles.centeredBox}>

          <TermsOfUseText localizedTermsOfUse={localizedTermsOfUse} />

          <div className={styles.checkbox}>
            <Checkbox
              label={intl.formatMessage(messages[checkboxLabel])}
              onChange={this.toggleAcceptance.bind(this)}
              checked={areTermsOfUseAccepted}
              skin={<SimpleCheckboxSkin />}
            />
          </div>

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

          <Button
            className={buttonClasses}
            label={intl.formatMessage(messages.submitLabel)}
            onMouseUp={this.submit}
            disabled={!areTermsOfUseAccepted}
            skin={<SimpleButtonSkin />}
          />

        </div>
      </div>
    );
  }

}
