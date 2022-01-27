import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { Button } from 'react-polymorph/lib/components/Button';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { defineMessages, intlShape } from 'react-intl';
import LocalizableError from '../../../i18n/LocalizableError';
import TermsOfUseText from './TermsOfUseText';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TermsOfUseForm.scss' or its ... Remove this comment to see the full error message
import styles from './TermsOfUseForm.scss';

const messages = defineMessages({
  checkboxLabel: {
    id: 'profile.termsOfUse.checkboxLabel',
    defaultMessage: '!!!I agree with terms of service',
    description: 'Label for the "I agree with terms of service" checkbox.',
  },
  checkboxLabelWithDisclaimer: {
    id: 'profile.termsOfUse.checkboxLabelWithDisclaimer',
    defaultMessage:
      '!!!I understand that the terms of use are only available in English and agree to the terms of use',
    description:
      'Label for the "I agree with terms of service" checkbox when terms of use are not translated.',
  },
  submitLabel: {
    id: 'profile.termsOfUse.submitLabel',
    defaultMessage: '!!!Continue',
    description: 'Label for the "Terms of service" form submit button.',
  },
});
type Props = {
  localizedTermsOfUse: string;
  onSubmit: (...args: Array<any>) => any;
  isSubmitting: boolean;
  error?: LocalizableError | null | undefined;
  onOpenExternalLink: (...args: Array<any>) => any;
};
type State = {
  areTermsOfUseAccepted: boolean;
};

@observer
class TermsOfUseForm extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    areTermsOfUseAccepted: false,
  };
  toggleAcceptance = () => {
    this.setState((prevState) => ({
      areTermsOfUseAccepted: !prevState.areTermsOfUseAccepted,
    }));
  };
  submit = () => {
    this.props.onSubmit();
  };

  render() {
    const { intl } = this.context;
    const {
      isSubmitting,
      error,
      localizedTermsOfUse,
      onOpenExternalLink,
    } = this.props;
    const { areTermsOfUseAccepted } = this.state;
    const buttonClasses = classnames([
      'primary',
      isSubmitting ? styles.submitButtonSpinning : styles.submitButton,
    ]);
    return (
      <div className={styles.component}>
        <div className={styles.centeredBox}>
          <TermsOfUseText
            localizedTermsOfUse={localizedTermsOfUse}
            onOpenExternalLink={onOpenExternalLink}
          />

          <div className={styles.checkbox}>
            <Checkbox
              label={intl.formatMessage(messages.checkboxLabel)}
              onChange={this.toggleAcceptance}
              checked={areTermsOfUseAccepted}
              skin={CheckboxSkin}
            />
          </div>

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

          <Button
            className={buttonClasses}
            label={intl.formatMessage(messages.submitLabel)}
            onClick={this.submit}
            disabled={!areTermsOfUseAccepted}
            skin={ButtonSkin}
          />
        </div>
      </div>
    );
  }
}

export default TermsOfUseForm;
