// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Button from 'react-toolbox/lib/button/Button';
import { defineMessages, intlShape } from 'react-intl';
import CheckboxWithLongLabel from '../../widgets/forms/CheckboxWithLongLabel';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './TermsOfUseForm.scss';

const messages = defineMessages({
  checkboxLabel: {
    id: 'profile.termsOfUse.checkboxLabel',
    defaultMessage: '!!!I agree with terms of use',
    description: 'Label for the "I agree with terms of use" checkbox.'
  },
  submitLabel: {
    id: 'profile.termsOfUse.submitLabel',
    defaultMessage: '!!!Continue',
    description: 'Label for the "Terms of use" form submit button.'
  },
});

@observer
export default class TermsOfUseForm extends Component {

  props: {
    onSubmit: Function,
    isSubmitting: boolean,
    error?: ?LocalizableError,
  };

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
    const { isSubmitting, error } = this.props;
    const { areTermsOfUseAccepted } = this.state;

    return (
      <div className={styles.component}>
        <div className={styles.centeredBox}>

          <div className={styles.terms}>
            <h1>Terms of use</h1>

            <p>
              First paragraph quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur,
              adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore
              some link aliquam quaerat voluptatem.
            </p>

            <p>
              Second paragraph ad minima veniam, quis nostrum exercitationem ullam corporis
              suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum
              iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur,
              vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?
            </p>

            <ol>
              <li>
                <h2>1. Numbered title</h2>
                <ul>
                  <li>
                    <strong>1.1 Sub-title name.</strong>
                    At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis
                    praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias
                    excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui
                    officia deserunt mollitia animi, id est laborum et dolorum fuga.
                  </li>
                  <li>
                    <strong>1.2 Sub-title name.</strong>
                    Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore,
                    cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod
                    maxime placeat facere possimus, omnis voluptas assumenda est,
                    omnis dolor repellendus.
                  </li>
                </ul>
              </li>

              <li>
                <h2>2. Numbered title</h2>
                <ul>
                  <li>
                    <strong>2.1 Sub-title name.</strong>
                    At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis
                    praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias
                    excepturi sint occaecati cupiditate non provident, similique sunt in culpa...
                    and so on...
                  </li>
                </ul>
              </li>
            </ol>

            <p>
              Last paragraph quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur,
              adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore
              some link aliquam quaerat voluptatem.
            </p>

            <CheckboxWithLongLabel
              className={styles.checkbox}
              label={intl.formatMessage(messages.checkboxLabel)}
              onChange={this.toggleAcceptance.bind(this)}
              checked={areTermsOfUseAccepted}
            />

          </div>

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

          <Button
            className={isSubmitting ? styles.submitButtonSpinning : styles.submitButton}
            label={intl.formatMessage(messages.submitLabel)}
            onMouseUp={this.submit}
            primary
            disabled={!areTermsOfUseAccepted}
          />

        </div>
      </div>
    );
  }

}
