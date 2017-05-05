// @flow
import React, { Component } from 'react';
import { intlShape } from 'react-intl';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './TermsOfUseSettings.scss';
import termsOfUseMessages from '../../../i18n/termsOfUse';

@observer
export default class TermsOfUseSettings extends Component {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const componentClassNames = classnames([styles.component, styles.termsOfUse]);
    return (
      <div className={componentClassNames}>
        <h1>{intl.formatMessage(termsOfUseMessages.label)}</h1>
        <p>{intl.formatMessage(termsOfUseMessages.firstParagraph)}</p>
        <p>{intl.formatMessage(termsOfUseMessages.secondParagraph)}</p>
        <ol>
          <li>
            <h2>{intl.formatMessage(termsOfUseMessages.firstNumberedTitle)}</h2>
            <ul>
              <li>
                <strong>{intl.formatMessage(termsOfUseMessages.firstSubtitle)}</strong>
                {intl.formatMessage(termsOfUseMessages.firstListText)}
              </li>
              <li>
                <strong>{intl.formatMessage(termsOfUseMessages.secondSubtitle)}</strong>
                {intl.formatMessage(termsOfUseMessages.secondListText)}
              </li>
            </ul>
          </li>
          <li>
            <h2>{intl.formatMessage(termsOfUseMessages.secondNumberedTitle)}</h2>
            <ul>
              <li>
                <strong>{intl.formatMessage(termsOfUseMessages.thirdSubtitle)}</strong>
                {intl.formatMessage(termsOfUseMessages.thirdListText)}
              </li>
            </ul>
          </li>
        </ol>
        <p>{intl.formatMessage(termsOfUseMessages.lastParagraph)}</p>
      </div>
    );
  }

}
