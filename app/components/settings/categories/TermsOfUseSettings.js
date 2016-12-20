// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './TermsOfUseSettings.scss';

@observer
export default class TermsOfUseSettings extends Component {

  static propTypes = {
    text: PropTypes.string.isRequired,
  };

  render() {
    const { text } = this.props;
    const componentClassNames = classnames([styles.component, 'termsOfUse']);
    return (
      <div className={componentClassNames} dangerouslySetInnerHTML={{ __html: text }} />
    );
  }

}
