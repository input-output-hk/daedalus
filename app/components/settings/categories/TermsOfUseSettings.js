// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import styles from './TermsOfUseSettings.scss';

@observer
export default class TermsOfUseSettings extends Component {

  static propTypes = {
    text: PropTypes.string.isRequired,
  };

  render() {
    const { text } = this.props;
    return (
      <div className={styles.component} dangerouslySetInnerHTML={{ __html: text }} />
    );
  }

}
