// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import ReactMarkdown from 'react-markdown';
import styles from './TermsOfUseText.scss';

@observer
export default class TermsOfUseText extends Component {

  props: {
    localizedTermsOfUse: string,
  };

  render() {
    return (
      <div className={styles.terms}>
        <ReactMarkdown source={this.props.localizedTermsOfUse} />
      </div>
    );
  }

}
