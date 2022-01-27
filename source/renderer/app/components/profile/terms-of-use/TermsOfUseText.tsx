// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import ReactMarkdown from 'react-markdown';
import styles from './TermsOfUseText.scss';

type Props = {
  localizedTermsOfUse: string,
  onOpenExternalLink: Function,
};

@observer
export default class TermsOfUseText extends Component<Props> {
  termsOfUseClickHandler = (event: SyntheticMouseEvent<HTMLElement>) => {
    const linkUrl = get(event, ['target', 'href']);
    if (linkUrl) {
      event.preventDefault();
      this.props.onOpenExternalLink(linkUrl);
    }
  };

  render() {
    return (
      <div
        className={styles.terms}
        role="presentation"
        onClick={this.termsOfUseClickHandler}
      >
        <ReactMarkdown
          escapeHtml={false}
          source={this.props.localizedTermsOfUse}
        />
      </div>
    );
  }
}
