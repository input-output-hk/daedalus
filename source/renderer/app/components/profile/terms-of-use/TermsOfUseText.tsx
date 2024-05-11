import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import ReactMarkdown from 'react-markdown';
import styles from './TermsOfUseText.scss';

type Props = {
  localizedTermsOfUse: string;
  onOpenExternalLink: (...args: Array<any>) => any;
};

class TermsOfUseText extends Component<Props> {
  termsOfUseClickHandler = (event: React.MouseEvent<HTMLElement>) => {
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

export default observer(TermsOfUseText);
