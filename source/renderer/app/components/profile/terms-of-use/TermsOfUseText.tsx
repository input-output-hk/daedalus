import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import ReactMarkdown from 'react-markdown';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TermsOfUseText.scss' or its ... Remove this comment to see the full error message
import styles from './TermsOfUseText.scss';

type Props = {
  localizedTermsOfUse: string;
  onOpenExternalLink: (...args: Array<any>) => any;
};

@observer
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

export default TermsOfUseText;
