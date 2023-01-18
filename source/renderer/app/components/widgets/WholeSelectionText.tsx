import React, { Component } from 'react';

type Props = {
  className?: string;
  text?: string | null | undefined;
};
export default class WholeSelectionText extends Component<Props> {
  static defaultProps = {
    className: null,
    text: '',
  };
  selectWholeText = (event: React.MouseEvent<HTMLElement>) => {
    const { target } = event;

    if (target instanceof HTMLElement) {
      const selection = window.getSelection();
      const range = document.createRange();
      range.selectNodeContents(target);
      selection.removeAllRanges();
      selection.addRange(range);
    }
  };

  render() {
    const { className, text } = this.props;
    return (
      <span className={className} onClick={this.selectWholeText}>
        {text}
      </span>
    );
  }
}
