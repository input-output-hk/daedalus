// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import styles from './BackToTopButton.scss';

const messages = defineMessages({
  backToTopLabel: {
    id: 'backToTopButton.label',
    defaultMessage: '!!!Back to top',
    description: '"backToTop" button label.',
  },
});

type Props = {
  scrollableElementClassName: string,
  buttonTopPosition: number,
  scrollTopToActivate: number,
};

type State = {
  isActive: boolean,
};

export default class BackToTopButton extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    scrollTopToActivate: 20,
    buttonTopPosition: 20,
  };

  state = {
    isActive: false,
  };

  scrollableDomElement: ?HTMLElement = null;

  componentDidMount() {
    this.scrollableDomElement = document.querySelector(
      `.${this.props.scrollableElementClassName}`
    );
    if (!this.scrollableDomElement) return false;
    return this.scrollableDomElement.addEventListener(
      'scroll',
      this.getIsBackToTopActive
    );
  }

  getIsBackToTopActive = () => {
    const { isActive } = this.state;
    const { scrollTopToActivate } = this.props;
    if (this.scrollableDomElement instanceof HTMLElement) {
      const scrollPosition = this.scrollableDomElement.scrollTop;
      if (scrollPosition > scrollTopToActivate && !isActive) {
        this.setState({ isActive: true });
      } else if (scrollPosition <= scrollTopToActivate && isActive) {
        this.setState({ isActive: false });
      }
    }
  };

  backToTop = () => {
    if (this.scrollableDomElement instanceof HTMLElement) {
      this.scrollableDomElement.scrollTop = 0;
    }
  };

  render() {
    const { intl } = this.context;
    const { isActive } = this.state;
    const { buttonTopPosition } = this.props;
    const componentStyles = classnames(styles.component, {
      [styles.isActive]: isActive,
    });
    const top = isActive ? buttonTopPosition : buttonTopPosition - 10;
    return (
      <button
        style={{ top }}
        className={componentStyles}
        onClick={this.backToTop}
      >
        {intl.formatMessage(messages.backToTopLabel)}
      </button>
    );
  }
}
