import React, { Component, PropTypes } from 'react';
import { Link } from 'react-router';

export default class NavigationLink extends Component {

  static propTypes = {
    children: PropTypes.element.isRequired,
    to: PropTypes.string.isRequired,
    linkStyles: PropTypes.shape
  };

  render() {
    const { to, children, linkStyles } = this.props;
    return (
      <Link to={to}>{
        ({ isActive, href, onClick }) =>
          <a href={href} onClick={onClick} className={linkStyles}>
            {React.cloneElement(children, { isActive })}
          </a>
      }</Link>
    );
  }

}
