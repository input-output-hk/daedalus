import React, { Component, Fragment } from 'react';
import { map } from 'lodash';

export default class ThemeManager extends Component {
  componentDidMount() {
    // eslint-disable-next-line react/prop-types
    this.updateCSSVariables(this.props.variables);
  }

  componentDidUpdate(prevProps) {
    if (this.props.variables !== prevProps.variables) {
      this.updateCSSVariables(this.props.variables);
    }
  }

  updateCSSVariables(variables) {
    map(variables, (value, prop) => {
      document.documentElement.style.setProperty(prop, value);
    });
  }
  render() {
    // eslint-disable-next-line react/prop-types
    return <Fragment>{this.props.children}</Fragment>;
  }
}
