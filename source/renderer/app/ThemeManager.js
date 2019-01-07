import React, { Component, Fragment } from 'react';
import { map } from 'lodash';

export default class ThemeManager extends Component {
  componentDidMount() {
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
    return <Fragment>{this.props.children}</Fragment>;
  }
}
