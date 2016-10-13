// @flow
import React, { Component, PropTypes } from 'react';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import getMuiTheme from 'material-ui/styles/getMuiTheme';
import daedalusTheme from './themes/daedalus';

export default class App extends Component {
  static propTypes = {
    children: PropTypes.element.isRequired
  };

  render() {
    return (
      <MuiThemeProvider muiTheme={getMuiTheme(daedalusTheme)}>
        {this.props.children}
      </MuiThemeProvider>
    );
  }
}
