// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import getMuiTheme from 'material-ui/styles/getMuiTheme';
import daedalusTheme from './themes/daedalus';

@observer
export default class App extends Component {
  render() {
    return (
      <MuiThemeProvider muiTheme={getMuiTheme(daedalusTheme)}>
        {this.props.children}
      </MuiThemeProvider>
    );
  }
}

App.propTypes = {
  children: PropTypes.element.isRequired
};
