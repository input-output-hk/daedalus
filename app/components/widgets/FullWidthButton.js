import React, { Component, PropTypes } from 'react';
import RaisedButton from 'material-ui/RaisedButton';
import styles from './FullWidthButton.scss';

export default class label extends Component {

  static propTypes = {
    label: PropTypes.string.isRequired,
    onClick: PropTypes.func.isRequired
  };

  render() {
    return (
      <RaisedButton
        className={styles.button}
        label={this.props.label}
        onClick={this.props.onClick}
        primary
        fullWidth
      />
    );
  }

}
