// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import icon from '../../../assets/images/not-responding.inline.svg';
import styles from './NotResponding.scss';

type Props = {
  onRestartNode: Function,
};

export default class NotResponding extends Component<Props> {
  render() {
    const { onRestartNode } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.content}>
          <SVGInline svg={icon} className={styles.icon} />
          <div className={styles.title}>Wallet not responding</div>
          <div className={styles.description}>
            <p>
              Your “Darko’s ada” wallet is not responding. This is caused by a
              known issue which happens very rarely and it is currently being
              fixed. Please use the button below to restart Cardano node which
              should resolve the issue. If the issue persists or if it happens
              again, please submit a support request.
            </p>
          </div>
          <Button
            className={styles.button}
            label="Restart Cardano node"
            onClick={onRestartNode}
            skin={ButtonSkin}
          />
        </div>
      </div>
    );
  }
}
