// @flow
import React, { Component, createRef } from 'react';
import { get } from 'lodash';
import { observer } from 'mobx-react';
import { Fireworks } from 'fireworks-js';
import styles from './FullyDecentralizedEffect.scss';
import {
  CREATE_THEME_PARAMS,
  CREATE_THEME_OBJ,
} from '../../themes/utils/constants';

console.log('CREATE_THEME_PARAMS', CREATE_THEME_PARAMS);
console.log('CREATE_THEME_OBJ', CREATE_THEME_OBJ);

type Props = {
  isActive: boolean,
};

@observer
export default class FullyDecentralizedEffect extends Component<Props> {
  constructor(props: Props) {
    super(props);
    this.container = createRef();
  }

  container: ?any;
  fireworks: ?Object = null;

  componentDidMount() {
    const { isActive } = this.props;
    const container = get(this, 'container.current');
    if (container instanceof HTMLElement) {
      const fireworks = new Fireworks({
        target: container,
        hue: 120,
        startDelay: 1,
        minDelay: 20,
        maxDelay: 30,
        speed: 4,
        acceleration: 1.05,
        friction: 0.98,
        gravity: 1,
        particles: 75,
        trace: 3,
        explosion: 5,
        boundaries: {
          top: 50,
          bottom: container.clientHeight,
          left: 50,
          right: container.clientWidth,
        },
      });
      this.fireworks = fireworks;
      if (isActive) {
        fireworks.start();
      }
    }
  }

  componentDidUpdate() {
    const { isActive } = this.props;
    const { fireworks } = this;
    if (isActive && fireworks) {
      fireworks.start();
    } else if (!isActive && fireworks) {
      fireworks.stop();
    }
  }

  render() {
    const { isActive } = this.props;
    return <div className={styles.component} ref={this.container} />;
  }
}
