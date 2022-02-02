import React, { Component, createRef } from 'react';
import { get } from 'lodash';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { Fireworks } from 'fireworks-js';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './FullyDecentralizedEffect.scs... Remove this comment to see the full error message
import styles from './FullyDecentralizedEffect.scss';

type Props = {
  isActive: boolean;
  className?: string;
};

@observer
class FullyDecentralizedEffect extends Component<Props> {
  constructor(props: Props) {
    super(props);
    this.container = createRef();
  }

  container: any | null | undefined;
  fireworks: Record<string, any> | null | undefined = null;

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
    const { className } = this.props;
    const componentStyles = classnames([styles.component, className]);
    return <div className={componentStyles} ref={this.container} />;
  }
}

export default FullyDecentralizedEffect;
