// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { confetti } from '../../utils/uiEffects';
import { Fireworks } from 'fireworks-js';

type Props = {
  isActive: boolean,
  layout?: string,
  effect?: 'fireworks' | 'confetti',
  containerSelector: string,
};

type State = {
  fireworks: ?Object,
};

@observer
export default class FullyDecentralizedEffect extends Component<Props, State> {
  state = {
    fireworks: null,
  };

  componentDidMount() {
    const { containerSelector, isActive } = this.props;
    const container = document.querySelector(containerSelector);
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
      this.setState({
        fireworks: fireworks,
      });
      fireworks.start();
    }
  }

  // changeEffectActive = (isActive: boolean) => {
  //   // const { }
  // };

  render() {
    const { isActive, effect } = this.props;
    window.Fireworks = Fireworks;
    window.confetti = confetti;
    return <div />;
  }
}
