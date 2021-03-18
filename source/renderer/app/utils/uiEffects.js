// @flow
import canvasConfetti from 'canvas-confetti';
import { randomInRange } from './numbers';

type confettiSettings = {
  duration: number,
  startVelocity: number,
  spread: number,
  ticks: number,
  zIndex: number,
};
const CONFETTI_DEFAULT_SETTINGS: confettiSettings = {
  duration: 15 * 1000,
  startVelocity: 30,
  spread: 360,
  ticks: 60,
  zIndex: 0,
};
export const confetti = (customSettings: $Shape<confettiSettings>) => {
  const { duration, ...settings } = {
    ...CONFETTI_DEFAULT_SETTINGS,
    ...customSettings,
  };
  const animationEnd = Date.now() + duration;

  const interval: IntervalID = setInterval(function () {
    const timeLeft = animationEnd - Date.now();

    if (timeLeft <= 0) {
      return clearInterval(interval);
    }

    const particleCount = 50 * (timeLeft / duration);
    // since particles fall down, start a bit higher than random

    canvasConfetti({
      ...settings,
      particleCount,
      origin: { x: randomInRange(0.1, 0.3), y: Math.random() - 0.2 },
    });
    canvasConfetti({
      ...settings,
      particleCount,
      origin: { x: randomInRange(0.7, 0.9), y: Math.random() - 0.2 },
    });
  }, 250);
};
