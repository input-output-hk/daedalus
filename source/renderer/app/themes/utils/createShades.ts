import chroma from 'chroma-js';
import type { BackgroundShades, ErrorShades } from '../types';

export const createBackgroundShades = (color: string): BackgroundShades => {
  const lightShades = chroma.scale([color, '#fff']);
  const darkShades = chroma.scale([color, '#000']);
  return {
    lightest: lightShades(0.6).hex(),
    ultralighter: lightShades(0.5).hex(),
    lighter: lightShades(0.4).hex(),
    ultralight: lightShades(0.3).hex(),
    light: lightShades(0.2).hex(),
    regular: color,
    dark: darkShades(0.1).hex(),
    darker: darkShades(0.2).hex(),
    darkest: darkShades(0.3).hex(),
  };
};
export const createErrorShades = (color: string): ErrorShades => {
  const lightShades = chroma.scale([color, '#fff']);
  const darkShades = chroma.scale([color, '#000']);
  return {
    ultralight: lightShades(0.8).hex(),
    lightest: lightShades(0.6).hex(),
    lighter: lightShades(0.4).hex(),
    light: lightShades(0.2).hex(),
    regular: color,
    dark: darkShades(0.1).hex(),
    darker: darkShades(0.2).hex(),
    darkest: darkShades(0.3).hex(),
  };
};
