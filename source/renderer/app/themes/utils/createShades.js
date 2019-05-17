// @flow
import chroma from 'chroma-js';

export const createShades = (color: string) => {
  const lightShades = chroma.scale([color, '#fff']);
  const darkShades = chroma.scale([color, '#000']);

  return {
    lightest: lightShades(0.6),
    lighter: lightShades(0.4),
    light: lightShades(0.2),
    regular: color,
    dark: darkShades(0.1),
    darker: darkShades(0.2),
    darkest: darkShades(0.3),
  };
};
