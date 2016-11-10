import { PropTypes } from 'react';

export const oneOrManyChildElements = PropTypes.oneOfType([
  PropTypes.arrayOf(PropTypes.element),
  PropTypes.element
]);
