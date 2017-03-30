import { PropTypes } from 'react';
import AppStore from './stores/AppStore';

export const oneOrManyChildElements = PropTypes.oneOfType([
  PropTypes.arrayOf(PropTypes.element),
  PropTypes.element,
]);

export const storesPropType = PropTypes.shape({
  app: PropTypes.instanceOf(AppStore),
});

export const translationMessageParams = PropTypes.shape({
  id: PropTypes.string.isRequired,
  defaultMessage: PropTypes.string.isRequired,
  description: PropTypes.string,
});
