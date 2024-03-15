// @flow

// the baseStyles obj will be composed with the stylesToAdd obj
// activeClasses is an array of the classes within stylesToAdd
// that should be added to baseStyles
export const composeBaseStyles = (
  baseStyles: Object,
  stylesToAdd: Object = {},
  activeClasses: Array<string>
) => {
  if (!activeClasses || !activeClasses.length) {
    return baseStyles;
  }
  const composedBase = { ...baseStyles };

  activeClasses.forEach((className) => {
    if (
      Object.hasOwnProperty.call(stylesToAdd, className) &&
      stylesToAdd[className]
    ) {
      composedBase.base += ` ${stylesToAdd[className]}`;
    }
  });

  return composedBase;
};

export const formatTemplateAreas = (areas: Array<string>) => {
  if (!areas || !areas.length) {
    return;
  }

  return areas.reduce((template, row, index) => {
    if (!index) {
      return `'${row}'`;
    }
    return `${template} '${row}'`;
  }, '');
};
