export default (variables) => {
  Object.entries(variables).map(([prop, value]) =>
    document.documentElement.style.setProperty(prop, value)
  );
};
