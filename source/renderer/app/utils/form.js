export const submitOnEnter = (action, ...args) => {
  const event = args.pop();
  /* eslint-disable-next-line no-unused-expressions */
  event.persist && event.persist();
  /* eslint-disable-next-line no-unused-expressions */
  event.key === 'Enter' && action.apply(this, args);
};
