export const submitOnEnter = (action, ...args) => {
  const event = args.pop();
  // eslint-disable-next-line
  event.persist && event.persist();
  // eslint-disable-next-line
  event.key === 'Enter' && action.apply(this, args);
};
