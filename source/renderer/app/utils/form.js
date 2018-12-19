export const submitOnEnter = (action, ...args) => {
  const event = args.pop();
  event.persist && event.persist();
  event.key === 'Enter' && action.apply(this, args);
};
