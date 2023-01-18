import addons from '@storybook/addons';

const channel = addons.getChannel();
export const setInitialState = (initialState: Record<string, any>) =>
  Object.entries(initialState).forEach(([param, value]) =>
    updateParam({
      param,
      value,
    })
  );

channel.on('daedalusMenu/updateParam', (query) => {
  channel.emit('daedalusMenu/paramUpdated', query);
});

export const updateParam = (query: Record<string, any>) =>
  channel.emit('daedalusMenu/updateParam', query);

export const onReceiveParam = (cb: (...args: Array<any>) => any) => {
  const eventHandler = (query) => cb(query);
  channel.on('daedalusMenu/updateParam', eventHandler);
  return () => channel.off('daedalusMenu/updateParam', eventHandler);
};
