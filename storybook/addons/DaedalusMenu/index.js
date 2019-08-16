// @flow
import addons from '@storybook/addons';

const channel = addons.getChannel();

export const setInitialState = (initialState: Object) =>
  Object.entries(initialState).forEach(([param, value]) =>
    updateParam({ param, value })
  );

channel.on('daedalusMenu/updateParam', query => {
  channel.emit('daedalusMenu/paramUpdated', query);
});

export const updateParam = (query: Object) =>
  channel.emit('daedalusMenu/updateParam', query);

export const onReceiveParam = (cb: Function) =>
  channel.on('daedalusMenu/updateParam', query => {
    cb(query);
  });
