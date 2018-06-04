// @flow
export const mockNextAdaUpdate = async () => (
  new Promise(resolve => setTimeout(() => resolve({ version: 50 }), 1000))
);
