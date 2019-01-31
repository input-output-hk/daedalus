// @flow
export const stringifyData = (data: any) => JSON.stringify(data, null, 2);

export const stringifyError = (error: any) => (
  JSON.stringify(error, Object.getOwnPropertyNames(error), 2)
);

export const formatMessage = (msg: Object) => {
  const [year, time] = msg.date.toISOString().split('T');
  const [context, body] = msg.data;

  console.log(`${context} [${year} ${time.slice(0, -1)} UTC] ${body}`);
};
