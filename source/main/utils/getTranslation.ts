export const getTranslation = (translations: {}, id: string) => (key: string) =>
  translations[`${id}.${key}`];
