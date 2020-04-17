// @flow
export type FileDialogRequestParams = {
  title?: string,
  defaultPath?: string,
  buttonLabel?: string,
  filters?: Array<any>,
  properties?: Array<string>,
  message?: string,
  nameFieldLabel?: string,
  showsTagField?: boolean,
  securityScopedBookmarks?: boolean,
};

export type OpenFileDialogResponseParams = {
  canceled: boolean,
  filePaths: string[],
  bookmarks?: string[],
};

export type SaveFileDialogResponseParams = {
  canceled: boolean,
  filePath?: string,
  bookmark?: string,
};
