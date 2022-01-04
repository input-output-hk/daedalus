export type FilterType = {
  name?: string;
  extensions: string[];
};
export type FileDialogRequestParams = {
  name?: string;
  title?: string;
  defaultPath?: string;
  buttonLabel?: string;
  filters?: Array<FilterType>;
  properties?: Array<string>;
  message?: string;
  nameFieldLabel?: string;
  showsTagField?: boolean;
  securityScopedBookmarks?: boolean;
};
export type OpenFileDialogResponseParams = {
  canceled: boolean;
  filePaths: string[];
  bookmarks?: string[];
};
export type SaveFileDialogResponseParams = {
  canceled: boolean;
  filePath?: string;
  bookmark?: string;
};
