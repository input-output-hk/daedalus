exports.__esModule = true;
exports.removeCharAtPosition = void 0;
const removeCharAtPosition = function (str, pos) {
  return str.substring(0, pos) + str.substring(pos + 1, str.length);
};
exports.removeCharAtPosition = removeCharAtPosition;
