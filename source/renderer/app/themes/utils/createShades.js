'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.createErrorShades = exports.createBackgroundShades = void 0;
const chroma_js_1 = __importDefault(require('chroma-js'));
const createBackgroundShades = (color) => {
  const lightShades = chroma_js_1.default.scale([color, '#fff']);
  const darkShades = chroma_js_1.default.scale([color, '#000']);
  return {
    lightest: lightShades(0.6).hex(),
    ultralighter: lightShades(0.5).hex(),
    lighter: lightShades(0.4).hex(),
    ultralight: lightShades(0.3).hex(),
    light: lightShades(0.2).hex(),
    regular: color,
    dark: darkShades(0.1).hex(),
    darker: darkShades(0.2).hex(),
    darkest: darkShades(0.3).hex(),
  };
};
exports.createBackgroundShades = createBackgroundShades;
const createErrorShades = (color) => {
  const lightShades = chroma_js_1.default.scale([color, '#fff']);
  const darkShades = chroma_js_1.default.scale([color, '#000']);
  return {
    ultralight: lightShades(0.8).hex(),
    lightest: lightShades(0.6).hex(),
    lighter: lightShades(0.4).hex(),
    light: lightShades(0.2).hex(),
    regular: color,
    dark: darkShades(0.1).hex(),
    darker: darkShades(0.2).hex(),
    darkest: darkShades(0.3).hex(),
  };
};
exports.createErrorShades = createErrorShades;
//# sourceMappingURL=createShades.js.map
