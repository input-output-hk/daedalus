const path = require("path");
const webpack = require("webpack");
const DllPlugin = require("webpack/lib/DllPlugin");

module.exports = {
  entry: {
    vendor: [path.join(__dirname, "../dll", "vendor.js")]
  },
  output: {
    path: path.join(__dirname, "../dll"),
    filename: "dll.[name].js",
    library: "[name]"
  },
  plugins: [
    new DllPlugin({
      path: path.join(__dirname, "../dll", "[name]-manifest.json"),
      name: "[name]",
      context: path.resolve(__dirname, "../app")
    }),
    new webpack.optimize.OccurenceOrderPlugin(),
  ],
  resolve: {
    root: path.resolve(__dirname, "../app"),
    modulesDirectories: ["node_modules"]
  }
};
