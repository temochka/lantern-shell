const path = require("path");
const merge = require("webpack-merge");
const common = require("./webpack.common.js");

module.exports = merge(common, {
  mode: "development",
  devtool: "inline-source-map",
  devServer: {
    index: "",
    contentBase: path.join(__dirname, "dist"),
    compress: true,
    port: 8080,
    proxy: {
      "/_api/ws": {
        target: "ws://localhost:4666",
        ws: true
      },
      "/": "http://localhost:4666",
      "/index.html": "http://localhost:4666",
      "/index.htm": "http://localhost:4666",
      "/_api": "http://localhost:4666"
    }
  }
});
