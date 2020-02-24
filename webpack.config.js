const path = require("path");

module.exports = {
  entry: "./src/index.ts",
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: ["elm-hot-webpack-loader", "elm-webpack-loader"]
      },
      {
        test: /\.tsx?$/,
        use: "ts-loader"
      }
    ]
  },
  output: {
    filename: "main.js",
    path: path.resolve(__dirname, "dist")
  },
  devServer: {
    contentBase: path.join(__dirname, "dist"),
    compress: true,
    port: 8080,
    proxy: {
      "/_api/ws": {
        target: "ws://localhost:4666",
        ws: true
      },
      "/": "http://localhost:4666",
      "index.html": "http://localhost:4666",
      "index.htm": "http://localhost:4666",
      "/_api": "http://localhost:4666"
    }
  },
  resolve: {
    extensions: [".ts", ".js"],
    alias: {
      LanternShell: path.resolve(__dirname, "src/LanternShell.elm")
    }
  }
};
