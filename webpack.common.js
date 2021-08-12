const path = require("path");
const CopyPlugin = require("copy-webpack-plugin");

module.exports = {
  entry: "./src/index.ts",
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: ["elm-hot-webpack-loader", "elm-webpack-loader"],
      },
      {
        test: /\.tsx?$/,
        use: "ts-loader",
      },
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
    ],
  },
  output: {
    filename: "main.js",
    path: path.resolve(__dirname, "dist", "public"),
  },
  plugins: [
    new CopyPlugin({
      patterns: [
        {
          from: path.resolve(__dirname, "index.html"),
          to: path.resolve(__dirname, "dist", "public"),
        },
      ],
    }),
  ],
  resolve: {
    extensions: [".ts", ".js"],
    alias: {
      LanternShell: path.resolve(__dirname, "src/LanternShell.elm"),
    },
  },
};
