const path = require('path');

module.exports = {
    entry: './src/index.ts',
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
            }
        ]
    },
    output: {
        filename: 'main.js',
        path: path.resolve(__dirname, 'dist'),
    },
    devServer: {
        contentBase: path.join(__dirname, 'dist'),
        compress: true,
        port: 8080
    },
    resolve: {
        alias: {
            'DevTools': path.resolve(__dirname, 'src/DevTools.elm')
        }
    }
};
