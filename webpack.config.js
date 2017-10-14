const path = require('path');
const webpack = require('webpack');
const ManifestPlugin = require('webpack-manifest-plugin');
const ExtractTextPlugin = require('extract-text-webpack-plugin')


const common_chunk = 'common';

const plugins = [
    new ExtractTextPlugin({
      filename: '[name].[chunkhash].css'
    }),
    new ManifestPlugin(),
];

module.exports = {
  entry: path.resolve(__dirname, "css/default.scss"),
  module: {
        rules: [{
            test: /\.scss$/,
            use: ExtractTextPlugin.extract({
              fallback: "style-loader",
              use: ["css-loader", "sass-loader"]
            })
        },
        {
              test: /\.woff2?$|\.ttf$|\.otf$|\.svg$/,
              use: [{
                      loader: "file-loader"
                    }]
        }]
  },
  output: {
    path: path.join(__dirname, "bundledOutputs"),
    filename: '[name].[chunkhash].js',
  },
  plugins: plugins,
};
