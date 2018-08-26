const path = require("path");
const webpack = require("webpack");
const ManifestPlugin = require("webpack-manifest-plugin");
const ExtractTextPlugin = require("extract-text-webpack-plugin");

const common_chunk = "common";

const plugins = [
  new ExtractTextPlugin({
    filename: "[name].[chunkhash].[contenthash].css"
  }),
  new ManifestPlugin()
];

module.exports = {
  entry: path.resolve(__dirname, "css/default.css"),
  module: {
    rules: [
      {
        test: /\.css$/,
        use: ExtractTextPlugin.extract({
          use: [
            { loader: "css-loader", options: { importLoaders: 0 } },
            "postcss-loader"
          ]
        })
      },
      {
        test: /\.woff2?$|\.ttf$|\.otf$|\.svg$/,
        use: [
          {
            loader: "file-loader"
          }
        ]
      }
    ]
  },
  output: {
    path: path.join(__dirname, "out"),
    filename: "[name].[chunkhash].js"
  },
  plugins: plugins
};
