const path = require("path");
const webpack = require("webpack");
const ManifestPlugin = require("webpack-manifest-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");


const common_chunk = "common";

const plugins = [
  new MiniCssExtractPlugin({
    filename: "[name].[chunkhash].[contenthash].css"
  }),
  new ManifestPlugin()
];

module.exports = {
  entry: [
    path.resolve(__dirname, "css/default.css"),
    path.resolve(__dirname, "js/default.js"),
  ],
  module: {
    rules: [
      {
        test: /\.css$/,
        use: [
          MiniCssExtractPlugin.loader,
            { loader: "css-loader", options: { importLoaders: 0 } },
            "postcss-loader"
          ]
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
    filename: "[name].[chunkhash].js",
    libraryTarget: 'var',
    library: 'EntryPoint'
  },
  plugins: plugins
};
