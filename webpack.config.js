const path = require("path");
const webpack = require("webpack");
const ManifestPlugin = require("webpack-manifest-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const OptimizeCSSAssetsPlugin = require("optimize-css-assets-webpack-plugin");
const UglifyJsPlugin = require("uglifyjs-webpack-plugin");

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
  optimization: {
    minimizer: [
      new UglifyJsPlugin({
        cache: true,
        parallel: true,
        sourceMap: true // set to true if you want JS source maps
      }),
      new OptimizeCSSAssetsPlugin({})
    ]
  },
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
