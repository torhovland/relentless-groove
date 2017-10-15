const webpack = require('webpack');
const path = require('path');
const webpackIf = require('webpack-if');
const ifProd = webpackIf.ifElse(process.env.NODE_ENV === 'production');
require('dotenv').config();

module.exports = {
  entry: [
    './src/index.js'
  ],
  module: {
    rules: [
      {
        test: /\.elm$/,
        use: [{
          loader: 'elm-webpack-loader',
          options: {
              verbose: true,
              warn: true,
              debug: ifProd(false, true)
          }
        }],
        exclude: [/elm-stuff/, /node_modules/]
      }
    ]
  },
  resolve: {
    extensions: [".elm", ".js"]
  },
  output: {
    path: path.resolve(__dirname, 'public/dist'),
    publicPath: "/dist/",
    filename: 'bundle.js',
    library: 'EntryPoint'    
  },
  plugins: [
    new webpack.EnvironmentPlugin(["API_URL"])
  ],
  devServer: {
    contentBase: './public'
  },
  devtool: "source-map"
}