const webpack = require('webpack');
const path = require('path');
require('dotenv').config();

module.exports = {
  entry: [
    './src/index.js'
  ],
  module: {
    rules: [
      {
        test: /\.elm$/,
        use: 'elm-webpack-loader',
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