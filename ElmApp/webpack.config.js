const path = require('path');

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
  devServer: {
    contentBase: './public'
  },
  devtool: "source-map"
}