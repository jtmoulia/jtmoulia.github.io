const path = require('path');
const ExtractTextPlugin = require("extract-text-webpack-plugin");

// const JSLoader = {
//   test: /\.js$/,
//   exclude: /node_modules/,
//   use: {
//     loader: 'babel-loader',
//     options: {
//       presets: ['env']
//     }
//   }
// };

module.exports = {
  entry: './src/index.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'js/bundle.js'
  },
  module: {
    rules: [{
      test: /\.scss$/,
      use: ExtractTextPlugin.extract({
        fallback: 'style-loader',
        use: [
          'css-loader',
          'sass-loader'
        ]
      })
    }]
  },
  plugins: [
    new ExtractTextPlugin('css/mystyles.css'),
  ]
};
