const fs = require('fs')
const path = require('path')

module.exports = function rimraf(dir) {
  if (fs.existsSync(dir)) {
    fs.readdirSync(dir).forEach((file) => {
      const filePath = path.join(dir, file)
      if (fs.lstatSync(filePath).isDirectory()) {
        rimraf(filePath)
      } else {
        fs.unlinkSync(filePath)
      }
    })
    
    fs.rmdirSync(dir)
  }
}