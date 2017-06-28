express = require 'express'

app = express()
exec = require('child_process').exec;
app.get '/pot', (req, res) ->
  exec 'luna-empire-invoker setValue "f5a44e7f-b023-4d8f-82f3-10500c00e347" 0 "77962bb1-32f1-4f4d-ae5f-4328b7c83708" 0 ' + req.query.value
  res.send('ok')

app.get '/temp2', (req, res) ->
  console.log(req.query.value)
  exec 'luna-empire-invoker setValue "f5a44e7f-b023-4d8f-82f3-10500c00e347" 0 "1a9eeda0-4627-4597-9d09-167c44cbe87e" 0 ' + req.query.value
  res.send('ok')

app.get '/temp1', (req, res) ->
  console.log(req.query.value)
  exec 'luna-empire-invoker setValue "f5a44e7f-b023-4d8f-82f3-10500c00e347" 0 "1a9eeda0-4627-4597-9d09-167c44cbe87f" 0 ' + req.query.value
  res.send('ok')

app.listen 6666
