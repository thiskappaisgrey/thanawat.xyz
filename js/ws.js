// this took me forever to figure out but, use ws for local connection
// and wss for production (for the ssl certificate)
var socket = new WebSocket("ws://localhost:3000")

socket.onopen = function (event) {
  socket.send("Browser client connected");
  console.log("Browser connected")
};
socket.onmessage = function(event) {
  console.log(event.data)
  window.location.reload()
}
