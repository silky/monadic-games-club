const s = new WebSocket("ws://localhost:8008/game");

s.addEventListener("open", (event) => {
  console.log("Connected!");
});

s.addEventListener("message", (event) => {
  console.log("Received: ", event.data);
  app.ports.receiveMessage.send(event.data);
});

export const onReady = ({ app, env }) => {
  console.log("Ready!");
  if (app.ports) {
    console.log("Subscribed!");
    app.ports.sendMessage.subscribe((data) => {
      console.log("Sending: ", JSON.stringify(data));
      s.send(JSON.stringify(data));
    });
  };
};
