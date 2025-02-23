const s = new WebSocket("ws://localhost:8008/game");

export const onReady = ({ app, env }) => {
  if (app.ports) {
    app.ports.sendMessage.subscribe((data) => {
      s.send(JSON.stringify(data));
    });
  };


  s.addEventListener("message", (event) => {
    const d = JSON.parse(event.data);
    try {
      app.ports.receiveMessage.send(d);
    } catch (e) {
      console.log("Error sending", e);
    }
  });
};
