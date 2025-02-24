const s = new WebSocket("ws://localhost:8008/game");

export const onReady = ({ app, env }) => {
  if (app.ports) {
    app.ports.sendMessage.subscribe((data) => {
      const m = JSON.stringify(data);
      console.log("Sent");
      console.log(data);
      s.send(m);
    });
  };


  s.addEventListener("message", (event) => {
    const d = JSON.parse(event.data);
    try {
      console.log("Response")
      console.log(d);
      app.ports.receiveMessage.send(d);
    } catch (e) {
      console.log("Error sending", e);
    }
  });
};
