var s = new WebSocket("ws://127.0.0.1:8008");

export const onReady = ({ app, env }) => {
  if (app.ports && app.ports.clientMessage) {
    app.ports.clientMessage.subscribe(({ data }) => {
    });
};
