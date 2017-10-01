const Elm = require('./Main');
const elmDiv = document.getElementById('elm-area');
const app = Elm.Main.embed(elmDiv, {
  apiUrl: process.env.API_URL,
});

export function onSignIn(googleUser) {
  var profile = googleUser.getBasicProfile();

  app.ports.authenticated.send({
    name: profile.getName(),
    image_url: profile.getImageUrl(),
    id_token: googleUser.getAuthResponse().id_token
  });

  console.log("Received Google user.");
};
