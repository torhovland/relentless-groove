const Elm = require('./Main');
const app = Elm.Main.embed(document.getElementById('elm-area'));

export function onSignIn(googleUser) {
  var profile = googleUser.getBasicProfile();

  app.ports.authenticated.send({
    name: profile.getName(),
    image_url: profile.getImageUrl(),
    id_token: googleUser.getAuthResponse().id_token
  });

  console.log("Received Google user.");
};
