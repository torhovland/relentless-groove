const Elm = require('./Main');
const elmDiv = document.getElementById('elm-area');
const app = Elm.Main.embed(elmDiv, process.env.API_URL);

export function onSignIn(googleUser) {
  var profile = googleUser.getBasicProfile();

  app.ports.authenticated.send({
    name: profile.getName(),
    imageUrl: profile.getImageUrl(),
    idToken: googleUser.getAuthResponse().id_token
  });

  console.log("Received Google user.");
};
