export function authenticatedUser()
{
  const googleUser = window.GoogleUser;
  var profile = googleUser.getBasicProfile();

  return {
    name: profile.getName(),
    image_url: profile.getImageUrl(),
    id_token: googleUser.getAuthResponse().id_token
  }; 
}
