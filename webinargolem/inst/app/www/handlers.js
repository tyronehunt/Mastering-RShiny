$( document ).ready(function() {
  Shiny.addCustomMessageHandler('alert', function(message) {
  alert(message);
  });
});
