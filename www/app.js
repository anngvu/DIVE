// initialize an introjs instance          
var intro = introJs();

function userDrill(event) { intro.goToStepNumber(2) }

// handler
Shiny.addCustomMessageHandler("startHelp",
  function(message) {
    intro.setOptions({steps: message.steps }).start()
    .onchange(function(targetElement) { 
      if(targetElement.id === "corM") { $("#drilldown").on("shiny:value", userDrill) }  })
    .goToStep(1)
    .onexit( function() { $("#drilldown").off("shiny:value", userDrill) });
});

