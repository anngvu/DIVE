// initialize an introjs instance          
var intro = introJs();

function userDemoDrill(event) { intro.goToStepNumber(2) }

// handler
Shiny.addCustomMessageHandler("startGuideC",
  function(message) {
    intro.setOptions({steps: message.steps }).start()
    .onchange(function(targetElement) { 
      if(targetElement.id === "corM") { $("#drilldown").on("shiny:value", userDrill) }  })
    .goToStep(1)
    .onexit( function() { $("#drilldown").off("shiny:value", userDemoDrill) });
});

Shiny.addCustomMessageHandler("startGuideM",
  function(message) {
    intro.setOptions({steps: message.steps }).start()
    .onbeforechange(function(targetElement) { 
      switch(targetElement.id) {
        // load demo dataset before selecting type of nPOD matches
        case "nPODInput":
          Shiny.setInputValue("cohortName", "ExampleCohort2020");
          break;
        // listen for user to click on "Match"
        case "matchParameters": break;
        case "matchResult": break;
        case "matchAttributes": break;
        case "matchPlot": break;
      }
    })
    .goToStep(1)
    // remove "Match" listener when the demo context ends
    .onexit( function() {  });
});

Shiny.addCustomMessageHandler("startGuideV",
  function(message) {
    intro.setOptions({steps: message.steps }).start()
    .onchange(function(targetElement) { 
      if(targetElement.id === "corM") { $("#drilldown").on("shiny:value", userDrill) }  })
    .goToStep(1)
    .onexit( function() { $("#drilldown").off("shiny:value", userDrill) });
});


