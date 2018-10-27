// initialize an introjs instance          
var intro = introJs();

function userDemoDrill(event) { intro.goToStepNumber(2) }
function userDemoMatch(event) { intro.goToStepNumber(5) }

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
        // set listener for "Match" before parameters are shown
        case "matchParameters":
          $("#match").on("shiny:value", userDemoMatch);
          break;
        // Switch to other tab
        case "matchResult":
          
          break;
        case "matchAttributes":
          
          break;
        case "matchPlot":
          
          break;
      }
    })
    .goToStep(1)
    // remove "Match" listener when the demo context ends
    .onexit( function() {  
      $("#match").on("shiny:value", userDemoMatch);
    });
});

Shiny.addCustomMessageHandler("startGuideV",
  function(message) {
    intro.setOptions({steps: message.steps }).start()
    .onchange(function(targetElement) { 
      if(targetElement.id === "corM") { $("#drilldown").on("shiny:value", userDrill) }  })
    .goToStep(1)
    .onexit( function() { $("#drilldown").off("shiny:value", userDrill) });
});


