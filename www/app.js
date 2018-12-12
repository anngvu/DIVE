// initialize an introjs instance
var intro = introJs();

function userGetsDrill() { intro.goToStepNumber(2) }
function userGetsMatches() { setTimeout(function() { intro.goToStepNumber(5) }, 500); }

// handler
Shiny.addCustomMessageHandler("startGuideC",
  function(message) {
    intro.setOptions({steps: message.steps, 'showStepNumbers': false }).start()
    .onchange(function(targetElement) {
      if(targetElement.id === "corM") { $("#drilldown").on("shiny:value", userGetsDrill)}  })
    .goToStep(1)
    .onexit( function() { $("#drilldown").off("shiny:value", userGetsDrill) });
});

Shiny.addCustomMessageHandler("startGuideM",
  function(message) {
    intro.setOptions({steps: message.steps, 'showStepNumbers': false }).start()
    .onbeforechange(function(targetElement) {

      switch(targetElement.id) {
        // load demo dataset before selecting type of nPOD matches
        case "nPODInput":
          Shiny.setInputValue("cohortName", "ExampleCohort2020");
          break;
        // set listener for "Match" before parameters are shown
        case "matchUI":
          $("#matchResult").on("shiny:outputinvalidated", userGetsMatches);
          break;
        case "matchResult":
          if(this._currentStep === 4) { // if no user trigger, simulate "Match" click before proceeding
            $("#match").trigger("click");
          }
          if(this._currentStep === 5) {
            $("#matchResult").off("shiny:outputinvalidated", userGetsMatches);
            $(".nav-tabs a[data-value='match-extra']").tab('show');

        }
          break;
      }
    })
    // remove listeners when the demo context ends
    .onexit( function() {
      $("#matchResult").off("shiny:outputinvalidated", userGetsMatches);
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


