// initialize an introjs instance
var intro = introJs();

function userGetsDrill() { intro.goToStepNumber(2) }
function userMsg() { alert("GRR") }

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
    firststeps = message.steps.slice(0,2);
    intro.setOptions({
      steps: firststeps,
      'showStepNumbers': false,
      'hideNext': true,
      'doneLabel': 'Load demo data',
      'showBullets': false })
    .start()
    .oncomplete( function() {
      Shiny.setInputValue("match-CohortX-name" + "", "examplecohort2020");
      setTimeout(function() { introMatchLoadData(message) }, 250);
    });
});

function introMatchLoadData(message) {
  nextsteps = message.steps.slice(2,4);
  intro.setOptions({
    steps: nextsteps,
    'showStepNumbers': false,
    'doneLabel': 'Explore data without matching',
    'hideNext': true,
    'showBullets': false
  })
  .start()
  .oncomplete( function() {
    $(".nav-tabs a[data-value='Explore']").tab('show');
    setTimeout(function() { introMatch3(message) }, 250);
  });
}

function introMatch3(message) {
  nextsteps = message.steps.slice(4,5);
  intro.setOptions({
    steps: nextsteps,
    'showStepNumbers': false,
    'doneLabel': 'Go back to get matched data',
    'showBullets': false
  })
  .start()
  .oncomplete( function() {
    $(".nav-tabs a[data-value='Match parameters']").tab('show');
    setTimeout(function() { introMatch4(message) }, 250);
  });
}

function introMatch4(message) {
  nextsteps = message.steps.slice(5,6);
  intro.setOptions({
    steps: nextsteps,
    'showStepNumbers': false,
    'hideNext': true,
    'doneLabel': 'OK',
    'showBullets': false
  })
  .start()
  .oncomplete(function() {
    setTimeout(function() { introMatch5(message) }, 300);
  });
}

function introMatch5(message) {
  nextsteps = message.steps.slice(6,7);
  intro.setOptions({
    steps: nextsteps,
    'showStepNumbers': false,
    'doneLabel': 'Explore data after matching',
    'showBullets': false
  })
  .start()
  .oncomplete( function() {
    $(".nav-tabs a[data-value='Explore']").tab('show');
    setTimeout(function() { introMatch6(message) }, 250);
  });
}

function introMatch6(message) {
  nextsteps = message.steps.slice(7,9);
  introJs().setOptions({
    steps: nextsteps,
    'showStepNumbers': false,
    'showBullets': false
  })
  .start();
}



Shiny.addCustomMessageHandler("startGuideV",
  function(message) {
    intro.setOptions({steps: message.steps }).start()
    .onchange(function(targetElement) {
      if(targetElement.id === "corM") { $("#drilldown").on("shiny:value", userDrill) }  })
    .goToStep(1)
    .onexit( function() { $("#drilldown").off("shiny:value", userDrill) });
});


