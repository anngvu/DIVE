// initialize an introjs instance
var intro = introJs();

function userGetsDrill() { intro.goToStepNumber(2) }

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
    $(".nav-tabs a[data-value='Reference cohort graph']").tab('show');
    var firststeps = message.steps.slice(0,2);
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
  $(".nav-tabs a[data-value='Match parameters']").tab('show');
  var nextsteps = message.steps.slice(2,4);
  intro.setOptions({
    steps: nextsteps,
    'showStepNumbers': false,
    'doneLabel': 'Try exploring data <strong>without</strong> matching',
    'hideNext': true,
    'hidePrev': true,
    'showBullets': false
  })
  .start()
  .oncomplete( function() {
    $(".nav-tabs a[data-value='Explore']").tab('show');
    setTimeout(function() { introMatch3(message) }, 250);
  });
}

function introMatch3(message) {
  var nextsteps = message.steps.slice(4,5);
  intro.setOptions({
    steps: nextsteps,
    'showStepNumbers': false,
    'doneLabel': 'OK, now proceed with getting matches',
    'showBullets': false
  })
  .start()
  .oncomplete( function() {
    $(".nav-tabs a[data-value='Match parameters']").tab('show');
    setTimeout(function() { introMatch4(message) }, 400);
  });
}

// need to listen for run-params by user
function introMatch4(message) {
  var userInitiatesRun = function(event) {
    if(event.target.id === 'match-results-table') {
      setTimeout(function() { introMatch5(message) }, 250);
    }
  };
  $(document).on('shiny:value', userInitiatesRun);
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
    setTimeout(function() { introMatch5(message) }, 250);
  })
  .onexit(function() {
    $(document).off('shiny:value', userInitiatesRun);
  });
}

function introMatch5(message) {
  nextsteps = message.steps.slice(6,7);
  intro.setOptions({
    steps: nextsteps,
    'showStepNumbers': false,
    'doneLabel': 'Try exploring data <strong>after</strong> matching',
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
  .start()
  .oncomplete( function() {
    Shiny.setInputValue("match-CohortX-name" + "", "");
  });
}



Shiny.addCustomMessageHandler("startGuideV",
  function(message) {
    intro.setOptions({steps: message.steps }).start()
    .onchange(function(targetElement) {
      if(targetElement.id === "corM") { $("#drilldown").on("shiny:value", userDrill) }  })
    .goToStep(1)
    .onexit( function() { $("#drilldown").off("shiny:value", userDrill) });
});


