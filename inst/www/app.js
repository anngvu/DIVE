var intro = introJs();

// handler demoCorrelation
Shiny.addCustomMessageHandler("demoCorrelation",
  function(message) {
    var userDoesDrill = function(event) {
          if(event.target.id === 'cor-matrix-scatter') { intro.nextStep() }
    };
    $(document).on('shiny:value', userDoesDrill);
    intro.setOptions({
      steps: message.steps,
      'showStepNumbers': false,
      'hidePrev': true,
      'hideNext': true,
      'showBullets': false,
      'highlightClass': 'custom-highlight',
      'doneLabel': 'Upload demo dataset'
    })
    .start()
    .onafterchange(function(targetElement) {
        if(this._currentStep === 1) { $(document).off('shiny:value', userDoesDrill) }
    })
    .oncomplete(function() {
      Shiny.setInputValue("cor-upload-appdata", "pilot.csv" );
    })
    .onexit(function() { $(document).off('shiny:value', userDoesDrill) });
});

// handler demoCohortExchange
Shiny.addCustomMessageHandler("demoCohortExchange",
  function(message) {
    $(".nav-tabs a[data-value='Reference cohort graph']").tab('show');
    var firststeps = message.steps.slice(0,2);
    intro.setOptions({
      steps: firststeps,
      'showStepNumbers': false,
      'hidePrev': true,
      'hideNext': true,
      'showBullets': false,
      'highlightClass': 'custom-highlight',
      'doneLabel': 'Upload demo dataset' })
    .start()
    .oncomplete( function() {
      Shiny.setInputValue("match-CohortX-upload-appdata", "examplecohort2020.csv");
      setTimeout(function() { introCX2(message) }, 250);
    });
});

function introCX2(message) {
  $(".nav-tabs a[data-value='Match parameters']").tab('show');
  var nextsteps = message.steps.slice(2,4);
  intro.setOptions({
    steps: nextsteps,
    'doneLabel': 'Try exploring data <strong>without</strong> matching'
  })
  .start()
  .oncomplete( function() {
    $(".nav-tabs a[data-value='Explore']").tab('show');
    setTimeout(function() { introCX3(message) }, 250);
  });
}

function introCX3(message) {
  var nextsteps = message.steps.slice(4,5);
  intro.setOptions({
    steps: nextsteps,
    'doneLabel': 'OK, now proceed with getting matches'
  })
  .start()
  .oncomplete( function() {
    $(".nav-tabs a[data-value='Match parameters']").tab('show');
    setTimeout(function() { introCX4(message) }, 750);
  });
}

// need to listen for run-params by user
function introCX4(message) {
  var userInitiatesRun = function(event) {
    if(event.target.id === 'match-results-table') {
      $(".nav-tabs a[data-value='Match results']").tab('show');
      setTimeout(function() { introCX5(message) }, 250);
    }
  };
  $(document).on('shiny:value', userInitiatesRun);
  var nextsteps = message.steps.slice(5,6);
  intro.setOptions({
    steps: nextsteps,
    'doneLabel': 'OK'
  })
  .start()
  .oncomplete(function() {
    setTimeout(function() { introCX5(message) }, 250);
  })
  .onexit(function() {
    $(document).off('shiny:value', userInitiatesRun);
  });
}

function introCX5(message) {
  var nextsteps = message.steps.slice(6,7);
  intro.setOptions({
    steps: nextsteps,
    'doneLabel': 'Try exploring data <strong>after</strong> matching',
  })
  .start()
  .oncomplete( function() {
    $(".nav-tabs a[data-value='Explore']").tab('show');
    setTimeout(function() { introCX6(message) }, 250);
  });
}

function introCX6(message) {
  var nextsteps = message.steps.slice(7,9);
  intro.setOptions({
    steps: nextsteps,
    'doneLabel': 'Done with demo'
  })
  .start()
  .oncomplete( function() {
    Shiny.setInputValue("match-CohortX-name" + "", "");
  });
}


