$(document).ready(function() {
  var survey;

  function initializeSurvey(surveyJSON) {
    try {
      if (typeof surveyJSON === 'string') {
        surveyJSON = JSON.parse(surveyJSON);
      }

      // Update document title if available
      if (surveyJSON.title) {
        document.title = surveyJSON.title;
      }

      $("#surveyContainer").empty();

      // Create the survey
      survey = new Survey.Model(surveyJSON);

      survey.onComplete.add(function(result) {
        Shiny.setInputValue("surveyData", JSON.stringify(result.data));
        Shiny.setInputValue("surveyComplete", true);
      });

      // Initialize jQuery Survey
      $("#surveyContainer").Survey({ model: survey });

    } catch (error) {
      console.error("Error initializing survey:", error);
      console.error(error.stack);
    }
  }

  Shiny.addCustomMessageHandler("loadSurvey", function(surveyJSON) {
    initializeSurvey(surveyJSON);
  });
});
