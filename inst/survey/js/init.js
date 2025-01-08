function initializeSurvey(data) {
   try {
       if (!data) {
           throw new Error("No survey data provided");
       }

       console.log("Survey data received:", data);

       // Clear existing survey if it exists
       if (survey) {
           try {
               survey.dispose();
           } catch (e) {
               console.warn("Error disposing existing survey:", e);
           }
           survey = null;
       }

       // Clear survey container
       $("#surveyContainer").empty();

       let surveyJSON;
       if (typeof data === "object") {
           surveyJSON = data.survey || data;
           if (data.params) {
               storedParams = data.params;
               console.log("Stored params:", storedParams);
           }
       } else if (typeof data === "string") {
           try {
               surveyJSON = JSON.parse(data);
           } catch (error) {
               throw new Error("Invalid survey JSON format: " + error.message);
           }
       } else {
           throw new Error("Invalid survey data format");
       }

       survey = new Survey.Model(surveyJSON);

       const savedData = getCookie(COOKIE_NAME);
       if (savedData) {
           survey.data = savedData;
       }

       setHiddenFieldsFromShiny(survey, storedParams);

       let valueChangeTimeout;
       survey.onValueChanged.add((sender, options) => {
           console.log("Value changed:", options.name, options.value);

           clearTimeout(valueChangeTimeout);
           valueChangeTimeout = setTimeout(() => {
               saveSurveyProgress(survey);
               if (typeof survey.runConditions === "function") {
                   survey.runConditions();
               }
           }, DEBOUNCE_DELAY);
       });

       // Add completion handler
       survey.onComplete.add((result) => {
           try {
               // 1. Immediately show saving message and trigger completion state
               document.getElementById("savingDataMessage").style.display = "block";
               Shiny.setInputValue("surveyComplete", true);

               // 2. Create a promise chain for the remaining operations
               Promise.resolve()
                   .then(() => {
                       // Prepare the data
                       const responses = {};
                       for (const [key, value] of Object.entries(result.data)) {
                           if (!["data", "currentPageNo", "timestamp"].includes(key)) {
                               const question = survey.getQuestionByName(key);
                               responses[key] = question?._internalValue ?? value;
                           }
                       }
                       return responses;
                   })
                   .then((responses) => {
                       // Send the data
                       Shiny.setInputValue("surveyData", responses);
                   })
                   .then(() => {
                       // Handle cookie cleanup
                       deleteCookie(COOKIE_NAME);
                       return new Promise(resolve => {
                           setTimeout(() => {
                               const remainingCookie = getCookie(COOKIE_NAME);
                               if (remainingCookie) {
                                   deleteCookie(COOKIE_NAME);
                               }
                               resolve();
                           }, 100);
                       });
                   })
                   .catch(error => {
                       console.error("Error in completion handler:", error);
                       Shiny.setInputValue("surveyError", {
                           type: "CompletionError",
                           message: "Error processing survey completion",
                           details: error.message
                       });
                   });
                   console.log("Processed data");
           } catch (error) {
               console.error("Error in completion handler:", error);
               Shiny.setInputValue("surveyError", {
                   type: "CompletionError",
                   message: "Error processing survey completion",
                   details: error.message
               });
           }
       });

       $("#surveyContainer").Survey({
           model: survey,
           onAfterRenderSurvey: () => {
               console.log("Loaded survey");
           }
       });

   } catch (error) {
       console.error("Survey initialization error:", error);
       isInitializing = false;
       Shiny.setInputValue("surveyError", {
           type: "InitializationError",
           message: "Failed to initialize survey",
           details: error.message
       });
   }
}
