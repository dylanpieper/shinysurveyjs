function setHiddenFieldsFromShiny(survey, params) {
   if (!survey) {
       console.error("Invalid survey instance");
       return;
   }

   const paramsToUse = Object.keys(params || {}).length > 0 ? params : storedParams;
   console.log("Setting hidden fields with params:", paramsToUse);

   const setValueWithRetry = (paramName, paramData, retries = 3) => {
       const attempt = () => {
           try {
               const question = survey.getQuestionByName(paramName);
               if (question) {
                   // For HTML display, set the text as the value
                   if (paramData.text !== undefined) {
                       question.value = paramData.text;
                   } else {
                       question.value = paramData;
                   }
                   // Store the actual value separately for data handling
                   if (paramData.value !== undefined) {
                       question._internalValue = paramData.value;
                   }
               }
               return true;
           } catch (error) {
               console.warn(`Attempt to set ${paramName} failed:`, error);
               return false;
           }
       };

       const retryWithDelay = (retriesLeft) => {
           if (retriesLeft <= 0) {
               console.error(`Failed to set value for ${paramName} after all retries`);
               return;
           }
           setTimeout(() => {
               if (!attempt()) {
                   retryWithDelay(retriesLeft - 1);
               }
           }, DEBOUNCE_DELAY);
       };

       if (!attempt()) {
           retryWithDelay(retries - 1);
       }
   };

   try {
       Object.entries(paramsToUse).forEach(([paramName, paramValue]) => {
           if (!paramName) return;
           const valueToSet = paramValue?.value !== undefined ?
               { text: paramValue.text, value: paramValue.value } :
               paramValue;
           setValueWithRetry(paramName, valueToSet);
       });

       setTimeout(() => {
           if (typeof survey.runConditions === "function") {
               survey.runConditions();
           }
       }, CONDITION_DELAY);
   } catch (error) {
       console.error("Error in setHiddenFieldsFromShiny:", error);
       Shiny.setInputValue("surveyError", {
           type: "HiddenFieldsError",
           message: "Failed to set hidden fields",
           details: error.message
       });
   }
}
