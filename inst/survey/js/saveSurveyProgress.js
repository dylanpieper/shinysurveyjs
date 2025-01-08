function saveSurveyProgress(survey) {
   try {
       if (!survey || !survey.data) {
           throw new Error("Invalid survey instance or data");
       }

       // Get the actual values for storage
       const dataToStore = { ...survey.data };
       Object.entries(storedParams).forEach(([paramName, paramValue]) => {
           if (paramValue?.value !== undefined) {
               dataToStore[paramName] = paramValue.value;
           }
       });

       setCookie(COOKIE_NAME, dataToStore, COOKIE_EXPIRATION_DAYS);
   } catch (error) {
       console.error("Error saving survey progress:", error);
       Shiny.setInputValue("surveyError", {
           type: "ProgressError",
           message: "Failed to save survey progress",
           details: error.message
       });
   }
}
