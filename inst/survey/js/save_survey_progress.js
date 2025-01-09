function saveSurveyProgress(survey) {
   try {
       if (!survey || !survey.data) {
           throw new Error("Invalid survey instance or data");
       }

       // Get the actual values for storage
       const dataToStore = { ...survey.data };

       // Store parameters
       Object.entries(storedParams).forEach(([paramName, paramValue]) => {
           if (paramValue?.value !== undefined) {
               dataToStore[paramName] = paramValue.value;
           }
       });

       // Handle dynamic child choices
       if (survey.parentConfig) {
           // Add parent config metadata to storage
           dataToStore._dynamicConfig = {
               parentFields: {},
               childChoices: {}
           };

           Object.entries(survey.parentConfig).forEach(([parentField, config]) => {
               const parentValue = survey.data[parentField];
               if (parentValue) {
                   // Store parent field configuration
                   dataToStore._dynamicConfig.parentFields[parentField] = {
                       value: parentValue,
                       childField: null,
                       choices: []
                   };

                   // Find the corresponding child field
                   const parentIndex = config.choices.value.indexOf(parentValue);
                   if (parentIndex !== -1) {
                       const childField = config.choices.childField[parentIndex];
                       if (childField) {
                           dataToStore._dynamicConfig.parentFields[parentField].childField = childField;

                           // Get the child question and its current choices
                           const childQuestion = survey.getQuestionByName(childField);
                           if (childQuestion && childQuestion.choices) {
                               dataToStore._dynamicConfig.childChoices[childField] = {
                                   choices: childQuestion.choices,
                                   parentField: parentField,
                                   value: childQuestion.value
                               };
                           }
                       }
                   }
               }
           });
       }

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
